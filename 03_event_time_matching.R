
library(data.table)

#---------------------------------------------------------
# 1. Check that the required datasets exist
#---------------------------------------------------------

required_objects <- c(
  "layoff_events",
  "buyback_events"
)

missing_objects <- required_objects[
  !vapply(required_objects, exists, logical(1))
]

if (length(missing_objects) > 0) {
  stop(
    "The following objects are missing: ",
    paste(missing_objects, collapse = ", "),
    ". Run the cleaning script first or load the saved datasets."
  )
}

#---------------------------------------------------------
# 2. Prepare layoff events
#---------------------------------------------------------

layoffs_match <- copy(layoff_events)[
  !is.na(gvkey) &
    !is.na(t_index)
]

layoffs_match <- layoffs_match[
  ,
  .(
    gvkey,
    layoff_original_date = original_date,
    layoff_trading_date = trading_date,
    layoff_t_index = t_index
  )
]

# Unique identifier for every layoff observation
layoffs_match[
  ,
  layoff_id := .I
]

# Define the +/- 30 trading-day matching window
layoffs_match[
  ,
  `:=`(
    window_start = layoff_t_index - 30L,
    window_end   = layoff_t_index + 30L
  )
]


#---------------------------------------------------------
# 3. Prepare buyback events
#---------------------------------------------------------

buybacks_match <- copy(buyback_events)[
  !is.na(gvkey) &
    !is.na(t_index)
]

buybacks_match <- buybacks_match[
  ,
  .(
    gvkey,
    buyback_original_date = original_date,
    buyback_trading_date = trading_date,
    
    # Preserve the true buyback trading-day index
    buyback_t_index = t_index
  )
]

buybacks_match[
  ,
  buyback_id := .I
]

# Separate key used only for the non-equi join.
# This prevents data.table from replacing the original index
# with one of the layoff-window boundary values.
buybacks_match[
  ,
  buyback_join_index := buyback_t_index
]

#---------------------------------------------------------
# 4. Create every same-firm pair within +/- 30 trading days
#---------------------------------------------------------

layoff_buyback_pairs <- buybacks_match[
  layoffs_match,
  on = .(
    gvkey,
    buyback_join_index >= window_start,
    buyback_join_index <= window_end
  ),
  nomatch = 0L,
  allow.cartesian = TRUE,
  .(
    layoff_id = i.layoff_id,
    buyback_id = x.buyback_id,
    gvkey = i.gvkey,
    
    layoff_original_date = i.layoff_original_date,
    layoff_trading_date = i.layoff_trading_date,
    layoff_t_index = i.layoff_t_index,
    
    buyback_original_date = x.buyback_original_date,
    buyback_trading_date = x.buyback_trading_date,
    
    # Explicitly retrieve the preserved index from the buyback table
    buyback_t_index = x.buyback_t_index
  )
]

layoff_buyback_pairs[
  ,
  `:=`(
    distance = buyback_t_index - layoff_t_index,
    absolute_distance = abs(buyback_t_index - layoff_t_index)
  )
]

setorder(
  layoff_buyback_pairs,
  layoff_id,
  distance,
  buyback_original_date,
  buyback_id
)


layoff_pair_summary <- layoff_buyback_pairs[
  ,
  {
    minimum_absolute_distance <- min(absolute_distance)
    
    nearest_signed_distances <- unique(
      distance[absolute_distance == minimum_absolute_distance]
    )
    
    .(
      buybacks_within_30 = .N,
      same_day = any(distance == 0L),
      buybacks_before_30 = sum(distance < 0L),
      buybacks_after_30 = sum(distance > 0L),
      
      nearest_pre_distance = if (any(distance < 0L)) {
        max(distance[distance < 0L])
      } else {
        NA_integer_
      },
      
      nearest_post_distance = if (any(distance > 0L)) {
        min(distance[distance > 0L])
      } else {
        NA_integer_
      },
      
      nearest_absolute_distance = minimum_absolute_distance,
      
      nearest_distance = if (
        length(nearest_signed_distances) == 1L
      ) {
        nearest_signed_distances
      } else {
        NA_integer_
      },
      
      nearest_tie = length(nearest_signed_distances) > 1L,
      
      within_1_day = any(absolute_distance <= 1L),
      within_2_days = any(absolute_distance <= 2L),
      within_5_days = any(absolute_distance <= 5L),
      within_10_days = any(absolute_distance <= 10L),
      within_30_days = TRUE
    )
  },
  by = layoff_id
]

#=========================================================
# CREATE COMPLETE LAYOFF-LEVEL EVENT-TIME DATASET
#=========================================================

layoff_event_time <- merge(
  layoffs_match[
    ,
    .(
      layoff_id,
      gvkey,
      layoff_original_date,
      layoff_trading_date,
      layoff_t_index
    )
  ],
  layoff_pair_summary,
  by = "layoff_id",
  all.x = TRUE,
  sort = FALSE
)

setorder(
  layoff_event_time,
  gvkey,
  layoff_t_index,
  layoff_id
)


count_columns <- c(
  "buybacks_within_30",
  "buybacks_before_30",
  "buybacks_after_30"
)

for (column in count_columns) {
  set(
    layoff_event_time,
    i = which(is.na(layoff_event_time[[column]])),
    j = column,
    value = 0L
  )
}



indicator_columns <- c(
  "same_day",
  "nearest_tie",
  "within_1_day",
  "within_2_days",
  "within_5_days",
  "within_10_days",
  "within_30_days"
)

layoff_event_time[
  ,
  (indicator_columns) := lapply(
    .SD,
    function(x) fifelse(is.na(x), FALSE, x)
  ),
  .SDcols = indicator_columns
]

window_summary <- data.table(
  Window = c(
    "Same trading day",
    "+/- 1 trading day",
    "+/- 2 trading days",
    "+/- 5 trading days",
    "+/- 10 trading days",
    "+/- 30 trading days"
  ),
  
  Layoffs_with_Buyback = c(
    sum(layoff_event_time$same_day),
    sum(layoff_event_time$within_1_day),
    sum(layoff_event_time$within_2_days),
    sum(layoff_event_time$within_5_days),
    sum(layoff_event_time$within_10_days),
    sum(layoff_event_time$within_30_days)
  )
)

window_summary[
  ,
  Percentage_of_Layoffs :=
    round(
      100 * Layoffs_with_Buyback / nrow(layoff_event_time),
      2
    )
]

print(window_summary)



event_time_distribution <- layoff_buyback_pairs[
  ,
  .(
    Buyback_Pairs = .N,
    Layoffs = uniqueN(layoff_id)
  ),
  by = distance
]

event_time_distribution <- merge(
  data.table(distance = -30:30),
  event_time_distribution,
  by = "distance",
  all.x = TRUE
)

event_time_distribution[
  is.na(Buyback_Pairs),
  Buyback_Pairs := 0
]

event_time_distribution[
  is.na(Layoffs),
  Layoffs := 0
]

print(event_time_distribution)

event_time_distribution[
  distance >= -10 &
    distance <= 10
]