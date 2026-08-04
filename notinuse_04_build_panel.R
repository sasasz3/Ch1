library(data.table)
library(readxl)
library(lubridate)
library(fixest)

source("config.R")  


# CREATE PANEL DATA ALL UNIQUE FIRMS X TRADING DAYS (4487 X 6040)

analysis_firms <- unique(
  c(
    layoffs$gvkey,
    sdc$gvkey
  )
)

analysis_firms <- data.table(gvkey = analysis_firms)

nrow(analysis_firms)

panel <- CJ(
  gvkey = analysis_firms$gvkey,
  calendar_date = calendar$calendar_date
)

panel <- merge(
  panel,
  calendar,
  by = "calendar_date",
  all.x = TRUE
)


#add buybacks to panel

buyback_days <- unique(
  buyback_events[
    ,
    .(
      gvkey,
      calendar_date
    )
  ]
)

buyback_days[
  ,
  buyback_today := TRUE
]

panel <- merge(
  panel,
  buyback_days,
  by = c("gvkey", "calendar_date"),
  all.x = TRUE
)

panel[
  is.na(buyback_today),
  buyback_today := FALSE
]



#add layoffs to panel

layoff_days <- unique(
  layoff_events[
    !is.na(calendar_date),
    .(
      gvkey,
      calendar_date
    )
  ]
)

layoff_days[
  ,
  layoff_today := TRUE
]

panel <- merge(
  panel,
  layoff_days,
  by = c("gvkey", "calendar_date"),
  all.x = TRUE
)

panel[
  is.na(layoff_today),
  layoff_today := FALSE
]




layoff_days <- merge(
  layoff_days,
  calendar[
    ,
    .(calendar_date, t_index)
  ],
  by = "calendar_date",
  all.x = TRUE
)


#add earnings to panel 

earnings_days <- unique(
  earnings_events[
    !is.na(calendar_date),
    .(
      gvkey,
      calendar_date
    )
  ]
)

earnings_days[
  ,
  earnings_today := TRUE
]

panel <- merge(
  panel,
  earnings_days,
  by = c("gvkey", "calendar_date"),
  all.x = TRUE
)

panel[
  is.na(earnings_today),
  earnings_today := FALSE
]




# CREATE WINDOWS AROUND LAYOFFS IN THE PANEL 


create_window <- function(layoff_days, window) {
  
  window_dt <- rbindlist(
    lapply(-window:window, function(i) {
      
      layoff_days[
        ,
        .(
          gvkey,
          t_index = t_index + i
        )
      ]
      
    })
  )
  
  unique(window_dt)
}

window1  <- create_window(layoff_days, 1)
window2  <- create_window(layoff_days, 2)
window5  <- create_window(layoff_days, 5)
window10 <- create_window(layoff_days, 10)
window30 <- create_window(layoff_days, 30)

window1[, within_1 := TRUE]
window2[, within_2 := TRUE]
window5[, within_5 := TRUE]
window10[, within_10 := TRUE]
window30[, within_30 := TRUE]


panel <- merge(
  panel,
  window1,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel[
  is.na(within_1),
  within_1 := FALSE
]

panel <- merge(
  panel,
  window2,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel[
  is.na(within_2),
  within_2 := FALSE
]

panel <- merge(
  panel,
  window5,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel[
  is.na(within_5),
  within_5 := FALSE
]

panel <- merge(
  panel,
  window10,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel[
  is.na(within_10),
  within_10 := FALSE
]

panel <- merge(
  panel,
  window30,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel[
  is.na(within_30),
  within_30 := FALSE
]




#CREATE DIRECTIONAL WINDOWS AROUND LAYOFFS, PREEMPTIVE AND REACTIVE 

create_directional_window <- function(layoff_days, offsets) {
  
  window_dt <- rbindlist(
    lapply(offsets, function(i) {
      layoff_days[
        ,
        .(
          gvkey,
          t_index = t_index + i
        )
      ]
    })
  )
  
  unique(window_dt)
}

pre_1_5   <- create_directional_window(layoff_days, -5:-1)
post_1_5  <- create_directional_window(layoff_days, 1:5)

pre_1_10  <- create_directional_window(layoff_days, -10:-1)
post_1_10 <- create_directional_window(layoff_days, 1:10)

pre_1_5[, pre_1_5 := TRUE]
post_1_5[, post_1_5 := TRUE]

pre_1_10[, pre_1_10 := TRUE]
post_1_10[, post_1_10 := TRUE]



panel <- merge(
  panel,
  pre_1_5,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel <- merge(
  panel,
  post_1_5,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel <- merge(
  panel,
  pre_1_10,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel <- merge(
  panel,
  post_1_10,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)



directional_columns <- c(
  "pre_1_5",
  "post_1_5",
  "pre_1_10",
  "post_1_10"
)

panel[
  ,
  (directional_columns) := lapply(
    .SD,
    function(x) fifelse(is.na(x), FALSE, x)
  ),
  .SDcols = directional_columns
]





# CREATE WINDOWS USED FOR REGRESSION 

panel[
  ,
  preemptive_window := pre_1_5
]

panel[
  ,
  bundle_day := layoff_today
]

panel[
  ,
  reactive_window := post_1_5
]

sapply(
  panel[
    ,
    .(
      preemptive_window,
      bundle_day,
      reactive_window
    )
  ],
  sum
)

panel[
  preemptive_window +
    bundle_day +
    reactive_window > 1,
  .N
]

panel[
  ,
  year := year(calendar_date)
]







panel[
  ,
  .(
    observations = .N,
    firms = uniqueN(gvkey),
    first_date = min(calendar_date),
    last_date = max(calendar_date),
    buyback_days = sum(buyback_today),
    layoff_days = sum(layoff_today),
    earnings_days = sum(earnings_today)
  )
]

baseline_pre_long_dt <- create_directional_window(
  layoff_days,
  -30:-10
)

baseline_preemptive_dt <- create_directional_window(
  layoff_days,
  -9:-4
)

baseline_bundle_dt <- create_directional_window(
  layoff_days,
  -3:3
)

baseline_reactive_dt <- create_directional_window(
  layoff_days,
  4:9
)

baseline_post_long_dt <- create_directional_window(
  layoff_days,
  10:30
)

baseline_pre_long_dt[
  ,
  baseline_pre_long := TRUE
]

baseline_preemptive_dt[
  ,
  baseline_preemptive := TRUE
]

baseline_bundle_dt[
  ,
  baseline_bundle := TRUE
]

baseline_reactive_dt[
  ,
  baseline_reactive := TRUE
]

baseline_post_long_dt[
  ,
  baseline_post_long := TRUE
]




panel <- merge(
  panel,
  baseline_pre_long_dt,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel <- merge(
  panel,
  baseline_preemptive_dt,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel <- merge(
  panel,
  baseline_bundle_dt,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel <- merge(
  panel,
  baseline_reactive_dt,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

panel <- merge(
  panel,
  baseline_post_long_dt,
  by = c("gvkey", "t_index"),
  all.x = TRUE
)

baseline_columns <- c(
  "baseline_pre_long",
  "baseline_preemptive",
  "baseline_bundle",
  "baseline_reactive",
  "baseline_post_long"
)

panel[
  ,
  (baseline_columns) := lapply(
    .SD,
    function(x) fifelse(is.na(x), FALSE, x)
  ),
  .SDcols = baseline_columns
]


# NEW APPROACH

# ============================================================
# EARNINGS-DAY DESCRIPTIVE STATISTICS
# ============================================================

earnings_sample <- panel[
  earnings_today == TRUE
]

earnings_window_rates <- data.table(
  Category = c(
    "All earnings days",
    "Outside +/-30 layoff window",
    "Pre-long: -30 to -10",
    "Pre-emptive: -9 to -4",
    "Bundle: -3 to +3",
    "Reactive: +4 to +9",
    "Post-long: +10 to +30",
    "Exact layoff day"
  ),
  
  Earnings_Days = c(
    nrow(earnings_sample),
    
    earnings_sample[
      baseline_pre_long == FALSE &
        baseline_preemptive == FALSE &
        baseline_bundle == FALSE &
        baseline_reactive == FALSE &
        baseline_post_long == FALSE,
      .N
    ],
    
    earnings_sample[baseline_pre_long == TRUE, .N],
    earnings_sample[baseline_preemptive == TRUE, .N],
    earnings_sample[baseline_bundle == TRUE, .N],
    earnings_sample[baseline_reactive == TRUE, .N],
    earnings_sample[baseline_post_long == TRUE, .N],
    earnings_sample[layoff_today == TRUE, .N]
  ),
  
  Buyback_Days = c(
    earnings_sample[buyback_today == TRUE, .N],
    
    earnings_sample[
      baseline_pre_long == FALSE &
        baseline_preemptive == FALSE &
        baseline_bundle == FALSE &
        baseline_reactive == FALSE &
        baseline_post_long == FALSE &
        buyback_today == TRUE,
      .N
    ],
    
    earnings_sample[
      baseline_pre_long == TRUE &
        buyback_today == TRUE,
      .N
    ],
    
    earnings_sample[
      baseline_preemptive == TRUE &
        buyback_today == TRUE,
      .N
    ],
    
    earnings_sample[
      baseline_bundle == TRUE &
        buyback_today == TRUE,
      .N
    ],
    
    earnings_sample[
      baseline_reactive == TRUE &
        buyback_today == TRUE,
      .N
    ],
    
    earnings_sample[
      baseline_post_long == TRUE &
        buyback_today == TRUE,
      .N
    ],
    
    earnings_sample[
      layoff_today == TRUE &
        buyback_today == TRUE,
      .N
    ]
  )
)

earnings_window_rates[
  ,
  Buyback_Rate :=
    Buyback_Days / Earnings_Days
]

earnings_window_rates[
  ,
  Buyback_Rate_Percent :=
    round(100 * Buyback_Rate, 3)
]

print(earnings_window_rates)


# ============================================================
# PREPARE EVENTS FOR +/-250 TRADING-DAY STACKED PLOT
# ============================================================

# One layoff event per firm-trading-day
layoffs_plot <- unique(
  layoff_events[
    !is.na(gvkey) &
      !is.na(trading_date) &
      !is.na(t_index),
    .(
      gvkey,
      layoff_original_date = original_date,
      layoff_trading_date = trading_date,
      layoff_t_index = t_index
    )
  ],
  by = c("gvkey", "layoff_trading_date")
)

layoffs_plot[, layoff_id := .I]

# One buyback indicator per firm-trading-day
buybacks_plot <- unique(
  buyback_events[
    !is.na(gvkey) &
      !is.na(trading_date),
    .(
      gvkey,
      trading_date
    )
  ],
  by = c("gvkey", "trading_date")
)

buybacks_plot[, buyback_today := 1L]


# ============================================================
# CREATE STACKED LAYOFF-EVENT PANEL
# ============================================================

plot_window <- 250L

event_grid <- CJ(
  layoff_id = layoffs_plot$layoff_id,
  relative_day = -plot_window:plot_window
)

event_grid <- merge(
  event_grid,
  layoffs_plot[
    ,
    .(
      layoff_id,
      gvkey,
      layoff_original_date,
      layoff_trading_date,
      layoff_t_index
    )
  ],
  by = "layoff_id",
  all.x = TRUE
)

# Identify the trading-day index corresponding to each
# relative day around each layoff
event_grid[
  ,
  target_t_index := layoff_t_index + relative_day
]

event_grid <- merge(
  event_grid,
  trading_calendar[
    ,
    .(
      target_t_index = t_index,
      event_trading_date = trading_date
    )
  ],
  by = "target_t_index",
  all.x = TRUE
)

complete_layoff_ids <- event_grid[
  ,
  .(
    complete_window = all(!is.na(event_trading_date))
  ),
  by = layoff_id
][
  complete_window == TRUE,
  layoff_id
]

event_grid_complete <- event_grid[
  layoff_id %in% complete_layoff_ids
]

event_grid_complete[, buyback_today := 0L]

event_grid_complete[
  buybacks_plot,
  on = .(
    gvkey,
    event_trading_date = trading_date
  ),
  buyback_today := i.buyback_today
]


