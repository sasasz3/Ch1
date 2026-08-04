library(data.table)
library(readxl)
library(lubridate)
library(stringr)
library(fixest)
library(ggplot2)
library(DescTools)

# ============================================================
# CREATE SYNTHETIC FIRM-SPECIFIC FISCAL-YEAR CALENDAR
# ============================================================

# Firms included in the analysis
analysis_gvkeys <- sort(
  unique(
    c(
      layoff_events$gvkey,
      buyback_events$gvkey
    )
  )
)


# ------------------------------------------------------------
# 1. EXTRACT OBSERVED Q4 FISCAL-YEAR INFORMATION
# ------------------------------------------------------------

q4_raw <- earnings[
  !is.na(gvkey) &
    gvkey != "" &
    !is.na(datadate) &
    !is.na(fyearq) &
    !is.na(fyr) &
    fqtr == 4L,
  .(
    gvkey,
    fiscal_year = as.integer(fyearq),
    observed_fyr = as.integer(fyr),
    observed_q4_end = as.Date(datadate)
  )
]


# ------------------------------------------------------------
# 2. KEEP ONLY UNAMBIGUOUS GVKEY-FISCAL-YEAR OBSERVATIONS
# ------------------------------------------------------------

q4_check <- q4_raw[
  ,
  .(
    classifications = uniqueN(
      paste(
        observed_fyr,
        observed_q4_end,
        sep = "_"
      )
    )
  ),
  by = .(
    gvkey,
    fiscal_year
  )
]

q4_unambiguous <- q4_raw[
  q4_check[classifications == 1L],
  on = .(
    gvkey,
    fiscal_year
  ),
  nomatch = 0L
]

q4_unambiguous <- unique(
  q4_unambiguous,
  by = c(
    "gvkey",
    "fiscal_year"
  )
)


# ------------------------------------------------------------
# 3. CREATE BALANCED GVKEY × FISCAL-YEAR SKELETON
# ------------------------------------------------------------

fiscal_year_calendar <- CJ(
  gvkey = analysis_gvkeys,
  fiscal_year = 2002:2025
)

fiscal_year_calendar <- merge(
  fiscal_year_calendar,
  q4_unambiguous,
  by = c(
    "gvkey",
    "fiscal_year"
  ),
  all.x = TRUE,
  sort = FALSE
)

setorder(
  fiscal_year_calendar,
  gvkey,
  fiscal_year
)


# ------------------------------------------------------------
# 4. IDENTIFY THE PREVIOUS AND NEXT OBSERVED FYR
# ------------------------------------------------------------

fiscal_year_calendar[
  ,
  fyr_previous := nafill(
    observed_fyr,
    type = "locf"
  ),
  by = gvkey
]

fiscal_year_calendar[
  ,
  fyr_next := nafill(
    observed_fyr,
    type = "nocb"
  ),
  by = gvkey
]


# ------------------------------------------------------------
# 5. ASSIGN THE SYNTHETIC FISCAL YEAR-END MONTH
# ------------------------------------------------------------

fiscal_year_calendar[
  ,
  fyr := fifelse(
    !is.na(observed_fyr),
    
    # Use the observed value when available
    observed_fyr,
    
    fifelse(
      !is.na(fyr_previous) &
        !is.na(fyr_next) &
        fyr_previous == fyr_next,
      
      # Internal missing year with the same FYR on both sides
      fyr_previous,
      
      fifelse(
        !is.na(fyr_previous) &
          is.na(fyr_next),
        
        # Extrapolate after the final observed year
        fyr_previous,
        
        fifelse(
          is.na(fyr_previous) &
            !is.na(fyr_next),
          
          # Extrapolate before the first observed year
          fyr_next,
          
          # Conflicting fiscal regimes: leave unresolved
          NA_integer_
        )
      )
    )
  )
]


# Flag missing years located between different fiscal regimes
fiscal_year_calendar[
  ,
  transition_ambiguous :=
    is.na(observed_fyr) &
    !is.na(fyr_previous) &
    !is.na(fyr_next) &
    fyr_previous != fyr_next
]


# ------------------------------------------------------------
# 6. GENERATE SYNTHETIC FISCAL-YEAR START AND END DATES
# ------------------------------------------------------------

# Fiscal-year end:
# last calendar day of the firm's fiscal year-end month
fiscal_year_calendar[
  !is.na(fyr),
  fiscal_year_end :=
    ceiling_date(
      make_date(
        year = fiscal_year,
        month = fyr,
        day = 1L
      ),
      unit = "month"
    ) - days(1L)
]


# Fiscal-year start:
# first day of the month after the preceding fiscal year-end
fiscal_year_calendar[
  !is.na(fyr),
  fiscal_year_start :=
    make_date(
      year = fifelse(
        fyr == 12L,
        fiscal_year,
        fiscal_year - 1L
      ),
      month = fifelse(
        fyr == 12L,
        1L,
        fyr + 1L
      ),
      day = 1L
    )
]


# ------------------------------------------------------------
# 7. CLEAN AND ORDER
# ------------------------------------------------------------

setcolorder(
  fiscal_year_calendar,
  c(
    "gvkey",
    "fiscal_year",
    "fyr",
    "fiscal_year_start",
    "fiscal_year_end",
    "observed_fyr",
    "observed_q4_end",
    "transition_ambiguous",
    "fyr_previous",
    "fyr_next"
  )
)

setorder(
  fiscal_year_calendar,
  gvkey,
  fiscal_year
)




assign_to_fiscal_year <- function(events, fiscal_calendar) {
  
  events_to_map <- copy(
    events[
      !is.na(gvkey) &
        !is.na(calendar_date)
    ]
  )
  
  events_to_map[
    ,
    event_id := .I
  ]
  
  mapped <- fiscal_calendar[
    events_to_map,
    on = .(
      gvkey,
      fiscal_year_start <= calendar_date,
      fiscal_year_end >= calendar_date
    ),
    allow.cartesian = TRUE
  ]
  
  mapped[
    ,
    matches := sum(!is.na(fiscal_year)),
    by = event_id
  ]
  
  mapped[
    matches != 1,
    `:=`(
      fiscal_year = NA_integer_,
      fyr = NA_integer_
    )
  ]
  
  mapped <- mapped[
    ,
    .SD[1],
    by = event_id
  ]
  
  mapped[
    ,
    matches := NULL
  ]
  
  mapped[]
}




layoff_fiscal_events <- assign_to_fiscal_year(
  layoff_events,
  fiscal_year_calendar
)

buyback_fiscal_events <- assign_to_fiscal_year(
  buyback_events,
  fiscal_year_calendar
)



layoff_counts <- layoff_fiscal_events[
  !is.na(fiscal_year),
  .(
    layoff_count = .N
  ),
  by = .(
    gvkey,
    fiscal_year
  )
]

buyback_counts <- buyback_fiscal_events[
  !is.na(fiscal_year),
  .(
    buyback_count = .N
  ),
  by = .(
    gvkey,
    fiscal_year
  )
]




fiscal_year_calendar <- merge(
  fiscal_year_calendar,
  layoff_counts,
  by = c(
    "gvkey",
    "fiscal_year"
  ),
  all.x = TRUE
)

fiscal_year_calendar <- merge(
  fiscal_year_calendar,
  buyback_counts,
  by = c(
    "gvkey",
    "fiscal_year"
  ),
  all.x = TRUE
)



fiscal_year_calendar[
  is.na(layoff_count),
  layoff_count := 0L
]

fiscal_year_calendar[
  is.na(buyback_count),
  buyback_count := 0L
]




fiscal_year_calendar[
  ,
  layoff_year := layoff_count > 0L
]

fiscal_year_calendar[
  ,
  buyback_year := buyback_count > 0L
]


fiscal_year_calendar[
  ,
  .(
    Total_Layoffs = sum(layoff_count),
    Total_Buybacks = sum(buyback_count),
    Layoff_Years = sum(layoff_year),
    Buyback_Years = sum(buyback_year),
    Both = sum(layoff_year & buyback_year)
  )
]


model_fiscal_binary <- feols(
  buyback_year ~ layoff_year |
    gvkey + fiscal_year,
  data = fiscal_year_calendar,
  cluster = ~gvkey
)

summary(model_fiscal_binary)




gvkey_download <- data.table(
  gvkey = sort(unique(fiscal_year_calendar$gvkey))
)



writeLines(
  sort(unique(fiscal_year_calendar$gvkey)),
  file.path(DATA_FOLDER, "gvkeys_for_wrds.txt")
)