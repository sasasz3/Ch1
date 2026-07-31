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
  trading_date = trading_calendar$trading_date
)

panel <- merge(
  panel,
  trading_calendar,
  by = "trading_date",
  all.x = TRUE
)



#add buybacks to panel

buyback_days <- unique(
  buyback_events[
    ,
    .(
      gvkey,
      trading_date
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
  by = c("gvkey", "trading_date"),
  all.x = TRUE
)

panel[
  is.na(buyback_today),
  buyback_today := FALSE
]



#add layoffs to panel

layoff_days <- unique(
  layoff_events[
    !is.na(trading_date),
    .(
      gvkey,
      trading_date
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
  by = c("gvkey", "trading_date"),
  all.x = TRUE
)

panel[
  is.na(layoff_today),
  layoff_today := FALSE
]

layoff_days <- merge(
  layoff_days,
  trading_calendar[
    ,
    .(trading_date, t_index)
  ],
  by = "trading_date",
  all.x = TRUE
)


#add earnings to panel 

earnings_days <- unique(
  earnings_events[
    !is.na(trading_date),
    .(
      gvkey,
      trading_date
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
  by = c("gvkey", "trading_date"),
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
  year := year(trading_date)
]



panel[
  ,
  .(
    observations = .N,
    firms = uniqueN(gvkey),
    first_date = min(trading_date),
    last_date = max(trading_date),
    buyback_days = sum(buyback_today),
    layoff_days = sum(layoff_today),
    earnings_days = sum(earnings_today)
  )
]

saveRDS(
  panel,
  file = file.path(DATA_FOLDER, "firm_day_event_panel.rds")
)