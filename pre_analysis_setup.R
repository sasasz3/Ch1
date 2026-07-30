library(data.table)
library(readxl)
library(lubridate)
library(stringr)
library(fixest)
library(ggplot2)
library(DescTools)

source("config.R")

#import data: trading day calendar, layoff announcements, buyback announcements
#earnings announcements and permco matching respectively
cal <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "cal.xlsx"),
    sheet = "calendar"
  )
)

layoffs <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "layoffs_data_final.xlsx"),
    sheet = "layoffs"
  )
)

sdc <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "repurchases_data_final.xlsx"),
    sheet = "sdc"
  )
)

earnings <- fread(
  file.path(DATA_FOLDER, "rdq.csv")
)

link_table <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "gvkey_permco.xlsx")
  )
)

#set correct names for earnings data
clean_and_rename <- function(DT) {
  current_names <- colnames(DT)
  
  #extract colnames from brackets
  new_names <- gsub(".*\\((.*)\\).*", "\\1", current_names)
  setnames(DT, current_names, new_names)
  
  # change rdq header to "date" for later
  if ("rdq" %in% colnames(DT)) {
    setnames(DT, "rdq", "date")
  }
}
clean_and_rename(earnings)

#set everything lowercase
lapply(list(cal, layoffs, sdc, earnings, link_table), function(x) {
  setnames(x, tolower(colnames(x)))
})


#fix gvkeys if leading zeros are lost during import and force them into str format
fix_gvkey <- function(DT) {
  if ("gvkey" %in% colnames(DT)) {
    DT[, gvkey := str_pad(as.character(gvkey), width = 6, side = "left", pad = "0")]
  }
}
lapply(list(cal, layoffs, sdc, earnings, link_table), fix_gvkey)

# standardise dates for all tables
tables_with_date <- list(cal, sdc, layoffs, earnings)
lapply(tables_with_date, function(x) x[, date := as.Date(date)])

# Restrict earnings announcements to the study period
earnings <- earnings[
  !is.na(date) &
    date >= as.Date("2002-01-01") &
    date <= as.Date("2025-12-31")
]

link_table[, `:=`(
  linkdt = as.Date(linkdt),
  linkenddt = as.Date(linkenddt)
)]
link_table[is.na(linkenddt), linkenddt := as.Date("2026-12-31")]



# CREATE TRADING DAY CALENDAR

cal <- cal[!is.na(date)]
cal <- unique(cal, by = "date")
setorder(cal, date)

# Create an integer trading-day index
cal[, t_index := .I]

# Use explicit names to avoid confusion in later joins
trading_calendar <- cal[, .(
  trading_date = date,
  t_index
)]

setkey(trading_calendar, trading_date)




# MAP EVENTS TO THE NEXT AVAILABLE TRADING DAY

map_to_trading_day <- function(DT, event_name) {
  
  events <- copy(
    DT[
      !is.na(gvkey) &
        gvkey != "" &
        !is.na(date)
    ]
  )
  
  # Preserve the original reported event date
  setnames(events, "date", "original_date")
  
  # Position of the next trading day:
  # findInterval gives the number of trading dates strictly before
  # or equal to the adjusted date.
  events[, calendar_position :=
           findInterval(
             original_date - 1,
             trading_calendar$trading_date
           ) + 1L
  ]
  
  # Events after the end of the calendar cannot be mapped
  events[
    calendar_position > nrow(trading_calendar),
    calendar_position := NA_integer_
  ]
  
  # Attach the matched trading date and index
  events[
    !is.na(calendar_position),
    `:=`(
      trading_date =
        trading_calendar$trading_date[calendar_position],
      
      t_index =
        trading_calendar$t_index[calendar_position]
    )
  ]
  
  # Ensure unmatched observations have correctly typed missing values
  events[
    is.na(calendar_position),
    `:=`(
      trading_date = as.Date(NA),
      t_index = NA_integer_
    )
  ]
  
  events[, event_type := event_name]
  
  events[
    ,
    shifted_to_trading_day :=
      !is.na(trading_date) &
      original_date != trading_date
  ]
  
  events[
    ,
    calendar_days_shifted :=
      as.integer(trading_date - original_date)
  ]
  
  events[, calendar_position := NULL]
  
  first_columns <- c(
    "gvkey",
    "event_type",
    "original_date",
    "trading_date",
    "t_index",
    "shifted_to_trading_day",
    "calendar_days_shifted"
  )
  
  setcolorder(
    events,
    c(
      first_columns,
      setdiff(names(events), first_columns)
    )
  )
  
  setorder(events, gvkey, original_date)
  
  events[]
}


layoff_events <- map_to_trading_day(
  layoffs,
  "Layoff"
)

buyback_events <- map_to_trading_day(
  sdc,
  "Buyback"
)

earnings_events <- map_to_trading_day(
  earnings,
  "Earnings"
)



