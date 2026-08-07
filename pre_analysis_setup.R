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
  !is.na(datadate) &
    datadate >= as.Date("2002-01-01") &
    datadate <= as.Date("2025-12-31")
]

link_table[, `:=`(
  linkdt = as.Date(linkdt),
  linkenddt = as.Date(linkenddt)
)]
link_table[is.na(linkenddt), linkenddt := as.Date("2026-12-31")]




# ============================================================
# CREATE FULL CALENDAR-DAY CALENDAR
# ============================================================

calendar_start <- as.Date("2002-01-01")
calendar_end   <- as.Date("2025-12-31")

calendar <- data.table(
  calendar_date = seq.Date(
    from = calendar_start,
    to = calendar_end,
    by = "day"
  )
)

# Sequential calendar-day index
calendar[
  ,
   t_index := .I
]

# Optional date information
calendar[
  ,
  `:=`(
    calendar_year = year(calendar_date),
    calendar_month = month(calendar_date),
    calendar_day = day(calendar_date),
    weekday = weekdays(calendar_date),
    is_weekend = wday(calendar_date) %in% c(1L, 7L)
  )
]

setkey(calendar, calendar_date)



# ============================================================
# MAP EVENTS TO THEIR ACTUAL CALENDAR DATES
# ============================================================

map_to_calendar <- function(DT, event_name) {
  
  events <- copy(
    DT[
      !is.na(gvkey) &
        gvkey != "" &
        !is.na(date)
    ]
  )
  
  # Preserve the reported date
  setnames(
    events,
    "date",
    "calendar_date"
  )
  
  events[
    ,
    calendar_date := as.Date(calendar_date)
  ]
  
  events[
    ,
    event_type := event_name
  ]
  
  # Attach the corresponding calendar-day index
  events <- merge(
    events,
    calendar[
      ,
      .(
        calendar_date = calendar_date,
        t_index,
        calendar_year,
        calendar_month,
        calendar_day,
        weekday,
        is_weekend
      )
    ],
    by = "calendar_date",
    all.x = TRUE,
    sort = FALSE
  )
  
  first_columns <- c(
    "gvkey",
    "event_type",
    "calendar_date",
    "t_index",
    "calendar_year",
    "calendar_month",
    "calendar_day",
    "weekday",
    "is_weekend"
  )
  
  setcolorder(
    events,
    c(
      first_columns,
      setdiff(names(events), first_columns)
    )
  )
  
  setorder(
    events,
    gvkey,
    calendar_date
  )
  
  events[]
}


layoff_events <- map_to_calendar(
  layoffs,
  "Layoff"
)

buyback_events <- map_to_calendar(
  sdc,
  "Buyback"
)

earnings_events <- map_to_calendar(
  earnings,
  "Earnings"
)


