library(data.table)
library(readxl)
library(lubridate)
library(stringr)
library(fixest)
library(DescTools)

source("config.R")
source("reusable_functions.R")

repurchases <- readRDS(
  file.path(DATA_FOLDER, "repurchases.rds")
)



#import data: capiq, warn, sdc and fiscal year calendar (later controls)
warn <- setDT(read_excel(file.path(DATA_FOLDER, "layoffs_warn.xlsx")))
warn[, gvkey := as.character(gvkey)]
warn[, date := as.Date(date)]

capiq <- setDT(read_excel(file.path(DATA_FOLDER, "layoffs_capiq.xlsx")))
capiq[, gvkey := as.character(gvkey)]
capiq[, date := as.Date(date)]


#OBSERVE SAMPLE OVERLAPS - FIRM-LEVEL
#----------------------------------------------------------------
# Valid repurchase observations
repurchases_sample <- repurchases[
  !is.na(actual_repurchases)
]

# Unique firms in the sample
repurchases_firms <- unique(
  repurchases_sample$gvkey
)

get_firms(warn)
get_firms(capiq)

#calculate intersections between datasets
repo_warn_intersect <- intersect(repurchases_firms, warn_firms)
capiq_warn_intersect <- intersect(capiq_firms, warn_firms)
repo_capiq_intersect <- intersect(repurchases_firms, capiq_firms)
all_layoff_firms <- union( warn_firms, capiq_firms)
layoff_repo_intersect <- intersect(all_layoff_firms, repurchases_firms)


#FISCAL YEAR ASSIGNMENT FOR LAYOFF ANNOUNCEMENTS
#----------------------------------------------------------------

setDT(repurchases)

#sort the data by gvkey and datadate to ensure proper lagging
setorder(repurchases, gvkey, datadate)

#set previous datadate as the lagged value of datadate for each gvkey
repurchases[, previous_datadate := shift(datadate),
            by = gvkey]

#calculate fiscal year length as the difference between current and previous datadate
repurchases[, fiscal_year_length :=
              as.integer(datadate - previous_datadate),
            by = gvkey]

#identify previous fiscal year and previous year-end date
repurchases[, previous_fyear := shift(fyear), by = gvkey]

repurchases[, previous_datadate := shift(datadate), by = gvkey]

#calculate fiscal year boundaries
repurchases[, fiscal_year_end := datadate]

#default condition if primary conditions are not met 
repurchases[, fiscal_year_start := datadate - 365]

# primary conditions
repurchases[
  #if previous fiscal year is current fiscal year minus 1
  (!is.na(previous_fyear) & previous_fyear == fyear - 1) |
    #or if fiscal year length is less than 732 days (2 years)
    (!is.na(fiscal_year_length) & fiscal_year_length < 732),
  #then fiscal year start is previous datadate + 1
  fiscal_year_start := previous_datadate + 1
]




#ASSIGN WARN AND CAPIQ LAYOFF ANNOUNCEMENTS TO FISCAL YEARS IN REPURCHASES DATA
#----------------------------------------------------------------

#ensure consistency
repurchases[, gvkey := as.character(gvkey)]
warn[, gvkey := as.character(gvkey)]
capiq[, gvkey := as.character(gvkey)]

repurchases[, `:=`(
  fiscal_year_start = as.Date(fiscal_year_start),
  fiscal_year_end = as.Date(fiscal_year_end)
)]

warn[, date := as.Date(date)]
capiq[, date := as.Date(date)]

#combine layoff announcement dates
layoff_dates <- rbindlist(
  list(
    warn[, .(gvkey, date)],
    capiq[, .(gvkey, date)]
  ),
  use.names = TRUE
)

#remove missing identifiers and dates
layoff_dates <- unique(
  layoff_dates[
    !is.na(gvkey) & !is.na(date)
  ]
)

#initialise layoff variable
repurchases[, layoff := NA_integer_]

#only consider observations with non-missing PRSTKC
valid_sample <- repurchases[!is.na(prstkc)]

#match layoff announcements to fiscal-year boundaries
layoff_matches <- layoff_dates[
  valid_sample,
  on = .(
    gvkey,
    date >= fiscal_year_start,
    date <= fiscal_year_end
  ),
  nomatch = 0L,
  allow.cartesian = TRUE
]

#identify firm-year observations with at least one announcement
layoff_matches <- unique(
  layoff_matches[, .(gvkey, fyear)]
)

#assign layoff = 0 to all valid observations initially
repurchases[!is.na(prstkc), layoff := 0L]

#assign layoff = 1 where an announcement was identified
repurchases[
  layoff_matches,
  on = .(gvkey, fyear),
  layoff := 1L
]