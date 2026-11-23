library(data.table)
library(readxl)
library(lubridate)
library(stringr)
library(fixest)
library(DescTools)

source("config.R")
source("reusable_functions.R")

analysis_sample <- readRDS(
  file.path(DATA_FOLDER, "sample_with_repurchases.rds")
)

sdc <- setDT(read_excel(file.path(DATA_FOLDER, "sdc_data_final.xlsx"),sheet = "sdc"))
sdc[, gvkey := as.character(gvkey)]
sdc[, date := as.Date(date)]

#import data: capiq, warn, sdc and fiscal year calendar (later controls)
warn <- setDT(read_excel(file.path(DATA_FOLDER, "layoffs_warn.xlsx")))
warn[, gvkey := as.character(gvkey)]
warn[, date := as.Date(date)]

capiq <- setDT(read_excel(file.path(DATA_FOLDER, "layoffs_capiq.xlsx")))
capiq[, gvkey := as.character(gvkey)]
capiq[, date := as.Date(date)]


#OBSERVE SAMPLE OVERLAPS - FIRM-LEVEL
#----------------------------------------------------------------

# Unique firms in the sample
repurchases_firms <- unique(
  analysis_sample$gvkey
)

get_firms(warn)
get_firms(capiq)

#calculate intersections between datasets
repo_warn_intersect <- intersect(repurchases_firms, warn_firms)
capiq_warn_intersect <- intersect(capiq_firms, warn_firms)
repo_capiq_intersect <- intersect(repurchases_firms, capiq_firms)
all_layoff_firms <- union( warn_firms, capiq_firms)
layoff_repo_intersect <- intersect(all_layoff_firms, repurchases_firms)


#FISCAL YEAR ASSIGNMENT FOR EVENT LEVEL LAYOFF ANNOUNCEMENTS
#----------------------------------------------------------------

#set data tables for existing object in R 
setDT(analysis_sample)

#sort the data by gvkey and end-date of fiscal year (datadate) to ensure proper lagging
setorder(analysis_sample, gvkey, datadate)

#set previous fiscal year end date as the lagged value of current one for each gvkey
analysis_sample[, previous_datadate := shift(datadate),
            by = gvkey]

#set previous fiscal year as the lagged value of current one for each gvkey
analysis_sample[, previous_fyear := shift(fyear), by = gvkey]


#calculate fiscal year length as the difference between current and previous fiscal year end dates
analysis_sample[, fiscal_year_length :=
              as.integer(datadate - previous_datadate),
            by = gvkey]



#calculate fiscal year boundaries
#end of current fiscal year is alway given by datadate
analysis_sample[, fiscal_year_end := datadate]

#fallback/default condition if primary conditions are not met 
analysis_sample[, fiscal_year_start := datadate - 365]

# primary conditions
analysis_sample[
  #if previous fiscal year is current fiscal year minus 1
  (!is.na(previous_fyear) & previous_fyear == fyear - 1) |
    #or if fiscal year length is less than 732 days (2 years)
    (!is.na(fiscal_year_length) & fiscal_year_length < 732),
  #then fiscal year start is the previous fiscal year end date  + 1 day
  fiscal_year_start := previous_datadate + 1
]




#ASSIGN WARN AND CAPIQ LAYOFF ANNOUNCEMENTS TO FISCAL YEARS IN REPURCHASES DATA
#----------------------------------------------------------------


#combine layoff announcement dates from capiq and warn 
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

#initialise layoff variable; assign NA to all observations by default
analysis_sample[, layoff := NA_integer_]



#match layoff announcements to fiscal-year boundaries
layoff_matches <- layoff_dates[
  analysis_sample,
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
analysis_sample[!is.na(prstkc), layoff := 0L]

#assign layoff = 1 where an announcement was identified
analysis_sample[
  layoff_matches,
  on = .(gvkey, fyear),
  layoff := 1L
]




#ASSIGN SDC ANNOUNCEMENTS TO FISCAL YEARS IN REPURCHASES DATA
#----------------------------------------------------------------

# Extract SDC announcement dates
repurchase_dates <- sdc[, .(gvkey, date)]

# Remove missing identifiers and dates
repurchase_dates <- unique(
  repurchase_dates[
    !is.na(gvkey) & !is.na(date)
  ]
)

# Initialise repurchase announcement variable
analysis_sample[, repurchase_announcement := NA_integer_]

# Match repurchase announcements to fiscal-year boundaries
repurchase_matches <- repurchase_dates[
  analysis_sample,
  on = .(
    gvkey,
    date >= fiscal_year_start,
    date <= fiscal_year_end
  ),
  nomatch = 0L,
  allow.cartesian = TRUE
]

# Identify firm-year observations with at least one announcement
repurchase_matches <- unique(
  repurchase_matches[, .(gvkey, fyear)]
)

# Assign zero to eligible observations initially
analysis_sample[
  !is.na(prstkc),
  repurchase_announcement := 0L
]

# Assign one where an announcement was identified
analysis_sample[
  repurchase_matches,
  on = .(gvkey, fyear),
  repurchase_announcement := 1L
]



#DESCRIPTIVE STATISTICS FOR LAYOFF ANNOUNCEMENTS
#----------------------------------------------------------------

# Number and percentage of observations
layoff_stats <- analysis_sample[
  !is.na(layoff),
  .(
    observations = .N,
    layoff_1 = sum(layoff == 1),
    layoff_0 = sum(layoff == 0),
    pct_layoff = mean(layoff == 1) * 100,
    pct_no_layoff = mean(layoff == 0) * 100
  )
]

layoff_stats

#firm statistics
firm_layoff_stats <-analysis_sample[
  !is.na(layoff),
  .(
    firm_years = .N,
    layoff_years = sum(layoff == 1),
    no_layoff_years = sum(layoff == 0),
    ever_layoff = as.integer(any(layoff == 1))
  ),
  by = gvkey
]

# Overall firm statistics
firm_layoff_stats[
  ,
  .(
    unique_firms = .N,
    firms_with_layoffs = sum(ever_layoff == 1),
    firms_without_layoffs = sum(ever_layoff == 0),
    pct_firms_with_layoffs = mean(ever_layoff == 1) * 100
  )
]


#annual distribution of layoffs
annual_layoffs <- analysis_sample[
  !is.na(layoff),
  .(
    firm_year_observations = .N,
    layoff_observations = sum(layoff == 1),
    no_layoff_observations = sum(layoff == 0),
    pct_layoff = mean(layoff == 1) * 100,
    unique_firms = uniqueN(gvkey)
  ),
  by = fyear
][order(fyear)]

annual_layoffs

#distribution of layoffs per firm year
firm_layoff_stats[
  ,
  .N,
  by = .(layoff_years)
][order(layoff_years)]


#DESCRIPTIVE STATISTICS FOR REPURCHASE ANNOUNCEMENTS
#----------------------------------------------------------------

# Number and percentage of observations
repurchase_stats <- analysis_sample[
  !is.na(repurchase_announcement),
  .(
    observations = .N,
    repurchase_1 = sum(repurchase_announcement == 1),
    repurchase_0 = sum(repurchase_announcement == 0),
    pct_repurchase = mean(repurchase_announcement == 1) * 100,
    pct_no_repurchase = mean(repurchase_announcement == 0) * 100
  )
]

repurchase_stats


# Firm statistics
firm_repurchase_stats <- analysis_sample[
  !is.na(repurchase_announcement),
  .(
    firm_years = .N,
    repurchase_years = sum(repurchase_announcement == 1),
    no_repurchase_years = sum(repurchase_announcement == 0),
    ever_repurchase = as.integer(any(repurchase_announcement == 1))
  ),
  by = gvkey
]

# Overall firm statistics
firm_repurchase_stats[
  ,
  .(
    unique_firms = .N,
    firms_with_repurchases = sum(ever_repurchase == 1),
    firms_without_repurchases = sum(ever_repurchase == 0),
    pct_firms_with_repurchases = mean(ever_repurchase == 1) * 100
  )
]


# Annual distribution of repurchase announcements
annual_repurchases <- analysis_sample[
  !is.na(repurchase_announcement),
  .(
    firm_year_observations = .N,
    repurchase_observations = sum(repurchase_announcement == 1),
    no_repurchase_observations = sum(repurchase_announcement == 0),
    pct_repurchase = mean(repurchase_announcement == 1) * 100,
    unique_firms = uniqueN(gvkey)
  ),
  by = fyear
][order(fyear)]

annual_repurchases


# Distribution of repurchase years per firm
firm_repurchase_stats[
  ,
  .N,
  by = .(repurchase_years)
][order(repurchase_years)]






# Save final repurchase-layoff panel
saveRDS(
  analysis_sample,
  file.path(DATA_FOLDER, "panel.rds")
)