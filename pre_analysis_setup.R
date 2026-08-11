library(data.table)
library(readxl)
library(lubridate)
library(stringr)
library(fixest)
library(DescTools)

source("config.R")

#import data: capiq, warn, sdc and fiscal year calendar (later controls)
warn <- setDT(read_excel(file.path(DATA_FOLDER, "warn_data_final.xlsx")))
warn[, gvkey := as.character(gvkey)]

capiq <- setDT(read_excel(file.path(DATA_FOLDER, "capitaliq_data_final.xlsx")))
capiq[, gvkey := as.character(gvkey)]

sdc <- setDT(read_excel(file.path(DATA_FOLDER, "sdc_data_final.xlsx"),sheet = "sdc"))
sdc[, gvkey := as.character(gvkey)]


#export union of all unique gvkeys
warn_gvkeys  <- unique(warn[!is.na(gvkey), gvkey])
capiq_gvkeys <- unique(capiq[!is.na(gvkey), gvkey])
sdc_gvkeys <- unique(sdc[!is.na(gvkey), gvkey])


all_layoff_gvkeys <- union(warn_gvkeys, capiq_gvkeys)
all_gvkeys <- union(all_layoff_gvkeys, sdc_gvkeys)
scd_layoff_intersect <- intersect(sdc_gvkeys, all_layoff_gvkeys)

# create event objects w/ valid dates
warn_events <- unique(warn[,.(gvkey, date)])
capiq_events <- unique(capiq[,.(gvkey, date)])
sdc_events <- unique(sdc[,.(gvkey, date)])

warn_events[, date := as.Date(date)]
capiq_events[, date := as.Date(date)]
sdc_events[, date := as.Date(date)]





#MAPPING

fiscal_year_map <- setDT(read_excel(file.path(DATA_FOLDER, "fiscal_year_map.xlsx")))

#clean column headers
clean_and_rename <- function(DT) {
current_names <- colnames(DT)
new_names <- gsub(".*\\((.*)\\).*", "\\1", current_names)
setnames(DT, current_names, new_names)}

clean_and_rename(fiscal_year_map)


#fix gvkeys if compromised during loading
fix_gvkey <- function(DT) {
  if ("gvkey" %in% colnames(DT)) {
    DT[, gvkey := str_pad(as.character(gvkey), width = 6, side = "left", pad = "0")]
  }
}
fix_gvkey(fiscal_year_map)


#create fiscal year map 
#create start dates for each gvkey each fiscal year based on compustat logic
fiscal_year_map[
  fyr <= 5,
  calendar_start := make_date(
    year = fyear,
    month = fyr + 1L,
    day = 1
  )
]

fiscal_year_map[
  fyr >= 6 & fyr <= 11,
  calendar_start := make_date(
    year = fyear - 1L,
    month = fyr + 1L,
    day = 1
  )
]

fiscal_year_map[
  fyr == 12,
  calendar_start := make_date(
    year = fyear,
    month = 1,
    day = 1
  )
]


#create end dates for each fiscal year based on compustat logic
fiscal_year_map[, end_year := fifelse( fyr <= 5,fyear + 1L,fyear)]

#set temp date to calculate the month end date 
fiscal_year_map[,end_month_start := make_date( year = end_year,month = fyr,day = 1)]

# returns first day of next month then subtracts one day
fiscal_year_map[,calendar_end := ceiling_date(end_month_start,unit = "month") - days(1)]

fiscal_year_map[
  ,
  c("end_year", "end_month_start") := NULL
]



fiscal_year_map[, calendar_start := as.Date(calendar_start)]
fiscal_year_map[, calendar_end := as.Date(calendar_end)]


#function to attach fiscal year 
attach_fyear <- function(events, fiscal_map) {
  
  result <- fiscal_map[
    events,
    on = .(
      gvkey,
      calendar_start <= date,
      calendar_end >= date
    ),
    .(
      gvkey = i.gvkey,
      date = i.date,
      fyear = x.fyear
    )
  ]
  
  return(result)
}


#attach correct fiscal years to event objects
warn_events <- attach_fyear(warn_events, fiscal_year_map)
capiq_events <- attach_fyear(capiq_events, fiscal_year_map)
sdc_events <- attach_fyear(sdc_events, fiscal_year_map)


# remove everything that could not be matched 
warn_unmatched_events <- warn_events[is.na(fyear)]
capiq_unmatched_events <- capiq_events[is.na(fyear)]
sdc_unmatched_events <- sdc_events[is.na(fyear)]

warn_events <- warn_events[!is.na(fyear)]
capiq_events <- capiq_events[!is.na(fyear)]
sdc_events <- sdc_events[!is.na(fyear)]


#audit
warn_firms <- unique(warn_events$gvkey)
capiq_firms <- unique(capiq_events$gvkey)
buyback_firms <- unique(sdc_events$gvkey)





#create fimr-year observation objects
warn_firm_years <- unique( warn_events[, .(gvkey, fyear)])
capiq_firm_years <- unique( capiq_events[, .(gvkey, fyear)])
buyback_firm_years <- unique(sdc_events[, .(gvkey, fyear)]) 

#put WARN observations and CAPIQ observations together
layoff_firm_years <- unique( rbindlist( list( warn_firm_years, capiq_firm_years ),use.names = TRUE ))
layoff_firms <- unique(layoff_firm_years$gvkey)

#interection between layoff and buyback firm-year observations 
layoff_buyback_fy_intersect <- merge(layoff_firm_years,buyback_firm_years,by = c("gvkey", "fyear"))


# create list of all events
all_event_firm_years <- unique(rbindlist(list(layoff_firm_years, buyback_firm_years), use.names = TRUE))

#audit again
all_event_firms <- union(buyback_firms, layoff_firms)
buyback_layoff_firms <- intersect( buyback_firms, layoff_firms)
same_fy_overlap_firms <- unique( layoff_buyback_fy_intersect$gvkey)



buyback_fy <- unique(sdc_events[,.(gvkey, fyear)])
buyback_fy[, is_buyback := 1L]

layoff_fy <- unique(layoff_events[ ,.(gvkey, fyear)])
layoff_fy[, is_layoff := 1L]



#create panel
panel <- copy(fiscal_year_map)

panel <- merge(
  panel,
  buyback_fy,
  by = c("gvkey", "fyear"),
  all.x = TRUE
)

panel[
  is.na(is_buyback),
  is_buyback := 0L
]


panel <- merge(
  panel,
  layoff_fy,
  by = c("gvkey", "fyear"),
  all.x = TRUE
)

panel[
  is.na(is_layoff),
  is_layoff := 0L
]


#firm-year audit 
cat( "Buyback firm-years:", panel[is_buyback == 1, .N], "\n")

cat("Layoff firm-years:", panel[is_layoff == 1, .N], "\n")

cat("Both:",panel[ is_buyback == 1 & is_layoff == 1, .N], "\n")


# removing firm-years where the company was not listed 
cat("Panel rows before EXCHG filter:", nrow(panel), "\n")
cat("Unique firms before filter:", uniqueN(panel$gvkey), "\n")

panel <- panel[exchg != 0]

cat("Panel rows after EXCHG filter:", nrow(panel), "\n")
cat("Unique firms after filter:", uniqueN(panel$gvkey), "\n")




