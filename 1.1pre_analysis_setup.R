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
warn[, date := as.Date(date)]

capiq <- setDT(read_excel(file.path(DATA_FOLDER, "capitaliq_data_final.xlsx")))
capiq[, gvkey := as.character(gvkey)]
capiq[, date := as.Date(date)]

sdc <- setDT(read_excel(file.path(DATA_FOLDER, "sdc_data_final.xlsx"),sheet = "sdc"))
sdc[, gvkey := as.character(gvkey)]
sdc[, date := as.Date(date)]


#unique gvkeys in each dataset
warn_gvkeys  <- unique(warn[!is.na(gvkey), gvkey])
capiq_gvkeys <- unique(capiq[!is.na(gvkey), gvkey])
sdc_gvkeys <- unique(sdc[!is.na(gvkey), gvkey])


all_layoff_gvkeys <- union(warn_gvkeys, capiq_gvkeys)
all_gvkeys <- union(all_layoff_gvkeys, sdc_gvkeys)
scd_layoff_intersect <- intersect(sdc_gvkeys, all_layoff_gvkeys)

#export union of all gvkeys
#to be used to generate the fiscal year map by downloading compustat fiscal years for the below given set
writeLines(sort(all_gvkeys), file.path(DATA_FOLDER, "all_gvkeys_for_compustat.txt"))

# create event objects w/ valid dates
warn_events <- unique(warn[,.(gvkey, date)])
capiq_events <- unique(capiq[,.(gvkey, date)])
sdc_events <- unique(sdc[,.(gvkey, date)])



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

fiscal_year_map[, datadate := as.Date(datadate)]

#create fiscal year map for general observations
setorder(fiscal_year_map, gvkey, datadate)
fiscal_year_map[ ,calendar_end := datadate]
fiscal_year_map[, calendar_start := shift(calendar_end) + days(1), by = gvkey]

# fill calendar_start for first observation of each firm
fiscal_year_map[ is.na(calendar_start),
  calendar_start := (datadate %m-% years(1)) + days(1)
]





#function to attach fiscal year to events
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





#CREATE PANEL
panel <- copy(fiscal_year_map)

# Keep one row per gvkey-fyear
panel <- unique( panel, by = c("gvkey", "fyear"))

#attach buyback events to panel
buyback_indicator <- copy(buyback_firm_years)

buyback_indicator[ , is_buyback := 1L]

panel <- merge(
  panel,
  buyback_indicator,
  by = c("gvkey", "fyear"),
  all.x = TRUE
)

# Firm-years without a buyback become zero
panel[ is.na(is_buyback), is_buyback := 0L]


#attach layoff events to panel
layoff_indicator <- copy(layoff_firm_years)

layoff_indicator[ ,is_layoff := 1L]

panel <- merge(
  panel,
  layoff_indicator,
  by = c("gvkey", "fyear"),
  all.x = TRUE
)


#remove the gvkeys left in the panel by the unmatched events
regression_panel_firms <- unique(panel$gvkey)
panel_without_events <- setdiff( regression_panel_firms, all_event_firms)
panel <- panel[!gvkey %in% panel_without_events]



# Firm-years without a layoff become zero
panel[ is.na(is_layoff), is_layoff := 0L]

#remove fimr-years from panel where firm was not listed 
panel <- panel[ !is.na(exchg) & exchg != 0]

panel_firms <- unique(panel$gvkey)

#save panel to reload in next step
saveRDS( panel, file.path(DATA_FOLDER, "firm_fyear_panel.rds"))




