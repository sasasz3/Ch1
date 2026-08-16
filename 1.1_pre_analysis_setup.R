library(data.table)
library(readxl)
library(lubridate)
library(stringr)
library(fixest)
library(DescTools)

source("config.R")
source("reusable_functions.R")

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

compustat <- setDT(read_excel(file.path(DATA_FOLDER, "compustat.xlsx")))

#column header cleaner functions
clean_and_rename(compustat)
fix_gvkey(compustat)

compustat[, datadate := as.Date(datadate)]


get_firms(warn)
get_firms(sdc)
get_firms(capiq)


#check if all capiq is present in the compustat universe for the set timehorizon
#adjust set if not
get_firms(compustat)
capiq <- capiq[
  gvkey %in% compustat_firms
]

#rewrite capig firms to only include the matching firms
get_firms(capiq)





#audit
all_layoff_firms <- union( warn_firms, capiq_firms)
all_firms <- union(all_layoff_firms, sdc_firms)
scd_layoff_intersect <- intersect(sdc_firms, all_layoff_firms)

#export union of all gvkeys
#to be used to generate the fiscal year map by downloading compustat fiscal years for the below given set
writeLines(sort(all_gvkeys), file.path(DATA_FOLDER, "all_gvkeys_for_compustat.txt"))

# create event objects w/ valid dates
warn_events <- unique(warn[,.(gvkey, date)])
capiq_events <- unique(capiq[,.(gvkey, date)])
sdc_events <- unique(sdc[,.(gvkey, date)])




#MAPPING

fiscal_year_map <- setDT(read_excel(file.path(DATA_FOLDER, "fiscal_year_map.xlsx")))


clean_and_rename(fiscal_year_map)
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

fiscal_year_gvkey <- unique(fiscal_year_map$gvkey)




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

warn_unmatched_events <- warn_events[is.na(fyear)]
capiq_unmatched_events <- capiq_events[is.na(fyear)]
sdc_unmatched_events <- sdc_events[is.na(fyear)]


warn_events <- warn_events[!is.na(fyear)]
capiq_events <- capiq_events[!is.na(fyear)]
sdc_events <- sdc_events[!is.na(fyear)]



#audit
get_firms(warn_events)
get_firms(capiq_events)
get_firms(sdc_events)


event_layoff_firms <- union(capiq_events_firms, warn_events_firms)

#create fimr-year observation objects
warn_firm_years <- unique( warn_events[, .(gvkey, fyear)])
capiq_firm_years <- unique( capiq_events[, .(gvkey, fyear)])
buyback_firm_years <- unique(sdc_events[, .(gvkey, fyear)]) 

#put WARN observations and CAPIQ observations together
layoff_firm_years <- unique( rbindlist( list( warn_firm_years, capiq_firm_years ),use.names = TRUE ))
event_layoff_firms <- unique(layoff_firm_years$gvkey)

#interection between layoff and buyback firm-year observations 
layoff_buyback_fy_intersect <- merge(layoff_firm_years,buyback_firm_years,by = c("gvkey", "fyear"))


# create list of all events
all_event_firm_years <- unique(rbindlist(list(layoff_firm_years, buyback_firm_years), use.names = TRUE))

#audit again
all_event_firms <- union(sdc_events_firms, layoff_firms)
buyback_layoff_firms <- intersect(sdc_events_firms, layoff_firms)
get_firms(layoff_buyback_fy_intersect)





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


# Firm-years without a layoff become zero
panel[ is.na(is_layoff), is_layoff := 0L]


get_firms(panel)
panel <- panel[ !is.na(exchg) & exchg != 0]
get_firms(panel)

#getting rid of those gvkeys that were in the compustat universe but without valid date
panel_without_events <- setdiff(panel_firms, all_event_firms)
panel <- panel[!gvkey %in% panel_without_events]

get_firms(panel)





panel_layoff_firms <- unique(panel[is_layoff == 1, gvkey])
panel_buyback_firms <- unique(panel[is_buyback == 1, gvkey])
panel_layoff_buyback_firms <- intersect( panel_layoff_firms, panel_buyback_firms)

panel_layoff_firm_years <- unique(
  panel[is_layoff == 1, .(gvkey, fyear)]
)

# Firm-years with at least one buyback
panel_buyback_firm_years <- unique(
  panel[is_buyback == 1, .(gvkey, fyear)]
)


panel_event_firm_years <- unique(
  panel[
    is_layoff == 1 | is_buyback == 1,
    .(gvkey, fyear)
  ]
)

# Firm-years with both
panel_layoff_buyback_firm_years <- unique(
  panel[
    is_layoff == 1 & is_buyback == 1,
    .(gvkey, fyear)
  ]
)




#save panel to reload in next step
saveRDS( panel, file.path(DATA_FOLDER, "firm_fyear_panel.rds"))




