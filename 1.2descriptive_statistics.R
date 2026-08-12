library(data.table)
library(fixest)

source("config.R")

# Load final analysis panel
panel <- readRDS(
  file.path(DATA_FOLDER, "firm_fyear_panel.rds")
)

setDT(panel)


#audits
firm_years <- nrow(panel)
all_firms <- unique(panel$gvkey)


#calendar borders - do not align with actual time horizon
year_start <- min(panel$fyear, na.rm = TRUE)
year_end <- max(panel$fyear, na.rm = TRUE)


event_firm_years <- panel[is_layoff == 1 | is_buyback == 1, .N] 


#count frequencies 
event_counts <- panel[
  ,
  .(
    firm_years = .N,
    
    layoff_firm_years =
      sum(is_layoff == 1),
    
    buyback_firm_years =
      sum(is_buyback == 1),
    
    both_firm_years =
      sum(is_layoff == 1 &
            is_buyback == 1),
    
    layoff_only =
      sum(is_layoff == 1 &
            is_buyback == 0),
    
    buyback_only =
      sum(is_layoff == 0 &
            is_buyback == 1),
    
    neither =
      sum(is_layoff == 0 &
            is_buyback == 0)
  )
]

print(event_counts)


#count event rates

event_rates <- panel[
  ,
  .(
    layoff_rate = mean(is_layoff),
    buyback_rate = mean(is_buyback),
    
    both_rate =
      mean(is_layoff == 1 &
             is_buyback == 1),
    
    layoff_only_rate =
      mean(is_layoff == 1 &
             is_buyback == 0),
    
    buyback_only_rate =
      mean(is_layoff == 0 &
             is_buyback == 1),
    
    neither_rate =
      mean(is_layoff == 0 &
             is_buyback == 0)
  )
]

print(event_rates)