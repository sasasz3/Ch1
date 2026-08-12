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



