library(data.table)
library(fixest)

source("config.R")

panel <- readRDS( file.path(DATA_FOLDER, "firm_fyear_panel.rds"))
setDT(panel)

