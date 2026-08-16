library(data.table)
library(readxl)
library(fixest)

source("config.R")


panel <- readRDS(
  file.path(
    DATA_FOLDER,
    "firm_fyear_panel_controls.rds"
  )
)

setDT(panel)

panel[
  ,
  gvkey := as.character(gvkey)
]

setorder(
  panel,
  gvkey,
  fyear
)

dim(panel)
uniqueN(panel$gvkey)
range(panel$fyear)