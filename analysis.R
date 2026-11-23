
# ==================================================
# INITIAL LAYOFF REGRESSIONS: LPM AND LOGIT
# ==================================================

library(data.table)
library(fixest)

source("config.R")
source("reusable_functions.R")


# --------------------------------------------------
# 1. Load panel dataset
# --------------------------------------------------

panel <- readRDS(
  file.path(DATA_FOLDER, "panel.rds")
)

setDT(panel)

# Ensure correct data types
panel[, gvkey := as.character(gvkey)]
panel[, fyear := as.integer(fyear)]

# Sort panel
setorder(panel, gvkey, fyear)
