library(data.table)

source("config.R")

employment <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "compustat_employment.xlsx"),
    sheet = "Results"
  )
)

clean_and_rename(employment)
fix_gvkey(employment)

setDT(employment)

setorder(employment, gvkey, fyear)

employment[, emp_prev := shift(emp), by = gvkey]

employment[, emp_change := (emp - emp_prev) / emp_prev]

employment[, change_10_pct := emp_change <= -0.10]

employment[, change_5_pct := emp_change <= -0.05]


num_decrease_10 <- employment[change_10_pct == TRUE, .N]
num_decrease_5 <- employment[change_5_pct == TRUE, .N]

num_decrease_10
num_decrease_5


firm_10_decrease <- employment[change_10_pct == TRUE, uniqueN(gvkey)]
firm_5_decrease <- employment[change_5_pct == TRUE, uniqueN(gvkey)]

firm_10_decrease
firm_5_decrease