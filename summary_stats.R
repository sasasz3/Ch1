library(data.table)
library(readxl)
library(stringr)
library(lubridate)


source("config.R")
source("reusable_functions.R")

#load panel with repurchase executions and layoff dummy variable
panel <- readRDS(
  file.path(
    DATA_FOLDER,
    "panel.rds"
  )
)

#load raw controls data 
controls <- read_excel(
  file.path(
    DATA_FOLDER,
    "controls.xlsx"
  )
)

setDT(panel)
setDT(controls)


#clean controls dataset
clean_and_rename(controls)
fix_gvkey(controls)

controls[ ,gvkey := as.character(gvkey)]
controls[ ,fyear := as.integer(fyear)]

setorder(
  panel,
  gvkey,
  fyear
)


#merge controls onto panel
join_vars <- setdiff(
  names(controls),
  c("gvkey", "fyear")
)

panel[
  controls,
  on = .(
    gvkey,
    fyear
  ),
  (join_vars) := mget(
    paste0("i.", join_vars)
  )
]

setorder(
  panel,
  gvkey,
  fyear
)



# AUDIT ADDED CONTROL VARIABLES
#--------------------------------------------------------------

audit_vars <- c(
  "act",
  "ap",
  "at",
  "ceq",
  "ch",
  "che",
  "dlc",
  "dltt",
  "invt",
  "ivst",
  "lct",
  "ppent",
  "rect",
  "dvc",
  "ib",
  "ni",
  "oibdp",
  "sale",
  "xint",
  "xrd",
  "xsga",
  "aqc",
  "capx",
  "dltis",
  "dltr",
  "dv",
  "oancf",
  "sstk",
  "csho",
  "emp",
  "prcc_f"
)

# Retain only variables available in the panel
audit_vars <- intersect(audit_vars, names(panel))

# Generate descriptive audit statistics
variable_audit <- panel[
  ,
  lapply(
    .SD,
    function(x) {
      c(
        n_nonmissing = sum(!is.na(x)),
        n_missing = sum(is.na(x)),
        n_zero = sum(x == 0, na.rm = TRUE),
        n_negative = sum(x < 0, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        p99 = quantile(x, 0.99, na.rm = TRUE),
        max = max(x, na.rm = TRUE)
      )
    }
  ),
  .SDcols = audit_vars
]

variable_audit


# 1. Firm size
panel[
  at > 0,
  size := log(at)
]

# 2. Return on assets
panel[
  at > 0,
  roa := ni / at
]

# 3. Leverage
panel[
  at > 0,
  leverage := (dltt + dlc) / at
]

# 4. Cash holdings
panel[
  at > 0,
  cash_ratio := che / at
]


panel[
  ,
  `:=`(
    previous_sale = shift(sale),
    previous_fyear = shift(fyear)
  ),
  by = gvkey
]

panel[
  previous_fyear != fyear - 1,
  previous_sale := NA_real_
]

panel[
  !is.na(previous_sale) & previous_sale > 0,
  sales_growth := (sale - previous_sale) / previous_sale
]

# 5. Market value of equity
panel[
  !is.na(csho) & !is.na(prcc_f),
  market_equity := csho * prcc_f
]

# 6. Market-to-assets ratio
panel[
  at > 0 & !is.na(ceq) & !is.na(market_equity),
  market_to_assets := (at - ceq + market_equity) / at
]



# 7. Investment intensity
panel[
  at > 0,
  investment := capx / at
]

# 8. Tangibility
panel[
  at > 0,
  tangibility := ppent / at
]



# 12. R&D intensity
panel[
  !is.na(xrd) & !is.na(sale) & sale > 0,
  rd_intensity := xrd / sale
]

# 13. SG&A intensity
panel[
  !is.na(xsga) & !is.na(sale) & sale > 0,
  sga_intensity := xsga / sale
]

# 14. Interest burden
panel[
  at > 0,
  interest_burden := xint / at
]



# 15. Operating cashflow 
panel[
  at > 0,
  operating_cash_flow := oancf / at
]






winsorize_1_99 <- function(x) {
  
  bounds <- quantile(
    x,
    probs = c(0.01, 0.99),
    na.rm = TRUE,
    names = FALSE
  )
  
  pmax(
    pmin(x, bounds[2]),
    bounds[1]
  )
}

# Variables to winsorise
winsor_vars <- c(
  "roa",
  "leverage",
  "cash_ratio",
  "market_to_assets",
  "investment",
  "tangibility",
  "sales_growth",
  "operating_cash_flow",
  "rd_intensity",
  "sga_intensity",
  "interest_burden"
)

# Generate winsorised versions
for (v in winsor_vars) {
  
  new_name <- paste0(v, "_w")
  
  set(
    panel,
    j = new_name,
    value = winsorize_1_99(panel[[v]])
  )
}




control_vars <- c(
  "size",
  "roa", "roa_w",
  "leverage", "leverage_w",
  "cash_ratio", "cash_ratio_w",
  "market_equity",
  "market_to_assets", "market_to_assets_w",
  "investment", "investment_w",
  "tangibility", "tangibility_w",
  "sales_growth", "sales_growth_w",
  "employee_size",
  "operating_cash_flow", "operating_cash_flow_w",
  "dividend_payer",
  "rd_intensity", "rd_intensity_w",
  "sga_intensity", "sga_intensity_w",
  "interest_burden", "interest_burden_w"
)



control_vars <- intersect(control_vars, names(panel))

control_audit <- panel[
  ,
  lapply(.SD, function(x) {
    c(
      non_missing = sum(!is.na(x)),
      missing = sum(is.na(x)),
      mean = if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE),
      median = if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE),
      p1 = if (all(is.na(x))) NA_real_ else
        quantile(x, 0.01, na.rm = TRUE),
      p99 = if (all(is.na(x))) NA_real_ else
        quantile(x, 0.99, na.rm = TRUE)
    )
  }),
  .SDcols = control_vars
]

control_audit


