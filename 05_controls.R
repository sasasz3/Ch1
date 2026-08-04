library(data.table)
library(readxl)
library(lubridate)
library(stringr)
library(fixest)
library(ggplot2)
library(DescTools)


source("config.R")

controls <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "controls.xlsx")
  )
)

clean_and_rename(controls)

setnames(
  controls,
  tolower(names(controls))
)


setnames(
  controls,
  old = c("loss", "net"),
  new = c("ni", "sale")
)

controls[
  ,
  gvkey := str_pad(
    as.character(gvkey),
    width = 6,
    side = "left",
    pad = "0"
  )
]

controls[
  ,
  `:=`(
    datadate = as.Date(datadate),
    fiscal_year = as.integer(fyear)
  )
]

controls <- controls[
  indfmt == "INDL" &
    datafmt == "STD" &
    consol == "C"
]





#CREATE CONTROL VARIABLES 

control_variables <- controls[
  !is.na(gvkey) &
    !is.na(fiscal_year),
  .(
    gvkey,
    fiscal_year,
    datadate,
    sic,
    at,
    ceq,
    che,
    dlc,
    dltt,
    dvc,
    ib,
    ni,
    sale,
    capx,
    csho,
    prcc_f
  )
]

control_variables[
  ,
  `:=`(
    log_assets = fifelse(
      !is.na(at) & at > 0,
      log(at),
      NA_real_
    ),
    
    roa = fifelse(
      !is.na(at) & at > 0,
      ib / at,
      NA_real_
    ),
    
    cash_ratio = fifelse(
      !is.na(at) & at > 0,
      che / at,
      NA_real_
    ),
    
    leverage = fifelse(
      !is.na(at) & at > 0,
      (
        fcoalesce(dltt, 0) +
          fcoalesce(dlc, 0)
      ) / at,
      NA_real_
    ),
    
    capex_ratio = fifelse(
      !is.na(at) & at > 0,
      capx / at,
      NA_real_
    ),
    
    sales_ratio = fifelse(
      !is.na(at) & at > 0,
      sale / at,
      NA_real_
    ),
    
    dividend_payer = fifelse(
      is.na(dvc),
      NA_integer_,
      as.integer(dvc > 0)
    ),
    
    market_to_book = fifelse(
      !is.na(prcc_f) &
        !is.na(csho) &
        !is.na(ceq) &
        ceq > 0,
      (prcc_f * csho) / ceq,
      NA_real_
    )
  )
]




#AATACH CONTROLS TO PANEL

# ============================================================
# CREATE ONE-FISCAL-YEAR LAGGED CONTROLS
# ============================================================

setorder(
  control_variables,
  gvkey,
  fiscal_year
)

control_variables[
  ,
  previous_fiscal_year := shift(fiscal_year),
  by = gvkey
]

control_variables[
  ,
  `:=`(
    log_assets_lag1 = shift(log_assets),
    roa_lag1 = shift(roa),
    cash_ratio_lag1 = shift(cash_ratio),
    leverage_lag1 = shift(leverage),
    capex_ratio_lag1 = shift(capex_ratio),
    sales_ratio_lag1 = shift(sales_ratio),
    dividend_payer_lag1 = shift(dividend_payer),
    market_to_book_lag1 = shift(market_to_book)
  ),
  by = gvkey
]


lagged_control_names <- c(
  "log_assets_lag1",
  "roa_lag1",
  "cash_ratio_lag1",
  "leverage_lag1",
  "capex_ratio_lag1",
  "sales_ratio_lag1",
  "dividend_payer_lag1",
  "market_to_book_lag1"
)

control_variables[
  is.na(previous_fiscal_year) |
    fiscal_year - previous_fiscal_year != 1L,
  (lagged_control_names) := NA
]


lagged_control_lookup <- control_variables[
  ,
  .(
    gvkey,
    fiscal_year,
    log_assets_lag1,
    roa_lag1,
    cash_ratio_lag1,
    leverage_lag1,
    capex_ratio_lag1,
    sales_ratio_lag1,
    dividend_payer_lag1,
    market_to_book_lag1
  )
]

fiscal_year_calendar <- merge(
  fiscal_year_calendar,
  lagged_control_lookup,
  by = c("gvkey", "fiscal_year"),
  all.x = TRUE,
  sort = FALSE
)

setorder(
  fiscal_year_calendar,
  gvkey,
  fiscal_year
)


fiscal_year_calendar[
  ,
  core_control_sample := complete.cases(
    log_assets_lag1,
    roa_lag1,
    cash_ratio_lag1,
    leverage_lag1
  )
]

table(fiscal_year_calendar$core_control_sample)

model_fiscal_binary_same_sample <- feols(
  buyback_year ~ layoff_year |
    gvkey + fiscal_year,
  data = fiscal_year_calendar[
    core_control_sample == TRUE
  ],
  cluster = ~gvkey
)

summary(model_fiscal_binary_same_sample)


model_fiscal_controls <- feols(
  buyback_year ~
    layoff_year +
    log_assets_lag1 +
    roa_lag1 +
    cash_ratio_lag1 +
    leverage_lag1 |
    gvkey + fiscal_year,
  data = fiscal_year_calendar[
    core_control_sample == TRUE
  ],
  cluster = ~gvkey
)

summary(model_fiscal_controls)
