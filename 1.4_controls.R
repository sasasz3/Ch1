library(data.table)
library(readxl)
library(fixest)

source("config.R")
source("reusable_functions.R")

#load previously made panel and controls/other variables file 
panel <- readRDS(
  file.path(
    DATA_FOLDER,
    "firm_fyear_panel.rds"
  )
)

other_variables <- read_excel(
  file.path(
    DATA_FOLDER,
    "compustat_controls.xlsx"
  )
)

setDT(panel)
setDT(other_variables)

clean_and_rename(other_variables)
fix_gvkey(other_variables)

other_variables[ ,gvkey := as.character(gvkey)]
other_variables[ ,fyear := as.integer(fyear)]

setorder(
  panel,
  gvkey,
  fyear
)

#merge controls and other variables onto panel
join_vars <- setdiff(
  names(other_variables),
  c("gvkey", "fyear")
)


panel[
  other_variables,
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



#audit added variables 
audit_vars <- c(
  "at",
  "sale",
  "ni",
  "che",
  "ch",
  "ivst",
  "dlc",
  "dltt",
  "ceq",
  "oancf",
  "prstkc",
  "sstk",
  "capx",
  "aqc",
  "dltis",
  "dltr",
  "emp",
  "csho",
  "prcc_f"
)

panel[
  ,
  lapply(
    .SD,
    function(x) {
      c(
        n_nonmissing = sum(!is.na(x)),
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



# create original model controls and add them to the panel 
# Firm size
panel[
  at > 0,
  size := log(at)
]

# Return on assets
panel[
  at > 0,
  roa := ni / at
]

# Leverage
panel[
  at > 0,
  leverage := (dltt + dlc) / at
]

# Cash ratio
panel[
  at > 0,
  cash_ratio := che / at
]

# Market value of equity
panel[
  ,
  market_equity := csho * prcc_f
]

# Book value
panel[
  ,
  book_value := at - lt
]

# Market-to-book ratio
panel[
  at > 0,
  market_to_book :=
    (at - ceq + market_equity) / at
]



#join the lagged version of the original controls back onto the panel
control_panel <- panel[
  ,
  .(
    gvkey,
    fyear,
    size,
    roa,
    leverage,
    cash_ratio,
    market_to_book
  )
]

control_lag1 <- control_panel[
  ,
  .(
    gvkey,
    fyear = fyear + 1,
    lag_size = size,
    lag_roa = roa,
    lag_leverage = leverage,
    lag_cash_ratio = cash_ratio,
    lag_market_to_book = market_to_book
  )
]

panel[
  control_lag1,
  on = .(
    gvkey,
    fyear
  ),
  `:=`(
    lag_size = i.lag_size,
    lag_roa = i.lag_roa,
    lag_leverage = i.lag_leverage,
    lag_cash_ratio = i.lag_cash_ratio,
    lag_market_to_book = i.lag_market_to_book
  )
]


#descriptive statistics for controls 
panel[
  ,
  .(
    variable = c(
      "size",
      "roa",
      "leverage",
      "cash_ratio",
      "market_to_book"
    ),
    
    mean = c(
      mean(size, na.rm = TRUE),
      mean(roa, na.rm = TRUE),
      mean(leverage, na.rm = TRUE),
      mean(cash_ratio, na.rm = TRUE),
      mean(market_to_book, na.rm = TRUE)
    ),
    
    median = c(
      median(size, na.rm = TRUE),
      median(roa, na.rm = TRUE),
      median(leverage, na.rm = TRUE),
      median(cash_ratio, na.rm = TRUE),
      median(market_to_book, na.rm = TRUE)
    ),
    
    p1 = c(
      quantile(size, .01, na.rm = TRUE),
      quantile(roa, .01, na.rm = TRUE),
      quantile(leverage, .01, na.rm = TRUE),
      quantile(cash_ratio, .01, na.rm = TRUE),
      quantile(market_to_book, .01, na.rm = TRUE)
    ),
    
    p99 = c(
      quantile(size, .99, na.rm = TRUE),
      quantile(roa, .99, na.rm = TRUE),
      quantile(leverage, .99, na.rm = TRUE),
      quantile(cash_ratio, .99, na.rm = TRUE),
      quantile(market_to_book, .99, na.rm = TRUE)
    )
  )
]


#descriptive statistics for lagged controls 
panel[
  ,
  .(
    variable = c(
      "lag_size",
      "lag_roa",
      "lag_leverage",
      "lag_cash_ratio",
      "lag_market_to_book"
    ),
    
    mean = c(
      mean(lag_size, na.rm = TRUE),
      mean(lag_roa, na.rm = TRUE),
      mean(lag_leverage, na.rm = TRUE),
      mean(lag_cash_ratio, na.rm = TRUE),
      mean(lag_market_to_book, na.rm = TRUE)
    ),
    
    median = c(
      median(lag_size, na.rm = TRUE),
      median(lag_roa, na.rm = TRUE),
      median(lag_leverage, na.rm = TRUE),
      median(lag_cash_ratio, na.rm = TRUE),
      median(lag_market_to_book, na.rm = TRUE)
    )
  )
]






