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


winsorize_1_99 <- function(x) {
  
  bounds <- quantile(
    x,
    probs = c(0.01, 0.99),
    na.rm = TRUE
  )
  
  pmax(
    pmin(
      x,
      bounds[2]
    ),
    bounds[1]
  )
}


# ROA
panel[
  ,
  roa_w :=
    winsorize_1_99(
      roa
    )
]


# Leverage
panel[
  ,
  leverage_w :=
    winsorize_1_99(
      leverage
    )
]


# Market-to-book
panel[
  ,
  market_to_book_w :=
    winsorize_1_99(
      market_to_book
    )
]


#join the lagged version of the original controls back onto the panel
control_panel <- panel[
  ,
  .(
    gvkey,
    fyear,
    size,
    roa_w,
    leverage_w,
    cash_ratio,
    market_to_book_w
  )
]

control_lag1 <- control_panel[
  ,
  .(
    gvkey,
    fyear = fyear + 1,
    
    lag_size = size,
    
    lag_roa = roa_w,
    
    lag_leverage = leverage_w,
    
    lag_cash_ratio = cash_ratio,
    
    lag_market_to_book = market_to_book_w
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

control_descriptives <- panel[
  ,
  .(
    variable = c(
      "Firm size",
      "ROA",
      "Leverage",
      "Cash ratio",
      "Market-to-book"
    ),
    
    n = c(
      sum(!is.na(size)),
      sum(!is.na(roa_w)),
      sum(!is.na(leverage_w)),
      sum(!is.na(cash_ratio)),
      sum(!is.na(market_to_book_w))
    ),
    
    mean = c(
      mean(size, na.rm = TRUE),
      mean(roa_w, na.rm = TRUE),
      mean(leverage_w, na.rm = TRUE),
      mean(cash_ratio, na.rm = TRUE),
      mean(market_to_book_w, na.rm = TRUE)
    ),
    
    sd = c(
      sd(size, na.rm = TRUE),
      sd(roa_w, na.rm = TRUE),
      sd(leverage_w, na.rm = TRUE),
      sd(cash_ratio, na.rm = TRUE),
      sd(market_to_book_w, na.rm = TRUE)
    ),
    
    min = c(
      min(size, na.rm = TRUE),
      min(roa_w, na.rm = TRUE),
      min(leverage_w, na.rm = TRUE),
      min(cash_ratio, na.rm = TRUE),
      min(market_to_book_w, na.rm = TRUE)
    ),
    
    p25 = c(
      quantile(size, 0.25, na.rm = TRUE),
      quantile(roa_w, 0.25, na.rm = TRUE),
      quantile(leverage_w, 0.25, na.rm = TRUE),
      quantile(cash_ratio, 0.25, na.rm = TRUE),
      quantile(market_to_book_w, 0.25, na.rm = TRUE)
    ),
    
    median = c(
      median(size, na.rm = TRUE),
      median(roa_w, na.rm = TRUE),
      median(leverage_w, na.rm = TRUE),
      median(cash_ratio, na.rm = TRUE),
      median(market_to_book_w, na.rm = TRUE)
    ),
    
    p75 = c(
      quantile(size, 0.75, na.rm = TRUE),
      quantile(roa_w, 0.75, na.rm = TRUE),
      quantile(leverage_w, 0.75, na.rm = TRUE),
      quantile(cash_ratio, 0.75, na.rm = TRUE),
      quantile(market_to_book_w, 0.75, na.rm = TRUE)
    ),
    
    max = c(
      max(size, na.rm = TRUE),
      max(roa_w, na.rm = TRUE),
      max(leverage_w, na.rm = TRUE),
      max(cash_ratio, na.rm = TRUE),
      max(market_to_book_w, na.rm = TRUE)
    )
  )
]

control_descriptives



lagged_control_descriptives <- panel[
  ,
  .(
    variable = c(
      "Firm size",
      "ROA",
      "Leverage",
      "Cash ratio",
      "Market-to-book"
    ),
    
    n = c(
      sum(!is.na(lag_size)),
      sum(!is.na(lag_roa)),
      sum(!is.na(lag_leverage)),
      sum(!is.na(lag_cash_ratio)),
      sum(!is.na(lag_market_to_book))
    ),
    
    mean = c(
      mean(lag_size, na.rm = TRUE),
      mean(lag_roa, na.rm = TRUE),
      mean(lag_leverage, na.rm = TRUE),
      mean(lag_cash_ratio, na.rm = TRUE),
      mean(lag_market_to_book, na.rm = TRUE)
    ),
    
    sd = c(
      sd(lag_size, na.rm = TRUE),
      sd(lag_roa, na.rm = TRUE),
      sd(lag_leverage, na.rm = TRUE),
      sd(lag_cash_ratio, na.rm = TRUE),
      sd(lag_market_to_book, na.rm = TRUE)
    ),
    
    min = c(
      min(lag_size, na.rm = TRUE),
      min(lag_roa, na.rm = TRUE),
      min(lag_leverage, na.rm = TRUE),
      min(lag_cash_ratio, na.rm = TRUE),
      min(lag_market_to_book, na.rm = TRUE)
    ),
    
    p25 = c(
      quantile(lag_size, 0.25, na.rm = TRUE),
      quantile(lag_roa, 0.25, na.rm = TRUE),
      quantile(lag_leverage, 0.25, na.rm = TRUE),
      quantile(lag_cash_ratio, 0.25, na.rm = TRUE),
      quantile(lag_market_to_book, 0.25, na.rm = TRUE)
    ),
    
    median = c(
      median(lag_size, na.rm = TRUE),
      median(lag_roa, na.rm = TRUE),
      median(lag_leverage, na.rm = TRUE),
      median(lag_cash_ratio, na.rm = TRUE),
      median(lag_market_to_book, na.rm = TRUE)
    ),
    
    p75 = c(
      quantile(lag_size, 0.75, na.rm = TRUE),
      quantile(lag_roa, 0.75, na.rm = TRUE),
      quantile(lag_leverage, 0.75, na.rm = TRUE),
      quantile(lag_cash_ratio, 0.75, na.rm = TRUE),
      quantile(lag_market_to_book, 0.75, na.rm = TRUE)
    ),
    
    max = c(
      max(lag_size, na.rm = TRUE),
      max(lag_roa, na.rm = TRUE),
      max(lag_leverage, na.rm = TRUE),
      max(lag_cash_ratio, na.rm = TRUE),
      max(lag_market_to_book, na.rm = TRUE)
    )
  )
]

lagged_control_descriptives



