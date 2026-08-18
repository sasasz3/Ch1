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


m_buyback_same_controls <- feols(
  is_buyback ~
    is_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

summary(m_buyback_same_controls)











#read in new controls file from Compustat
repurchase_actual <- read_excel(
  file.path(
    DATA_FOLDER,
    "actual_repurchase_controls.xlsx"
  )
)

setDT(repurchase_actual)


#clean headers, keep names in the parentheses
clean_and_rename <- function(DT) {
  current_names <- colnames(DT)
  
  new_names <- gsub(
    ".*\\((.*)\\).*",
    "\\1",
    current_names
  )
  
  setnames(
    DT,
    current_names,
    new_names
  )
}

clean_and_rename(repurchase_actual)



repurchase_actual[
  ,
  gvkey := as.character(gvkey)
]

setorder(
  repurchase_actual,
  gvkey,
  fyear
)



#audit actual repurchase information
repurchase_actual[
  ,
  .(
    n_obs = .N,
    n_missing = sum(is.na(prstkc)),
    pct_missing = mean(is.na(prstkc)),
    n_zero = sum(prstkc == 0, na.rm = TRUE),
    n_positive = sum(prstkc > 0, na.rm = TRUE),
    n_negative = sum(prstkc < 0, na.rm = TRUE)
  )
]

repurchase_actual[
  !is.na(prstkc),
  .(
    mean = mean(prstkc),
    median = median(prstkc),
    p90 = quantile(prstkc, 0.90),
    p95 = quantile(prstkc, 0.95),
    p99 = quantile(prstkc, 0.99),
    max = max(prstkc)
  )
]


prstkc_by_year <- repurchase_actual[
  ,
  .(
    n_obs = .N,
    n_nonmissing = sum(!is.na(prstkc)),
    missing_rate = mean(is.na(prstkc)),
    n_zero = sum(prstkc == 0, na.rm = TRUE),
    n_positive = sum(prstkc > 0, na.rm = TRUE),
    positive_rate = mean(prstkc > 0, na.rm = TRUE)
  ),
  by = fyear
][order(fyear)]

prstkc_by_year


#merge new observations into the panel 
repurchase_merge <- repurchase_actual[
  ,
  .(
    gvkey,
    fyear,
    prstkc
  )
]

panel <- merge(
  panel,
  repurchase_merge,
  by = c("gvkey", "fyear"),
  all.x = TRUE
)

setorder(
  panel,
  gvkey,
  fyear
)

panel[
  ,
  .(
    n_panel_obs = .N,
    n_prstkc_observed = sum(!is.na(prstkc)),
    n_prstkc_missing = sum(is.na(prstkc)),
    prstkc_missing_rate = mean(is.na(prstkc)),
    
    n_prstkc_zero = sum(
      prstkc == 0,
      na.rm = TRUE
    ),
    
    n_prstkc_positive = sum(
      prstkc > 0,
      na.rm = TRUE
    )
  )
]

panel_prstkc_by_year <- panel[
  ,
  .(
    n_obs = .N,
    n_observed = sum(!is.na(prstkc)),
    n_missing = sum(is.na(prstkc)),
    missing_rate = mean(is.na(prstkc)),
    
    n_positive = sum(
      prstkc > 0,
      na.rm = TRUE
    ),
    
    positive_rate = mean(
      prstkc > 0,
      na.rm = TRUE
    )
  ),
  by = fyear
][
  order(fyear)
]

panel_prstkc_by_year


panel[
  !is.na(prstkc),
  actual_repurchaser := as.integer(
    prstkc > 0
  )
]



sdc_actual_table <- panel[
  !is.na(prstkc),
  .N,
  by = .(
    is_buyback,
    actual_repurchaser
  )
][
  order(
    is_buyback,
    actual_repurchaser
  )
]

sdc_actual_table




sdc_actual_comparison <- panel[
  !is.na(prstkc),
  .(
    n_obs = .N,
    
    n_actual_repurchasers = sum(
      actual_repurchaser == 1
    ),
    
    actual_repurchase_rate = mean(
      actual_repurchaser
    ),
    
    mean_prstkc = mean(
      prstkc
    ),
    
    median_prstkc = median(
      prstkc
    )
  ),
  by = is_buyback
]

sdc_actual_comparison


#add total assets to create repurchase intensity

assets_merge <- repurchase_actual[
  ,
  .(
    gvkey,
    fyear,
    at_current = at
  )
]

panel <- merge(
  panel,
  assets_merge,
  by = c("gvkey", "fyear"),
  all.x = TRUE
)

setorder(
  panel,
  gvkey,
  fyear
)


#audit total assets
panel[
  ,
  .(
    n_obs = .N,
    n_missing_at = sum(is.na(at_current)),
    n_zero_at = sum(
      at_current == 0,
      na.rm = TRUE
    ),
    n_negative_at = sum(
      at_current < 0,
      na.rm = TRUE
    )
  )
]


#create repurchase intestiy
panel[
  !is.na(prstkc) &
    !is.na(at_current) &
    at_current > 0,
  repurchase_intensity :=
    prstkc / at_current
]


panel[
  !is.na(repurchase_intensity),
  .(
    n_obs = .N,
    
    mean = mean(
      repurchase_intensity
    ),
    
    median = median(
      repurchase_intensity
    ),
    
    p75 = quantile(
      repurchase_intensity,
      0.75
    ),
    
    p90 = quantile(
      repurchase_intensity,
      0.90
    ),
    
    p95 = quantile(
      repurchase_intensity,
      0.95
    ),
    
    p99 = quantile(
      repurchase_intensity,
      0.99
    ),
    
    max = max(
      repurchase_intensity
    )
  )
]


panel[
  repurchase_intensity > 0 &
    !is.na(repurchase_intensity),
  .(
    n_obs = .N,
    
    mean = mean(
      repurchase_intensity
    ),
    
    median = median(
      repurchase_intensity
    ),
    
    p75 = quantile(
      repurchase_intensity,
      0.75
    ),
    
    p90 = quantile(
      repurchase_intensity,
      0.90
    ),
    
    p95 = quantile(
      repurchase_intensity,
      0.95
    ),
    
    p99 = quantile(
      repurchase_intensity,
      0.99
    ),
    
    max = max(
      repurchase_intensity
    )
  )
]



repurchase_actual[
  prstkc < 0,
  .(
    gvkey,
    fyear,
    datadate,
    prstkc,
    at
  )
]




panel[
  ,
  prstkc_clean := prstkc
]

panel[
  !is.na(prstkc_clean) &
    prstkc_clean < 0,
  prstkc_clean := 0
]

panel[
  !is.na(prstkc_clean) &
    !is.na(at_current) &
    at_current > 0,
  repurchase_intensity :=
    prstkc_clean / at_current
]

panel[
  !is.na(repurchase_intensity)
][
  order(-repurchase_intensity)
][
  1:30,
  .(
    gvkey,
    fyear,
    prstkc,
    at_current,
    repurchase_intensity,
    is_buyback,
    is_layoff
  )
]

panel[
  !is.na(repurchase_intensity),
  .(
    n_obs = .N,
    
    above_10pct = sum(
      repurchase_intensity > 0.10
    ),
    
    above_25pct = sum(
      repurchase_intensity > 0.25
    ),
    
    above_50pct = sum(
      repurchase_intensity > 0.50
    ),
    
    above_100pct = sum(
      repurchase_intensity > 1
    ),
    
    above_200pct = sum(
      repurchase_intensity > 2
    )
  )
]


panel[
  ,
  repurchase_intensity_raw :=
    repurchase_intensity
]

p01 <- quantile(
  panel$repurchase_intensity_raw,
  0.01,
  na.rm = TRUE
)

p99 <- quantile(
  panel$repurchase_intensity_raw,
  0.99,
  na.rm = TRUE
)

panel[
  ,
  repurchase_intensity_w :=
    pmin(
      pmax(
        repurchase_intensity_raw,
        p01
      ),
      p99
    )
]

c(
  p01 = p01,
  p99 = p99
)


panel[
  ,
  log_repurchase_intensity :=
    log1p(
      repurchase_intensity_raw
    )
]




#recreate leads and lags for layoffs
event_panel <- panel[
  ,
  .(
    gvkey,
    fyear,
    is_layoff
  )
]

for (k in 1:3) {
  
  # ----------------------------------------------------------
  # LAYOFF LAG
  # layoff_lag1 = layoff in fiscal year t-1
  # ----------------------------------------------------------
  
  temp <- event_panel[
    ,
    .(
      gvkey,
      fyear = fyear + k,
      value = is_layoff
    )
  ]
  
  panel[
    temp,
    on = .(gvkey, fyear),
    (paste0("layoff_lag", k)) := i.value
  ]
  
  
  # ----------------------------------------------------------
  # LAYOFF LEAD
  # layoff_lead1 = layoff in fiscal year t+1
  # ----------------------------------------------------------
  
  temp <- event_panel[
    ,
    .(
      gvkey,
      fyear = fyear - k,
      value = is_layoff
    )
  ]
  
  panel[
    temp,
    on = .(gvkey, fyear),
    (paste0("layoff_lead", k)) := i.value
  ]
}


panel[
  ,
  .(
    n_obs = .N,
    
    lag1_observed = sum(!is.na(layoff_lag1)),
    lag2_observed = sum(!is.na(layoff_lag2)),
    lag3_observed = sum(!is.na(layoff_lag3)),
    
    lead1_observed = sum(!is.na(layoff_lead1)),
    lead2_observed = sum(!is.na(layoff_lead2)),
    lead3_observed = sum(!is.na(layoff_lead3))
  )
]






lead2_intensity_comparison <- panel[
  !is.na(repurchase_intensity_raw) &
    !is.na(layoff_lead2),
  .(
    n_obs = .N,
    
    mean_raw = mean(repurchase_intensity_raw),
    median_raw = median(repurchase_intensity_raw),
    
    mean_winsorized = mean(repurchase_intensity_w),
    median_winsorized = median(repurchase_intensity_w),
    
    p75_raw = quantile(
      repurchase_intensity_raw,
      0.75
    ),
    
    p90_raw = quantile(
      repurchase_intensity_raw,
      0.90
    )
  ),
  by = layoff_lead2
]

lead2_intensity_comparison





panel[
  ,
  repurchase_group := NA_character_
]

# Zero expenditure gets its own category
panel[
  !is.na(repurchase_intensity_raw) &
    repurchase_intensity_raw == 0,
  repurchase_group := "0: No expenditure"
]

# Fiscal-year-specific quintiles among positive repurchasers
panel[
  repurchase_intensity_raw > 0 &
    !is.na(repurchase_intensity_raw),
  positive_intensity_quintile :=
    frank(
      repurchase_intensity_raw,
      ties.method = "average"
    ) / .N,
  by = fyear
]

panel[
  !is.na(positive_intensity_quintile),
  repurchase_group :=
    fcase(
      positive_intensity_quintile <= 0.20, "1: Positive Q1",
      positive_intensity_quintile <= 0.40, "2: Positive Q2",
      positive_intensity_quintile <= 0.60, "3: Positive Q3",
      positive_intensity_quintile <= 0.80, "4: Positive Q4",
      positive_intensity_quintile <= 1.00, "5: Positive Q5"
    )
]






lead2_by_repurchase_group <- panel[
  !is.na(repurchase_group) &
    !is.na(layoff_lead2),
  .(
    n_obs = .N,
    
    n_layoffs_lead2 = sum(
      layoff_lead2
    ),
    
    layoff_lead2_rate = mean(
      layoff_lead2
    ),
    
    mean_intensity = mean(
      repurchase_intensity_raw
    ),
    
    median_intensity = median(
      repurchase_intensity_raw
    )
  ),
  by = repurchase_group
][
  order(repurchase_group)
]

lead2_by_repurchase_group






m_actual_layoff_same <- feols(
  is_layoff ~
    repurchase_intensity_w +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

m_actual_layoff_lead1 <- feols(
  layoff_lead1 ~
    repurchase_intensity_w +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

m_actual_layoff_lead2 <- feols(
  layoff_lead2 ~
    repurchase_intensity_w +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

m_actual_layoff_lead3 <- feols(
  layoff_lead3 ~
    repurchase_intensity_w +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)


etable(
  m_actual_layoff_same,
  m_actual_layoff_lead1,
  m_actual_layoff_lead2,
  m_actual_layoff_lead3
)







panel[
  !is.na(layoff_lead2) &
    !is.na(layoff_lead3),
  layoff_lead23 := as.integer(
    layoff_lead2 == 1 |
      layoff_lead3 == 1
  )
]

panel[
  !is.na(layoff_lead2) &
    !is.na(layoff_lead3),
  layoff_count_lead23 :=
    layoff_lead2 + layoff_lead3
]