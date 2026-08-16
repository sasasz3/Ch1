library(data.table)
library(readxl)
library(fixest)

source("config.R")

panel <- readRDS( file.path(DATA_FOLDER, "firm_fyear_panel_controls.rds"))
setDT(panel)

panel[, gvkey := as.character(gvkey)]


# keep the original event indicators in a temporary object
event_panel <- panel[
  ,
  .(
    gvkey,
    fyear,
    is_layoff,
    is_buyback
  )
]

for (k in 1:3) {
  

# creating lags for layoffs
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
  
  

#creating leads for layoffs
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
  
  
# creating lags for buybacks
  temp <- event_panel[
    ,
    .(
      gvkey,
      fyear = fyear + k,
      value = is_buyback
    )
  ]
  
  panel[
    temp,
    on = .(gvkey, fyear),
    (paste0("buyback_lag", k)) := i.value
  ]
  
  

# creating leads for buybacks  
  temp <- event_panel[
    ,
    .(
      gvkey,
      fyear = fyear - k,
      value = is_buyback
    )
  ]
  
  panel[
    temp,
    on = .(gvkey, fyear),
    (paste0("buyback_lead", k)) := i.value
  ]
}


#layoff correlation matrix

layoff_corr_vars <- c(
  "layoff_lag3",
  "layoff_lag2",
  "layoff_lag1",
  "is_layoff",
  "layoff_lead1",
  "layoff_lead2",
  "layoff_lead3"
)

layoff_corr_matrix <- cor(
  panel[, ..layoff_corr_vars],
  use = "pairwise.complete.obs",
  method = "pearson"
)

round(layoff_corr_matrix, 3)



#persistence for all layoff leads and lags 
layoff_persistence <- data.table(
  variable = c(
    "layoff_lag3",
    "layoff_lag2",
    "layoff_lag1",
    "layoff_lead1",
    "layoff_lead2",
    "layoff_lead3"
  )
)

layoff_persistence[
  ,
  `:=`(
    correlation = sapply(
      variable,
      function(v) {
        cor(
          panel$is_layoff,
          panel[[v]],
          use = "complete.obs"
        )
      }
    ),
    
    n_obs = sapply(
      variable,
      function(v) {
        sum(
          complete.cases(
            panel$is_layoff,
            panel[[v]]
          )
        )
      }
    )
  )
]

layoff_persistence



#same sample testing
#running the previous dynamic models on the same sample
#to exclude sample change assumption in the changing of results



#create common sample
pm3_required_vars <- c(
  "is_buyback",
  "layoff_lag3",
  "layoff_lag2",
  "layoff_lag1",
  "is_layoff",
  "layoff_lead1",
  "layoff_lead2",
  "layoff_lead3",
  "lag_size",
  "lag_roa",
  "lag_leverage",
  "lag_cash_ratio",
  "lag_market_to_book",
  "gvkey",
  "fyear"
)

panel_pm3_common <- panel[
  complete.cases(panel[, ..pm3_required_vars])
]

nrow(panel_pm3_common)
uniqueN(panel_pm3_common$gvkey)


#same year model
m_buyback_common_same <- feols(
  is_buyback ~
    is_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm3_common,
  cluster = ~gvkey
)

summary(m_buyback_common_same)


# +/- 1 lag/lead model 
m_buyback_common_pm1 <- feols(
  is_buyback ~
    layoff_lag1 +
    is_layoff +
    layoff_lead1 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm3_common,
  cluster = ~gvkey
)

summary(m_buyback_common_pm1)


# +/- 2 lag/lead model 
m_buyback_common_pm2 <- feols(
  is_buyback ~
    layoff_lag2 +
    layoff_lag1 +
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm3_common,
  cluster = ~gvkey
)

summary(m_buyback_common_pm2)



#+/- 3 lag/lead model 
m_buyback_common_pm3 <- feols(
  is_buyback ~
    layoff_lag3 +
    layoff_lag2 +
    layoff_lag1 +
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    layoff_lead3 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm3_common,
  cluster = ~gvkey
)

summary(m_buyback_common_pm3)


#separating differences between samples and lead/lag effects 

#original: original sample, same year model
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


#create lead/lag 1 sample
pm1_required_vars <- c(
  "is_buyback",
  "layoff_lag1",
  "is_layoff",
  "layoff_lead1",
  "lag_size",
  "lag_roa",
  "lag_leverage",
  "lag_cash_ratio",
  "lag_market_to_book",
  "gvkey",
  "fyear"
)

panel_pm1_common <- panel[
  complete.cases(panel[, ..pm1_required_vars])
]


#same year model on lead/lag 1 sample
m_buyback_common_pm1_same <- feols(
  is_buyback ~
    is_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm1_common,
  cluster = ~gvkey
)

summary(m_buyback_common_pm1_same)

#1 lead/lag model on lead/lag 1 sample
m_buyback_common_pm1_dynamic <- feols(
  is_buyback ~
    layoff_lag1 +
    is_layoff +
    layoff_lead1 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm1_common,
  cluster = ~gvkey
)

summary(m_buyback_common_pm1_dynamic)




#check differences between the samples
same_required_vars <- c(
  "is_buyback",
  "is_layoff",
  "lag_size",
  "lag_roa",
  "lag_leverage",
  "lag_cash_ratio",
  "lag_market_to_book",
  "gvkey",
  "fyear"
)

# Flag observations eligible for the same-year model
panel[
  ,
  eligible_same := complete.cases(panel[, ..same_required_vars])
]

# Flag observations eligible for the +/-1 dynamic model
panel[
  ,
  eligible_pm1 := complete.cases(panel[, ..pm1_required_vars])
]

# Check counts before FE singleton removal
panel[, .(
  same_eligible = sum(eligible_same),
  pm1_eligible  = sum(eligible_pm1),
  lost_for_pm1  = sum(eligible_same & !eligible_pm1)
)]


# Keep only observations that could enter the same-year model
sample_diagnostic <- panel[
  eligible_same == TRUE
]

# Group:
# retained = remains eligible when +/-1 information is required
# excluded = lost only because +/-1 information is unavailable
sample_diagnostic[
  ,
  pm1_status := fifelse(
    eligible_pm1,
    "Retained in +/-1 sample",
    "Excluded by +/-1 requirement"
  )
]



sample_comparison <- sample_diagnostic[
  ,
  .(
    n_obs = .N,
    n_firms = uniqueN(gvkey),
    
    layoff_rate = mean(is_layoff, na.rm = TRUE),
    buyback_rate = mean(is_buyback, na.rm = TRUE),
    
    both_rate = mean(
      is_layoff == 1 & is_buyback == 1,
      na.rm = TRUE
    ),
    
    mean_size = mean(lag_size, na.rm = TRUE),
    mean_roa = mean(lag_roa, na.rm = TRUE),
    mean_leverage = mean(lag_leverage, na.rm = TRUE),
    mean_cash_ratio = mean(lag_cash_ratio, na.rm = TRUE),
    mean_market_to_book = mean(lag_market_to_book, na.rm = TRUE)
  ),
  by = pm1_status
]

sample_comparison


buyback_by_layoff_status <- sample_diagnostic[
  ,
  .(
    n_obs = .N,
    buyback_rate = mean(is_buyback, na.rm = TRUE)
  ),
  by = .(
    pm1_status,
    is_layoff
  )
]

buyback_by_layoff_status



#exclusion by fiscal year

pm1_by_year <- sample_diagnostic[
  ,
  .(
    n_obs = .N,
    n_excluded = sum(!eligible_pm1),
    exclusion_rate = mean(!eligible_pm1)
  ),
  by = fyear
][
  order(fyear)
]

pm1_by_year[
  ,
  exclusion_rate := round(exclusion_rate, 4)
]

pm1_by_year


#check wether exlcuded observations are firms first or last observations

sample_diagnostic[
  ,
  `:=`(
    first_fyear = min(fyear),
    last_fyear  = max(fyear)
  ),
  by = gvkey
]

sample_diagnostic[
  ,
  `:=`(
    is_first_fyear = fyear == first_fyear,
    is_last_fyear  = fyear == last_fyear
  )
]

boundary_comparison <- sample_diagnostic[
  ,
  .(
    n_obs = .N,
    first_year_rate = mean(is_first_fyear),
    last_year_rate = mean(is_last_fyear),
    boundary_rate = mean(
      is_first_fyear | is_last_fyear
    )
  ),
  by = pm1_status
]

boundary_comparison



#check firms that disappear entirely 

firm_pm1_status <- sample_diagnostic[
  ,
  .(
    n_same_year_obs = .N,
    n_pm1_eligible = sum(eligible_pm1)
  ),
  by = gvkey
]

firm_pm1_status[
  ,
  lost_entirely := n_pm1_eligible == 0
]

firm_pm1_status[
  ,
  .(
    total_firms = .N,
    firms_lost_entirely = sum(lost_entirely),
    share_lost_entirely = mean(lost_entirely)
  )
]


#same year model no lead/lag with restricted sample 
#to see whether material changes are coming from global end points 
panel_2003_2024 <- panel[
  fyear >= 2003 &
    fyear <= 2024
]

m_buyback_same_controls_2003_2024 <- feols(
  is_buyback ~
    is_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_2003_2024,
  cluster = ~gvkey
)

summary(m_buyback_same_controls_2003_2024)

endpoint_comparison <- data.table(
  model = c(
    "Full sample",
    "2003-2024 only"
  ),
  
  estimate = c(
    coef(m_buyback_same_controls)["is_layoff"],
    coef(m_buyback_same_controls_2003_2024)["is_layoff"]
  ),
  
  std_error = c(
    se(m_buyback_same_controls)["is_layoff"],
    se(m_buyback_same_controls_2003_2024)["is_layoff"]
  ),
  
  p_value = c(
    pvalue(m_buyback_same_controls)["is_layoff"],
    pvalue(m_buyback_same_controls_2003_2024)["is_layoff"]
  ),
  
  n_obs = c(
    nobs(m_buyback_same_controls),
    nobs(m_buyback_same_controls_2003_2024)
  )
)

endpoint_comparison[
  ,
  `:=`(
    estimate = round(estimate, 6),
    std_error = round(std_error, 6),
    p_value = round(p_value, 4)
  )
]

endpoint_comparison


#one sided tests for all leads and lags separately 
#both on original sample (assymetric sample comparison) and on the same sample
# identified in the two sided tests

#lead 1 and lag 1 on original sample 
m_buyback_lead1_max <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

summary(m_buyback_lead1_max)

m_buyback_lag1_max <- feols(
  is_buyback ~
    layoff_lag1 +
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

summary(m_buyback_lag1_max)


# buyback lead 1 and lag 1 on the same sample
m_buyback_lead1_pm1common <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm1_common,
  cluster = ~gvkey
)

summary(m_buyback_lead1_pm1common)


m_buyback_lag1_pm1common <- feols(
  is_buyback ~
    layoff_lag1 +
    is_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm1_common,
  cluster = ~gvkey
)

summary(m_buyback_lag1_pm1common)



# lead 2 and lag 2 models on original sample 
m_buyback_lead2_max <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

summary(m_buyback_lead2_max)

m_buyback_lag2_max <- feols(
  is_buyback ~
    layoff_lag2 +
    layoff_lag1 +
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

summary(m_buyback_lag2_max)


#create common sample for lag/lead 2 observations 
pm2_required_vars <- c(
  "is_buyback",
  "layoff_lag2",
  "layoff_lag1",
  "is_layoff",
  "layoff_lead1",
  "layoff_lead2",
  "lag_size",
  "lag_roa",
  "lag_leverage",
  "lag_cash_ratio",
  "lag_market_to_book",
  "gvkey",
  "fyear"
)

panel_pm2_common <- panel[
  complete.cases(panel[, ..pm2_required_vars])
]

nrow(panel_pm2_common)


# lead 2 lag 2 models on the same sample 
m_buyback_lead2_pm2common <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm2_common,
  cluster = ~gvkey
)

summary(m_buyback_lead2_pm2common)

m_buyback_lag2_pm2common <- feols(
  is_buyback ~
    layoff_lag2 +
    layoff_lag1 +
    is_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm2_common,
  cluster = ~gvkey
)

summary(m_buyback_lag2_pm2common)



#lead 3 and lag 3 models on original sample 
m_buyback_lead3_max <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    layoff_lead3 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

summary(m_buyback_lead3_max)


m_buyback_lag3_max <- feols(
  is_buyback ~
    layoff_lag3 +
    layoff_lag2 +
    layoff_lag1 +
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

summary(m_buyback_lag3_max)


#lead 3 lag 3 on the same sample 
m_buyback_lead3_pm3common <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    layoff_lead3 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm3_common,
  cluster = ~gvkey
)

summary(m_buyback_lead3_pm3common)

m_buyback_lag3_pm3common <- feols(
  is_buyback ~
    layoff_lag3 +
    layoff_lag2 +
    layoff_lag1 +
    is_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_pm3_common,
  cluster = ~gvkey
)

summary(m_buyback_lag3_pm3common)



#actual event countst that satisfy lags and leads - on the original sample
lead2_max_vars <- c(
  "is_buyback",
  "is_layoff",
  "layoff_lead1",
  "layoff_lead2",
  "lag_size",
  "lag_roa",
  "lag_leverage",
  "lag_cash_ratio",
  "lag_market_to_book",
  "gvkey",
  "fyear"
)

lead2_max_sample <- panel[
  complete.cases(panel[, ..lead2_max_vars])
]

# Cross-tabulation
lead2_max_table <- lead2_max_sample[
  ,
  .N,
  by = .(
    is_buyback,
    layoff_lead2
  )
][
  order(layoff_lead2, is_buyback)
]

lead2_max_table


#conditional probabilities 
lead2_max_rates <- lead2_max_sample[
  ,
  .(
    n_obs = .N,
    n_buyback = sum(is_buyback == 1),
    buyback_rate = mean(is_buyback)
  ),
  by = layoff_lead2
]

lead2_max_rates


#actual event countst that satisfy lags and leads - on restricted sample 

lead2_pm2_table <- panel_pm2_common[
  ,
  .N,
  by = .(
    is_buyback,
    layoff_lead2
  )
][
  order(layoff_lead2, is_buyback)
]

lead2_pm2_table


lead2_pm2_rates <- panel_pm2_common[
  ,
  .(
    n_obs = .N,
    n_buyback = sum(is_buyback == 1),
    buyback_rate = mean(is_buyback)
  ),
  by = layoff_lead2
]

lead2_pm2_rates


#within firm variation for the counts 
lead2_event_firms <- lead2_max_sample[
  is_buyback == 1 &
    layoff_lead2 == 1,
  .(
    n_event_observations = .N,
    n_unique_firms = uniqueN(gvkey)
  )
]

lead2_event_firms

lead2_event_firms_pm2 <- panel_pm2_common[
  is_buyback == 1 &
    layoff_lead2 == 1,
  .(
    n_event_observations = .N,
    n_unique_firms = uniqueN(gvkey)
  )
]

lead2_event_firms_pm2



#lead2 combinations by fiscal year 
lead2_by_year <- lead2_max_sample[
  ,
  .(
    n_obs = .N,
    n_lead2_layoffs = sum(layoff_lead2 == 1),
    n_buybacks = sum(is_buyback == 1),
    
    n_buyback_and_lead2 = sum(
      is_buyback == 1 &
        layoff_lead2 == 1
    )
  ),
  by = fyear
][
  order(fyear)
]

lead2_by_year


# information on observations that were excluded when moving from full sample lead 2
#to restricted sample lead 2 specification
lead2_max_sample[
  ,
  in_pm2_common := complete.cases(
    layoff_lag1,
    layoff_lag2
  )
]

lead2_sample_cells <- lead2_max_sample[
  ,
  .N,
  by = .(
    sample_status = fifelse(
      in_pm2_common,
      "Retained in +/-2",
      "Excluded from +/-2"
    ),
    layoff_lead2,
    is_buyback
  )
][
  order(sample_status, layoff_lead2, is_buyback)
]

lead2_sample_cells

lead2_sample_rates <- lead2_max_sample[
  ,
  .(
    n_obs = .N,
    n_buybacks = sum(is_buyback),
    buyback_rate = mean(is_buyback)
  ),
  by = .(
    sample_status = fifelse(
      in_pm2_common,
      "Retained in +/-2",
      "Excluded from +/-2"
    ),
    layoff_lead2
  )
]

lead2_sample_rates



#checking characteristics of events unobservable by lead2 
lead2_observability_sample <- panel[
  eligible_same == TRUE
]

# Flag whether lead2 is observable
lead2_observability_sample[
  ,
  lead2_observable := !is.na(layoff_lead2)
]

# Basic counts
lead2_observability_sample[
  ,
  .(
    n_obs = .N,
    n_firms = uniqueN(gvkey)
  ),
  by = lead2_observable
]

lead2_observability_comparison <- lead2_observability_sample[
  ,
  .(
    n_obs = .N,
    n_firms = uniqueN(gvkey),
    
    layoff_rate = mean(is_layoff),
    buyback_rate = mean(is_buyback),
    
    both_rate = mean(
      is_layoff == 1 &
        is_buyback == 1
    ),
    
    mean_size = mean(lag_size),
    mean_roa = mean(lag_roa),
    mean_leverage = mean(lag_leverage),
    mean_cash_ratio = mean(lag_cash_ratio),
    mean_market_to_book = mean(lag_market_to_book)
  ),
  by = lead2_observable
]

lead2_observability_comparison


# First and last eligible same-year observation for each firm
lead2_observability_sample[
  ,
  `:=`(
    firm_first_fyear = min(fyear),
    firm_last_fyear  = max(fyear)
  ),
  by = gvkey
]

lead2_observability_sample[
  ,
  lead2_missing_reason := fcase(
    
    # Global right-censoring:
    # t+2 would fall outside the 2002-2025 study window
    !lead2_observable & fyear >= 2024,
    "Global right-censoring (2024-2025)",
    
    # Firm stops appearing before t+2 can be observed
    !lead2_observable &
      fyear < 2024 &
      firm_last_fyear < fyear + 2,
    "Firm-specific right boundary",
    
    # Anything else
    !lead2_observable,
    "Other / internal gap",
    
    # Lead2 exists
    default = "Lead2 observable"
  )
]

lead2_observability_sample[
  ,
  .(
    n_obs = .N,
    share = .N / nrow(lead2_observability_sample)
  ),
  by = lead2_missing_reason
][
  order(-n_obs)
]

#characteristics of missing firms
lead2_missing_comparison <- lead2_observability_sample[
  ,
  .(
    n_obs = .N,
    n_firms = uniqueN(gvkey),
    
    layoff_rate = mean(is_layoff),
    buyback_rate = mean(is_buyback),
    
    both_rate = mean(
      is_layoff == 1 &
        is_buyback == 1
    ),
    
    mean_size = mean(lag_size),
    mean_roa = mean(lag_roa),
    mean_leverage = mean(lag_leverage),
    mean_cash_ratio = mean(lag_cash_ratio),
    mean_market_to_book = mean(lag_market_to_book)
  ),
  by = lead2_missing_reason
]

lead2_missing_comparison



#year distribution of missing firms 
lead2_observability_by_year <- lead2_observability_sample[
  ,
  .(
    n_obs = .N,
    n_observable = sum(lead2_observable),
    n_unobservable = sum(!lead2_observable),
    observable_rate = mean(lead2_observable)
  ),
  by = fyear
][
  order(fyear)
]

lead2_observability_by_year[
  ,
  observable_rate := round(observable_rate, 4)
]

lead2_observability_by_year

#behaviour of missing firms
lead2_current_event_rates <- lead2_observability_sample[
  ,
  .(
    n_obs = .N,
    buyback_rate = mean(is_buyback),
    layoff_rate = mean(is_layoff)
  ),
  by = lead2_missing_reason
]

lead2_current_event_rates


#restricting sample to firms observable throughout the whole sample 
survivor_firms_2025 <- unique(
  panel[fyear == 2025, gvkey]
)

length(survivor_firms_2025)

panel_survivors_2025 <- panel[
  gvkey %in% survivor_firms_2025
]

m_buyback_lead2_survivors <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book
  | gvkey + fyear,
  data = panel_survivors_2025,
  cluster = ~gvkey
)

summary(m_buyback_lead2_survivors)




#final statistical tests

#multiple testing 
dynamic_terms <- c(
  "layoff_lag3",
  "layoff_lag2",
  "layoff_lag1",
  "is_layoff",
  "layoff_lead1",
  "layoff_lead2",
  "layoff_lead3"
)

# Extract coefficient table
ct_pm3 <- coeftable(m_buyback_common_pm3)

multiple_testing_all <- data.table(
  term = dynamic_terms,
  estimate = ct_pm3[dynamic_terms, "Estimate"],
  std_error = ct_pm3[dynamic_terms, "Std. Error"],
  p_raw = ct_pm3[dynamic_terms, "Pr(>|t|)"]
)

# Multiple-testing corrections
multiple_testing_all[
  ,
  `:=`(
    p_bonferroni = p.adjust(
      p_raw,
      method = "bonferroni"
    ),
    
    p_holm = p.adjust(
      p_raw,
      method = "holm"
    ),
    
    p_bh = p.adjust(
      p_raw,
      method = "BH"
    )
  )
]

multiple_testing_all[
  ,
  `:=`(
    estimate = round(estimate, 6),
    std_error = round(std_error, 6),
    p_raw = round(p_raw, 4),
    p_bonferroni = round(p_bonferroni, 4),
    p_holm = round(p_holm, 4),
    p_bh = round(p_bh, 4)
  )
]

multiple_testing_all



lead_terms <- c(
  "layoff_lead1",
  "layoff_lead2",
  "layoff_lead3"
)

multiple_testing_leads <- data.table(
  term = lead_terms,
  estimate = ct_pm3[lead_terms, "Estimate"],
  std_error = ct_pm3[lead_terms, "Std. Error"],
  p_raw = ct_pm3[lead_terms, "Pr(>|t|)"]
)

multiple_testing_leads[
  ,
  `:=`(
    p_bonferroni = p.adjust(
      p_raw,
      method = "bonferroni"
    ),
    
    p_holm = p.adjust(
      p_raw,
      method = "holm"
    ),
    
    p_bh = p.adjust(
      p_raw,
      method = "BH"
    )
  )
]

multiple_testing_leads[
  ,
  `:=`(
    estimate = round(estimate, 6),
    std_error = round(std_error, 6),
    p_raw = round(p_raw, 4),
    p_bonferroni = round(p_bonferroni, 4),
    p_holm = round(p_holm, 4),
    p_bh = round(p_bh, 4)
  )
]

multiple_testing_leads



lag_terms <- c(
  "layoff_lag3",
  "layoff_lag2",
  "layoff_lag1"
)

multiple_testing_lags <- data.table(
  term = lag_terms,
  estimate = ct_pm3[lag_terms, "Estimate"],
  std_error = ct_pm3[lag_terms, "Std. Error"],
  p_raw = ct_pm3[lag_terms, "Pr(>|t|)"]
)

multiple_testing_lags[
  ,
  `:=`(
    p_bonferroni = p.adjust(
      p_raw,
      method = "bonferroni"
    ),
    
    p_holm = p.adjust(
      p_raw,
      method = "holm"
    ),
    
    p_bh = p.adjust(
      p_raw,
      method = "BH"
    )
  )
]

multiple_testing_lags[
  ,
  `:=`(
    estimate = round(estimate, 6),
    std_error = round(std_error, 6),
    p_raw = round(p_raw, 4),
    p_bonferroni = round(p_bonferroni, 4),
    p_holm = round(p_holm, 4),
    p_bh = round(p_bh, 4)
  )
]

multiple_testing_lags


#wald f test
wald_all_dynamic <- wald(
  m_buyback_common_pm3,
  keep = "layoff_lag|is_layoff|layoff_lead"
)

wald_all_dynamic

wald_future_leads <- wald(
  m_buyback_common_pm3,
  keep = "layoff_lead"
)

wald_future_leads

wald_past_lags <- wald(
  m_buyback_common_pm3,
  keep = "layoff_lag"
)

wald_past_lags



wald_lead2_lead3 <- wald(
  m_buyback_common_pm3,
  keep = "layoff_lead2|layoff_lead3"
)

wald_lead2_lead3



#is lead 2 significantly different from lead3?
b <- coef(m_buyback_common_pm3)
V <- vcov(m_buyback_common_pm3)

# Difference between coefficients
diff_lead2_lead3 <-
  b["layoff_lead2"] -
  b["layoff_lead3"]

# Standard error of the difference:
# Var(b2 - b3) = Var(b2) + Var(b3) - 2Cov(b2,b3)

se_diff_lead2_lead3 <- sqrt(
  V["layoff_lead2", "layoff_lead2"] +
    V["layoff_lead3", "layoff_lead3"] -
    2 * V["layoff_lead2", "layoff_lead3"]
)

# t statistic
t_lead2_lead3 <-
  diff_lead2_lead3 /
  se_diff_lead2_lead3

# Two-sided p-value
p_lead2_lead3 <- 2 * pt(
  abs(t_lead2_lead3),
  df = 3159,
  lower.tail = FALSE
)

lead2_lead3_equality <- data.table(
  lead2 = b["layoff_lead2"],
  lead3 = b["layoff_lead3"],
  difference = diff_lead2_lead3,
  se_difference = se_diff_lead2_lead3,
  t_value = t_lead2_lead3,
  p_value = p_lead2_lead3
)

lead2_lead3_equality