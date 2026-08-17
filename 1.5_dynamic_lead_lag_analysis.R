library(data.table)
library(readxl)
library(fixest)

source("config.R")
source("reusable_functions.R")
source("1.4_controls.R")

panel[, gvkey := as.character(gvkey)]
panel[, fyear := as.integer(fyear)] 

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


#audit
panel[
  ,
  .(
    lag1_observed  = sum(!is.na(layoff_lag1)),
    lag2_observed  = sum(!is.na(layoff_lag2)),
    lag3_observed  = sum(!is.na(layoff_lag3)),
    lead1_observed = sum(!is.na(layoff_lead1)),
    lead2_observed = sum(!is.na(layoff_lead2)),
    lead3_observed = sum(!is.na(layoff_lead3))
  )
]



#descriptive stats for dynamic variables 
dynamic_vars <- c(
  "layoff_lag3",
  "layoff_lag2",
  "layoff_lag1",
  "is_layoff",
  "layoff_lead1",
  "layoff_lead2",
  "layoff_lead3",
  "buyback_lag3",
  "buyback_lag2",
  "buyback_lag1",
  "is_buyback",
  "buyback_lead1",
  "buyback_lead2",
  "buyback_lead3"
)

dynamic_descriptives <- rbindlist(
  lapply(
    dynamic_vars,
    function(v) {
      
      x <- panel[[v]]
      
      data.table(
        variable = v,
        
        n_observable = sum(!is.na(x)),
        
        n_firms_observable = uniqueN(
          panel$gvkey[!is.na(x)]
        ),
        
        n_event = sum(
          x == 1,
          na.rm = TRUE
        ),
        
        n_event_firms = uniqueN(
          panel$gvkey[
            !is.na(x) &
              x == 1
          ]
        ),
        
        n_no_event = sum(
          x == 0,
          na.rm = TRUE
        ),
        
        event_rate = mean(
          x,
          na.rm = TRUE
        )
      )
    }
  )
)

dynamic_descriptives




#raw dynamics relationships
relative_layoff_vars <- c(
  "layoff_lag3",
  "layoff_lag2",
  "layoff_lag1",
  "is_layoff",
  "layoff_lead1",
  "layoff_lead2",
  "layoff_lead3"
)

relative_years <- c(
  -3, -2, -1, 0, 1, 2, 3
)

raw_dynamic_relationship <- rbindlist(
  lapply(
    seq_along(relative_layoff_vars),
    function(j) {
      
      v <- relative_layoff_vars[j]
      
      panel[
        !is.na(get(v)),
        .(
          n_obs = .N,
          
          n_firms = uniqueN(gvkey),
          
          n_buybacks = sum(
            is_buyback == 1
          ),
          
          n_buyback_firms = uniqueN(
            gvkey[is_buyback == 1]
          ),
          
          buyback_rate = mean(
            is_buyback
          )
        ),
        by = .(
          layoff_status = get(v)
        )
      ][
        ,
        relative_year := relative_years[j]
      ]
    }
  )
)

setcolorder(
  raw_dynamic_relationship,
  c(
    "relative_year",
    "layoff_status",
    "n_obs",
    "n_firms",
    "n_buybacks",
    "n_buyback_firms",
    "buyback_rate"
  )
)

setorder(
  raw_dynamic_relationship,
  relative_year,
  layoff_status
)

raw_dynamic_relationship

raw_dynamic_rates <- dcast(
  raw_dynamic_relationship,
  relative_year ~ layoff_status,
  value.var = "buyback_rate"
)

setnames(
  raw_dynamic_rates,
  c("0", "1"),
  c(
    "buyback_rate_no_layoff",
    "buyback_rate_layoff"
  )
)

raw_dynamic_rates[
  ,
  `:=`(
    difference =
      buyback_rate_layoff -
      buyback_rate_no_layoff,
    
    difference_pp =
      100 * (
        buyback_rate_layoff -
          buyback_rate_no_layoff
      )
  )
]

raw_dynamic_rates





#layoff correlation matrix
layoff_dynamic_vars <- c(
  "layoff_lag3",
  "layoff_lag2",
  "layoff_lag1",
  "is_layoff",
  "layoff_lead1",
  "layoff_lead2",
  "layoff_lead3"
)

layoff_correlations <- cor(
  panel[
    ,
    ..layoff_dynamic_vars
  ],
  use = "pairwise.complete.obs"
)

round(
  layoff_correlations,
  3
)


lead2_lead3_overlap <- panel[
  !is.na(layoff_lead2) &
    !is.na(layoff_lead3),
  .(
    n_obs = .N,
    n_firms = uniqueN(gvkey)
  ),
  by = .(
    layoff_lead2,
    layoff_lead3
  )
][
  order(
    layoff_lead2,
    layoff_lead3
  )
]

lead2_lead3_overlap[
  ,
  pct_obs := n_obs / sum(n_obs)
]

lead2_lead3_overlap

panel[
  layoff_lead2 == 1 &
    !is.na(layoff_lead3),
  .(
    lead2_events = .N,
    also_lead3 = sum(layoff_lead3 == 1),
    pct_also_lead3 = mean(layoff_lead3 == 1),
    firms = uniqueN(gvkey)
  )
]

panel[
  layoff_lead3 == 1 &
    !is.na(layoff_lead2),
  .(
    lead3_events = .N,
    also_lead2 = sum(layoff_lead2 == 1),
    pct_also_lead2 = mean(layoff_lead2 == 1),
    firms = uniqueN(gvkey)
  )
]


#baseline regressions
m_same_base <- feols(
  is_buyback ~
    is_layoff,
  data = panel,
  cluster = ~gvkey
)


m_pm1_base <- feols(
  is_buyback ~
    layoff_lag1 +
    is_layoff +
    layoff_lead1,
  data = panel,
  cluster = ~gvkey
)

m_pm2_base <- feols(
  is_buyback ~
    layoff_lag2 +
    layoff_lag1 +
    is_layoff +
    layoff_lead1 +
    layoff_lead2,
  data = panel,
  cluster = ~gvkey
)


m_pm3_base <- feols(
  is_buyback ~
    layoff_lag3 +
    layoff_lag2 +
    layoff_lag1 +
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    layoff_lead3,
  data = panel,
  cluster = ~gvkey
)


m_lag1_base <- feols(
  is_buyback ~
    layoff_lag1 +
    is_layoff,
  data = panel,
  cluster = ~gvkey
)


m_lag12_base <- feols(
  is_buyback ~
    layoff_lag2 +
    layoff_lag1 +
    is_layoff,
  data = panel,
  cluster = ~gvkey
)


m_lag123_base <- feols(
  is_buyback ~
    layoff_lag3 +
    layoff_lag2 +
    layoff_lag1 +
    is_layoff,
  data = panel,
  cluster = ~gvkey
)

m_lead1_base <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1,
  data = panel,
  cluster = ~gvkey
)

m_lead12_base <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    layoff_lead2,
  data = panel,
  cluster = ~gvkey
)

m_lead123_base <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    layoff_lead3,
  data = panel,
  cluster = ~gvkey
)


etable(
  m_same_base,
  m_pm1_base,
  m_pm2_base,
  m_pm3_base,
  m_lag1_base,
  m_lag12_base,
  m_lag123_base,
  m_lead1_base,
  m_lead12_base,
  m_lead123_base,
  
  keep_raw = "layoff",
  
  dict = c(
    "is_layoff" = "Layoff t",
    "layoff_lag1" = "Layoff t-1",
    "layoff_lag2" = "Layoff t-2",
    "layoff_lag3" = "Layoff t-3",
    "layoff_lead1" = "Layoff t+1",
    "layoff_lead2" = "Layoff t+2",
    "layoff_lead3" = "Layoff t+3"
  ),
  
  headers = c(
    "Same year",
    "+/- 1",
    "+/- 2",
    "+/- 3",
    "Lag 1",
    "Lags 1-2",
    "Lags 1-3",
    "Lead 1",
    "Leads 1-2",
    "Leads 1-3"
  )
)



#baseline regression with fixed effects 

m_same_fe <- feols(
  is_buyback ~
    is_layoff
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

m_pm1_fe <- feols(
  is_buyback ~
    layoff_lag1 +
    is_layoff +
    layoff_lead1
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

m_pm2_fe <- feols(
  is_buyback ~
    layoff_lag2 +
    layoff_lag1 +
    is_layoff +
    layoff_lead1 +
    layoff_lead2
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

m_pm3_fe <- feols(
  is_buyback ~
    layoff_lag3 +
    layoff_lag2 +
    layoff_lag1 +
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    layoff_lead3
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

m_lag1_fe <- feols(
  is_buyback ~
    layoff_lag1 +
    is_layoff
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)


m_lag12_fe <- feols(
  is_buyback ~
    layoff_lag2 +
    layoff_lag1 +
    is_layoff
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)


m_lag123_fe <- feols(
  is_buyback ~
    layoff_lag3 +
    layoff_lag2 +
    layoff_lag1 +
    is_layoff
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

m_lead1_fe <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)


m_lead12_fe <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    layoff_lead2
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

m_lead123_fe <- feols(
  is_buyback ~
    is_layoff +
    layoff_lead1 +
    layoff_lead2 +
    layoff_lead3
  | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)

etable(
  m_same_fe,
  m_pm1_fe,
  m_pm2_fe,
  m_pm3_fe,
  m_lag1_fe,
  m_lag12_fe,
  m_lag123_fe,
  m_lead1_fe,
  m_lead12_fe,
  m_lead123_fe,
  
  keep_raw = "layoff",
  
  dict = c(
    "is_layoff" = "Layoff t",
    "layoff_lag1" = "Layoff t-1",
    "layoff_lag2" = "Layoff t-2",
    "layoff_lag3" = "Layoff t-3",
    "layoff_lead1" = "Layoff t+1",
    "layoff_lead2" = "Layoff t+2",
    "layoff_lead3" = "Layoff t+3"
  ),
  
  headers = c(
    "Same year",
    "+/- 1",
    "+/- 2",
    "+/- 3",
    "Lag 1",
    "Lags 1-2",
    "Lags 1-3",
    "Lead 1",
    "Leads 1-2",
    "Leads 1-3"
  )
)





#regression with controls 
m_same <- feols(
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

m_pm1 <- feols(
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
  data = panel,
  cluster = ~gvkey
)


m_pm2 <- feols(
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
  data = panel,
  cluster = ~gvkey
)


m_pm3 <- feols(
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
  data = panel,
  cluster = ~gvkey
)


#lag only models
m_lag1 <- feols(
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

m_lag12 <- feols(
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


m_lag123 <- feols(
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



#lead only models
m_lead1 <- feols(
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

m_lead12 <- feols(
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



m_lead123 <- feols(
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



#create output
etable(
  m_same,
  m_pm1,
  m_pm2,
  m_pm3,
  m_lag1,
  m_lag12,
  m_lag123,
  m_lead1,
  m_lead12,
  m_lead123,
  
  keep_raw = "layoff",
  
  dict = c(
    "is_layoff" = "Layoff t",
    "layoff_lag1" = "Layoff t-1",
    "layoff_lag2" = "Layoff t-2",
    "layoff_lag3" = "Layoff t-3",
    "layoff_lead1" = "Layoff t+1",
    "layoff_lead2" = "Layoff t+2",
    "layoff_lead3" = "Layoff t+3"
  ),
  
  headers = c(
    "Same year",
    "+/- 1",
    "+/- 2",
    "+/- 3",
    "Lag 1",
    "Lags 1-2",
    "Lags 1-3",
    "Lead 1",
    "Leads 1-2",
    "Leads 1-3"
  )
)


#create controls output
etable(
  m_same,
  m_pm1,
  m_pm2,
  m_pm3,
  m_lag1,
  m_lag12,
  m_lag123,
  m_lead1,
  m_lead12,
  m_lead123,
  
  keep_raw = c(
    "lag_size",
    "lag_roa",
    "lag_leverage",
    "lag_cash_ratio",
    "lag_market_to_book"
  ),
  
  dict = c(
    "lag_size" = "Firm size t-1",
    "lag_roa" = "ROA t-1",
    "lag_leverage" = "Leverage t-1",
    "lag_cash_ratio" = "Cash ratio t-1",
    "lag_market_to_book" = "Market-to-book t-1"
  ),
  
  headers = c(
    "Same year",
    "+/- 1",
    "+/- 2",
    "+/- 3",
    "Lag 1",
    "Lags 1-2",
    "Lags 1-3",
    "Lead 1",
    "Leads 1-2",
    "Leads 1-3"
  )
)






