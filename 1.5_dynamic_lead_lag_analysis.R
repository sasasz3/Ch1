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
