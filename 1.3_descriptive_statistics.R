library(data.table)
library(fixest)

source("config.R")
source("reusable_functions.R")
source("1.2_controls.R")




panel[
  ,
  gvkey := as.character(gvkey)
]

setorder(
  panel,
  gvkey,
  fyear
)

#sample audit
firm_years <- nrow(panel)

n_firms <- uniqueN(
  panel$gvkey
)

year_start <- min(
  panel$fyear,
  na.rm = TRUE
)

year_end <- max(
  panel$fyear,
  na.rm = TRUE
)

event_firm_years <- panel[
  is_layoff == 1 |
    is_buyback == 1,
  .N
]

sample_audit <- data.table(
  firm_years = firm_years,
  firms = n_firms,
  first_fyear = year_start,
  last_fyear = year_end,
  event_firm_years = event_firm_years
)

sample_audit


#event counts
event_counts <- panel[
  ,
  .(
    firm_years = .N,
    
    layoff_firm_years =
      sum(
        is_layoff == 1,
        na.rm = TRUE
      ),
    
    buyback_firm_years =
      sum(
        is_buyback == 1,
        na.rm = TRUE
      ),
    
    both_firm_years =
      sum(
        is_layoff == 1 &
          is_buyback == 1,
        na.rm = TRUE
      ),
    
    layoff_only =
      sum(
        is_layoff == 1 &
          is_buyback == 0,
        na.rm = TRUE
      ),
    
    buyback_only =
      sum(
        is_layoff == 0 &
          is_buyback == 1,
        na.rm = TRUE
      ),
    
    neither =
      sum(
        is_layoff == 0 &
          is_buyback == 0,
        na.rm = TRUE
      )
  )
]

event_counts


event_rates <- panel[
  ,
  .(
    layoff_rate =
      mean(
        is_layoff,
        na.rm = TRUE
      ),
    
    buyback_rate =
      mean(
        is_buyback,
        na.rm = TRUE
      ),
    
    both_rate =
      mean(
        is_layoff == 1 &
          is_buyback == 1,
        na.rm = TRUE
      ),
    
    layoff_only_rate =
      mean(
        is_layoff == 1 &
          is_buyback == 0,
        na.rm = TRUE
      ),
    
    buyback_only_rate =
      mean(
        is_layoff == 0 &
          is_buyback == 1,
        na.rm = TRUE
      ),
    
    neither_rate =
      mean(
        is_layoff == 0 &
          is_buyback == 0,
        na.rm = TRUE
      )
  )
]

event_rates


#fimr level composition
firm_summary <- panel[
  ,
  .(
    observed_years = .N,
    
    layoff_years =
      sum(
        is_layoff,
        na.rm = TRUE
      ),
    
    buyback_years =
      sum(
        is_buyback,
        na.rm = TRUE
      ),
    
    both_years =
      sum(
        is_layoff == 1 &
          is_buyback == 1,
        na.rm = TRUE
      ),
    
    ever_layoff =
      any(
        is_layoff == 1,
        na.rm = TRUE
      ),
    
    ever_buyback =
      any(
        is_buyback == 1,
        na.rm = TRUE
      ),
    
    layoff_share =
      mean(
        is_layoff,
        na.rm = TRUE
      ),
    
    buyback_share =
      mean(
        is_buyback,
        na.rm = TRUE
      )
  ),
  by = gvkey
]


firm_types <- firm_summary[
  ,
  .N,
  by = .(
    ever_layoff,
    ever_buyback
  )
]

firm_types[
  ,
  percent :=
    100 * N / sum(N)
]

firm_types


#event frequencies
firm_event_descriptives <- rbindlist(
  lapply(
    c(
      "observed_years",
      "layoff_years",
      "buyback_years",
      "both_years",
      "layoff_share",
      "buyback_share"
    ),
    function(v) {
      
      x <- firm_summary[[v]]
      
      data.table(
        variable = v,
        
        n =
          sum(
            !is.na(x)
          ),
        
        mean =
          mean(
            x,
            na.rm = TRUE
          ),
        
        sd =
          sd(
            x,
            na.rm = TRUE
          ),
        
        min =
          min(
            x,
            na.rm = TRUE
          ),
        
        p25 =
          quantile(
            x,
            0.25,
            na.rm = TRUE
          ),
        
        median =
          median(
            x,
            na.rm = TRUE
          ),
        
        p75 =
          quantile(
            x,
            0.75,
            na.rm = TRUE
          ),
        
        max =
          max(
            x,
            na.rm = TRUE
          )
      )
    }
  )
)

firm_event_descriptives



#regression variables descriptive statistics
regression_vars <- c(
  "is_layoff",
  "is_buyback",
  "lag_size",
  "lag_roa",
  "lag_leverage",
  "lag_cash_ratio",
  "lag_market_to_book"
)

regression_descriptives <- rbindlist(
  lapply(
    regression_vars,
    function(v) {
      
      x <- panel[[v]]
      
      data.table(
        variable = v,
        
        n =
          sum(
            !is.na(x)
          ),
        
        mean =
          mean(
            x,
            na.rm = TRUE
          ),
        
        sd =
          sd(
            x,
            na.rm = TRUE
          ),
        
        min =
          min(
            x,
            na.rm = TRUE
          ),
        
        p25 =
          quantile(
            x,
            0.25,
            na.rm = TRUE
          ),
        
        median =
          median(
            x,
            na.rm = TRUE
          ),
        
        p75 =
          quantile(
            x,
            0.75,
            na.rm = TRUE
          ),
        
        max =
          max(
            x,
            na.rm = TRUE
          )
      )
    }
  )
)

regression_descriptives


#fiscal year trends
year_summary <- panel[
  ,
  .(
    firms =
      uniqueN(
        gvkey
      ),
    
    firm_years =
      .N,
    
    layoff_firm_years =
      sum(
        is_layoff,
        na.rm = TRUE
      ),
    
    buyback_firm_years =
      sum(
        is_buyback,
        na.rm = TRUE
      ),
    
    both_firm_years =
      sum(
        is_layoff == 1 &
          is_buyback == 1,
        na.rm = TRUE
      ),
    
    layoff_rate =
      mean(
        is_layoff,
        na.rm = TRUE
      ),
    
    buyback_rate =
      mean(
        is_buyback,
        na.rm = TRUE
      ),
    
    both_rate =
      mean(
        is_layoff == 1 &
          is_buyback == 1,
        na.rm = TRUE
      )
  ),
  by = fyear
]

setorder(
  year_summary,
  fyear
)

year_summary


#within firm variation
variation_summary <- panel[
  ,
  .(
    layoff_variation =
      uniqueN(
        is_layoff[
          !is.na(is_layoff)
        ]
      ) > 1,
    
    buyback_variation =
      uniqueN(
        is_buyback[
          !is.na(is_buyback)
        ]
      ) > 1
  ),
  by = gvkey
]


variation_table <- variation_summary[
  ,
  .(
    firms = .N,
    
    variation_layoff =
      sum(
        layoff_variation
      ),
    
    variation_buyback =
      sum(
        buyback_variation
      ),
    
    variation_both =
      sum(
        layoff_variation &
          buyback_variation
      ),
    
    pct_variation_layoff =
      mean(
        layoff_variation
      ),
    
    pct_variation_buyback =
      mean(
        buyback_variation
      ),
    
    pct_variation_both =
      mean(
        layoff_variation &
          buyback_variation
      )
  )
]

variation_table



#within firm event dispersion
within_firm_descriptives <- panel[
  ,
  .(
    layoff_mean =
      mean(
        is_layoff,
        na.rm = TRUE
      ),
    
    layoff_sd =
      sd(
        is_layoff,
        na.rm = TRUE
      ),
    
    buyback_mean =
      mean(
        is_buyback,
        na.rm = TRUE
      ),
    
    buyback_sd =
      sd(
        is_buyback,
        na.rm = TRUE
      )
  ),
  by = gvkey
][
  ,
  .(
    mean_within_layoff_sd =
      mean(
        layoff_sd,
        na.rm = TRUE
      ),
    
    median_within_layoff_sd =
      median(
        layoff_sd,
        na.rm = TRUE
      ),
    
    mean_within_buyback_sd =
      mean(
        buyback_sd,
        na.rm = TRUE
      ),
    
    median_within_buyback_sd =
      median(
        buyback_sd,
        na.rm = TRUE
      )
  )
]

within_firm_descriptives


#event persistence
event_panel <- panel[
  ,
  .(
    gvkey,
    fyear,
    is_layoff,
    is_buyback
  )
]


event_lag1 <- event_panel[
  ,
  .(
    gvkey,
    fyear = fyear + 1,
    lag_layoff = is_layoff,
    lag_buyback = is_buyback
  )
]


panel[
  event_lag1,
  on = .(
    gvkey,
    fyear
  ),
  `:=`(
    lag_layoff = i.lag_layoff,
    lag_buyback = i.lag_buyback
  )
]


persistence_summary <- panel[
  !is.na(lag_layoff) &
    !is.na(lag_buyback),
  .(
    P_layoff_given_previous_layoff =
      mean(
        is_layoff[
          lag_layoff == 1
        ],
        na.rm = TRUE
      ),
    
    P_layoff_given_previous_no_layoff =
      mean(
        is_layoff[
          lag_layoff == 0
        ],
        na.rm = TRUE
      ),
    
    P_buyback_given_previous_buyback =
      mean(
        is_buyback[
          lag_buyback == 1
        ],
        na.rm = TRUE
      ),
    
    P_buyback_given_previous_no_buyback =
      mean(
        is_buyback[
          lag_buyback == 0
        ],
        na.rm = TRUE
      ),
    
    P_buyback_given_previous_layoff =
      mean(
        is_buyback[
          lag_layoff == 1
        ],
        na.rm = TRUE
      ),
    
    P_buyback_given_previous_no_layoff =
      mean(
        is_buyback[
          lag_layoff == 0
        ],
        na.rm = TRUE
      ),
    
    P_layoff_given_previous_buyback =
      mean(
        is_layoff[
          lag_buyback == 1
        ],
        na.rm = TRUE
      ),
    
    P_layoff_given_previous_no_buyback =
      mean(
        is_layoff[
          lag_buyback == 0
        ],
        na.rm = TRUE
      )
  )
]

persistence_summary


