library(data.table)
library(fixest)

source("config.R")

# Load final analysis panel
panel <- readRDS(
  file.path(DATA_FOLDER, "firm_fyear_panel.rds")
)

setDT(panel)

# Full constructed panel - retain for auditing
panel_full <- copy(panel)


# Main analysis sample
panel <- panel[
  fyear >= 2002 &
    fyear <= 2025
]


#remove firms that were dropped because of the fiscal year restriction
#and now feed into the data as 0-0s
non_event_firms_restricted <- panel[
  ,
  .(
    ever_event = any(
      is_layoff == 1 |
        is_buyback == 1
    )
  ),
  by = gvkey
][
  ever_event == FALSE,
  gvkey
]

# Remove them
panel <- panel[
  !gvkey %in% non_event_firms_restricted
]

#audits
firm_years <- nrow(panel)
all_firms <- unique(panel$gvkey)


#calendar borders - do not align with actual time horizon
year_start <- min(panel$fyear, na.rm = TRUE)
year_end <- max(panel$fyear, na.rm = TRUE)


event_firm_years <- panel[is_layoff == 1 | is_buyback == 1, .N] 


#count frequencies 
event_counts <- panel[
  ,
  .(
    firm_years = .N,
    
    layoff_firm_years =
      sum(is_layoff == 1),
    
    buyback_firm_years =
      sum(is_buyback == 1),
    
    both_firm_years =
      sum(is_layoff == 1 &
            is_buyback == 1),
    
    layoff_only =
      sum(is_layoff == 1 &
            is_buyback == 0),
    
    buyback_only =
      sum(is_layoff == 0 &
            is_buyback == 1),
    
    neither =
      sum(is_layoff == 0 &
            is_buyback == 0)
  )
]

print(event_counts)


#count event rates

event_rates <- panel[
  ,
  .(
    layoff_rate = mean(is_layoff),
    buyback_rate = mean(is_buyback),
    
    both_rate =
      mean(is_layoff == 1 &
             is_buyback == 1),
    
    layoff_only_rate =
      mean(is_layoff == 1 &
             is_buyback == 0),
    
    buyback_only_rate =
      mean(is_layoff == 0 &
             is_buyback == 1),
    
    neither_rate =
      mean(is_layoff == 0 &
             is_buyback == 0)
  )
]

print(event_rates)




# buyback layoff cooccurance calculation 
p_buyback <- mean(panel$is_buyback)

p_layoff <- mean(panel$is_layoff)

p_buyback_given_layoff <- panel[
  is_layoff == 1,
  mean(is_buyback)
]

p_buyback_given_no_layoff <- panel[
  is_layoff == 0,
  mean(is_buyback)
]

p_layoff_given_buyback <- panel[
  is_buyback == 1,
  mean(is_layoff)
]

p_layoff_given_no_buyback <- panel[
  is_buyback == 0,
  mean(is_layoff)
]

cat("P(Buyback):", p_buyback, "\n")
cat("P(Layoff):", p_layoff, "\n")

cat(
  "P(Buyback | Layoff):",
  p_buyback_given_layoff,
  "\n"
)

cat(
  "P(Buyback | No Layoff):",
  p_buyback_given_no_layoff,
  "\n"
)

cat(
  "P(Layoff | Buyback):",
  p_layoff_given_buyback,
  "\n"
)

cat(
  "P(Layoff | No Buyback):",
  p_layoff_given_no_buyback,
  "\n"
)



#observed vs expected probabilities
observed_joint_probability <- panel[
  ,
  mean(
    is_layoff == 1 &
      is_buyback == 1
  )
]

expected_joint_independence <-
  p_layoff * p_buyback

overlap_ratio <-
  observed_joint_probability /
  expected_joint_independence

cat(
  "Observed joint probability:",
  observed_joint_probability,
  "\n"
)

cat(
  "Expected joint probability under independence:",
  expected_joint_independence,
  "\n"
)

cat(
  "Observed / expected overlap:",
  overlap_ratio,
  "\n"
)


# calculate absolute and relative probabilities 
buyback_probability_difference <-
  p_buyback_given_layoff -
  p_buyback_given_no_layoff

buyback_probability_ratio <-
  p_buyback_given_layoff /
  p_buyback_given_no_layoff

cat(
  "Buyback probability difference:",
  buyback_probability_difference,
  "\n"
)

cat(
  "Buyback probability ratio:",
  buyback_probability_ratio,
  "\n"
)


# firm-level composition 
firm_summary <- panel[
  ,
  .(
    observed_years = .N,
    
    layoff_years =
      sum(is_layoff),
    
    buyback_years =
      sum(is_buyback),
    
    both_years =
      sum(
        is_layoff == 1 &
          is_buyback == 1
      ),
    
    ever_layoff =
      any(is_layoff == 1),
    
    ever_buyback =
      any(is_buyback == 1)
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
  percent := 100 * N / sum(N)
]

print(firm_types)

#event years
summary(
  firm_summary$layoff_years
)

summary(
  firm_summary$buyback_years
)

summary(
  firm_summary$both_years
)


#time trends
year_summary <- panel[
  ,
  .(
    firms = uniqueN(gvkey),
    firm_years = .N,
    
    layoff_firm_years =
      sum(is_layoff),
    
    buyback_firm_years =
      sum(is_buyback),
    
    both_firm_years =
      sum(
        is_layoff == 1 &
          is_buyback == 1
      ),
    
    layoff_rate =
      mean(is_layoff),
    
    buyback_rate =
      mean(is_buyback),
    
    both_rate =
      mean(
        is_layoff == 1 &
          is_buyback == 1
      )
  ),
  by = fyear
]

setorder(year_summary, fyear)

print(year_summary)



#within firm variation
variation_summary <- panel[
  ,
  .(
    layoff_variation =
      uniqueN(is_layoff) > 1,
    
    buyback_variation =
      uniqueN(is_buyback) > 1
  ),
  by = gvkey
]

variation_summary[
  ,
  .(
    firms = .N,
    
    variation_layoff =
      sum(layoff_variation),
    
    variation_buyback =
      sum(buyback_variation),
    
    variation_both =
      sum(
        layoff_variation &
          buyback_variation
      )
  )
]

#persistence
setorder(panel, gvkey, fyear)

panel[
  ,
  lag_layoff := shift(is_layoff),
  by = gvkey
]

panel[
  ,
  lag_buyback := shift(is_buyback),
  by = gvkey
]

panel[
  ,
  lag_fyear := shift(fyear),
  by = gvkey
]

panel[
  ,
  consecutive_year :=
    fyear - lag_fyear == 1
]

panel[
  consecutive_year == TRUE,
  .(
    P_layoff_given_previous_layoff =
      mean(is_layoff[lag_layoff == 1]),
    
    P_layoff_given_previous_no_layoff =
      mean(is_layoff[lag_layoff == 0]),
    
    P_buyback_given_previous_buyback =
      mean(is_buyback[lag_buyback == 1]),
    
    P_buyback_given_previous_no_buyback =
      mean(is_buyback[lag_buyback == 0]),
    
    P_buyback_given_previous_layoff =
      mean(is_buyback[lag_layoff == 1]),
    
    P_buyback_given_previous_no_layoff =
      mean(is_buyback[lag_layoff == 0]),
    
    # Cross-event: previous buyback -> current layoff
    P_layoff_given_previous_buyback =
      mean(is_layoff[lag_buyback == 1]),
    
    P_layoff_given_previous_no_buyback =
      mean(is_layoff[lag_buyback == 0])
  )
]

table(
  Layoff = panel$is_layoff,
  Buyback = panel$is_buyback
)

prop.table(
  table(
    Layoff = panel$is_layoff,
    Buyback = panel$is_buyback
  )
)





