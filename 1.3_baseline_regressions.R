library(data.table)
library(fixest)

source("config.R")

panel <- readRDS( file.path(DATA_FOLDER, "firm_fyear_panel.rds"))
setDT(panel)

panel[, gvkey := as.character(gvkey)]


panel <- panel[
  fyear >= 2002 &
    fyear <= 2025
]

setorder(panel, gvkey, fyear)

#create lag variables
panel[,previous_fyear := shift(fyear), by = gvkey]
panel[, consecutive_year := !is.na(previous_fyear) &  fyear == previous_fyear + 1]

# Lagged layoff
panel[,lag_layoff := shift(is_layoff), by = gvkey]

# Lagged buyback
panel[,lag_buyback := shift(is_buyback),by = gvkey]

# Do not treat non-consecutive observations as valid lags
panel[
  consecutive_year == FALSE,
  `:=`(
    lag_layoff = NA_integer_,
    lag_buyback = NA_integer_
  )
]



#SAME YEAR EVENTS
m_buyback_same <- feols(
  is_buyback ~ is_layoff,
  data = panel,
  cluster = ~gvkey
)

summary(m_buyback_same)


m_layoff_same <- feols(
  is_layoff ~ is_buyback,
  data = panel,
  cluster = ~gvkey
)

summary(m_layoff_same)


#LAGGED YEAR EVENTS
m_buyback_lag <- feols(
  is_buyback ~ lag_layoff,
  data = panel,
  cluster = ~gvkey
)

summary(m_buyback_lag)


m_layoff_lag <- feols(
  is_layoff ~ lag_buyback,
  data = panel,
  cluster = ~gvkey
)
summary(m_layoff_lag)

#SAME YEAR EVENTS WITH YEAR FIXED EFFECT
m_buyback_same_yearfe <- feols(
  is_buyback ~ is_layoff | fyear,
  data = panel,
  cluster = ~gvkey
)
summary(m_buyback_same_yearfe)

m_layoff_same_yearfe <- feols(
  is_layoff ~ is_buyback | fyear,
  data = panel,
  cluster = ~gvkey
)
summary(m_layoff_same_yearfe)


#SAME YEAR EVENTS WITH FIRM AND YEAR FE
m_buyback_same_twfe <- feols(
  is_buyback ~ is_layoff | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)
summary(m_buyback_same_twfe
        )
m_layoff_same_twfe <- feols(
  is_layoff ~ is_buyback | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)
summary(m_layoff_same_twfe)

#LAGGED EVENTS WITH YEAR FE
m_buyback_lag_yearfe <- feols(
  is_buyback ~ lag_layoff | fyear,
  data = panel,
  cluster = ~gvkey
)
summary(m_buyback_lag_yearfe)

m_layoff_lag_yearfe <- feols(
  is_layoff ~ lag_buyback | fyear,
  data = panel,
  cluster = ~gvkey
)
summary(m_layoff_lag_yearfe)

#LAGGED EVENTS WITH FIRM AND YEAR FE
m_buyback_lag_twfe <- feols(
  is_buyback ~ lag_layoff | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)
summary(m_buyback_lag_twfe)

m_layoff_lag_twfe <- feols(
  is_layoff ~ lag_buyback | gvkey + fyear,
  data = panel,
  cluster = ~gvkey
)
summary(m_layoff_lag_twfe)