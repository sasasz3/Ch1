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

compustat <- setDT(read_excel(file.path(DATA_FOLDER, "compustat_controls.xlsx")))


#clean compustat data column headers
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

clean_and_rename(compustat)

# Fix GVKEY format
compustat[
  ,
  gvkey := str_pad(
    as.character(gvkey),
    width = 6,
    side = "left",
    pad = "0"
  )
]

setnames(compustat, c("Loss", "Net"), c("ni", "sale"))

#create controls object
controls <- unique(
  compustat[
    ,
    .(
      gvkey,
      fyear,
      at,
      ni,
      dltt,
      dlc,
      che,
      sale,
      ceq,
      csho,
      prcc_f
    )
  ],
  by = c("gvkey", "fyear")
)


# create actual control variables 

#size: log(total assets)
controls[, size := log(at)]

# return on asset: net income / total assets
controls[, roa := ni / at]

#leverage 
controls[ ,leverage :=(fcoalesce(dltt, 0) + fcoalesce(dlc, 0)) / at]

#cash ratio: cash / total assets
controls[ ,cash_ratio := che / at]

#market equity
controls[, market_equity :=  prcc_f * csho]

#market-to-book ratio
controls[ , market_to_book :=  (market_equity + at - ceq) / at]



#create lagged controls 
controls[ , previous_fyear := shift(fyear), by = gvkey]

controls[
  ,
  `:=`(
    lag_size = shift(size),
    lag_roa = shift(roa),
    lag_leverage = shift(leverage),
    lag_cash_ratio = shift(cash_ratio),
    lag_market_to_book = shift(market_to_book)
  ),
  by = gvkey
]


#do not assign if function shifts to not previous years
controls[
  fyear != previous_fyear + 1,
  `:=`(
    lag_size = NA_real_,
    lag_roa = NA_real_,
    lag_leverage = NA_real_,
    lag_cash_ratio = NA_real_,
    lag_market_to_book = NA_real_,
    lag_sales_growth = NA_real_
  )
]



#merge lagged controls into panel 
controls_for_panel <- controls[
  ,
  .(
    gvkey,
    fyear,
    lag_size,
    lag_roa,
    lag_leverage,
    lag_cash_ratio,
    lag_market_to_book
  )
]

panel <- merge(
  panel,
  controls_for_panel,
  by = c("gvkey", "fyear"),
  all.x = TRUE
)


#check missing values for controls 
control_missingness <- panel[
  ,
  .(
    observations = .N,
    
    missing_size =
      sum(is.na(lag_size)),
    
    missing_roa =
      sum(is.na(lag_roa)),
    
    missing_leverage =
      sum(is.na(lag_leverage)),
    
    missing_cash =
      sum(is.na(lag_cash_ratio)),
    
    missing_market_to_book =
      sum(is.na(lag_market_to_book))

  )
]

control_missingness

panel[
  ,
  controls_complete :=
    complete.cases(
      lag_size,
      lag_roa,
      lag_leverage,
      lag_cash_ratio,
      lag_market_to_book
    )
]

panel[
  ,
  .(
    total = .N,
    complete_controls = sum(controls_complete),
    pct_complete = mean(controls_complete)
  )
]



#create panel with completed controls 
panel_controls <- panel[controls_complete == TRUE]



setorder(panel, gvkey, fyear)

panel[
  ,
  previous_fyear := shift(fyear),
  by = gvkey
]

panel[
  ,
  `:=`(
    lag_layoff = shift(is_layoff),
    lag_buyback = shift(is_buyback)
  ),
  by = gvkey
]

# Remove lags where observations are not consecutive fiscal years
panel[
  is.na(previous_fyear) | fyear != previous_fyear + 1,
  `:=`(
    lag_layoff = NA_integer_,
    lag_buyback = NA_integer_
  )
]

panel_controls <- panel[
  controls_complete == TRUE
]



#PANEL A
mA1_controls <- feols(
  is_buyback ~
    is_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book,
  data = panel_controls,
  cluster = ~gvkey
)

mA2_controls <- feols(
  is_buyback ~
    is_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book |
    fyear,
  data = panel_controls,
  cluster = ~gvkey
)

mA3_controls <- feols(
  is_buyback ~
    is_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book |
    gvkey + fyear,
  data = panel_controls,
  cluster = ~gvkey
)

#PANEL B
mB1_controls <- feols(
  is_buyback ~
    lag_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book,
  data = panel_controls,
  cluster = ~gvkey
)

mB2_controls <- feols(
  is_buyback ~
    lag_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book |
    fyear,
  data = panel_controls,
  cluster = ~gvkey
)

mB3_controls <- feols(
  is_buyback ~
    lag_layoff +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book |
    gvkey + fyear,
  data = panel_controls,
  cluster = ~gvkey
)

#PANEL C
mC1_controls <- feols(
  is_layoff ~
    is_buyback +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book,
  data = panel_controls,
  cluster = ~gvkey
)

mC2_controls <- feols(
  is_layoff ~
    is_buyback +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book |
    fyear,
  data = panel_controls,
  cluster = ~gvkey
)

mC3_controls <- feols(
  is_layoff ~
    is_buyback +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book |
    gvkey + fyear,
  data = panel_controls,
  cluster = ~gvkey
)


#PANEL D
mD1_controls <- feols(
  is_layoff ~
    lag_buyback +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book,
  data = panel_controls,
  cluster = ~gvkey
)

mD2_controls <- feols(
  is_layoff ~
    lag_buyback +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book |
    fyear,
  data = panel_controls,
  cluster = ~gvkey
)

mD3_controls <- feols(
  is_layoff ~
    lag_buyback +
    lag_size +
    lag_roa +
    lag_leverage +
    lag_cash_ratio +
    lag_market_to_book |
    gvkey + fyear,
  data = panel_controls,
  cluster = ~gvkey
)

etable(
  mA1_controls,
  mA2_controls,
  mA3_controls,
  headers = c(
    "Pooled",
    "Year FE",
    "Firm + Year FE"
  )
)

etable(
  mB1_controls,
  mB2_controls,
  mB3_controls,
  headers = c(
    "Pooled",
    "Year FE",
    "Firm + Year FE"
  )
)

etable(
  mC1_controls,
  mC2_controls,
  mC3_controls,
  headers = c(
    "Pooled",
    "Year FE",
    "Firm + Year FE"
  )
)

etable(
  mD1_controls,
  mD2_controls,
  mD3_controls,
  headers = c(
    "Pooled",
    "Year FE",
    "Firm + Year FE"
  )
)

saveRDS( panel, file.path(DATA_FOLDER, "firm_fyear_panel_controls.rds"))
