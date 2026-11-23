library(data.table)
library(readxl)
library(ggplot2)
library(scales)

#fetch file locations and reusable functions
source("config.R")
source("reusable_functions.R")


#LOAD AND CLEAN DATA
#-------------------------------------------------------------------

#load raw repurchase data
original_sample <- setDT(read_excel(file.path(DATA_FOLDER, "sample_with_repurchases.xlsx")))

#get rid of special characters and unnecessary text in column names 
#set all column names to lowercase
clean_and_rename(original_sample)


#force gvkey and dates into the right object types
original_sample[, gvkey := as.character(gvkey)]
original_sample[, datadate := as.Date(datadate)]

#filter out financial firms and utilities based on SIC codes
original_sample <- filter_industries(original_sample)

#filter out non NYSE NASDAQ and AMEX exchange observations 
original_sample <- filter_exchange(original_sample)




# IDENTIFY AND RESOLVE DUPLICATES
#--------------------------------------------------------------

#identify duplicate firm-year combinations
duplicates <- original_sample[
  , .N, by = .(gvkey, fyear)
][N > 1]

#count duplicate firm-year combinations before cleaning
duplicates[, .N]

#keep the observation with non-missing PRSTKC
duplicates_to_keep <- original_sample[
  duplicates,
  on = .(gvkey, fyear),
  nomatch = 0
][
  order(is.na(prstkc)),
  .SD[1],
  by = .(gvkey, fyear)
]

#remove original duplicate groups
original_sample <- original_sample[
  !duplicates,
  on = .(gvkey, fyear)
]

#add back selected observations
original_sample <- rbind(
  original_sample,
  duplicates_to_keep,
  fill = TRUE
)


#check remaining duplicate firm-year combinations
remaining_duplicates <- original_sample[
  , .N, by = .(gvkey, fyear)
][N > 1]

remaining_duplicates[, .N]

#remove temporary objects
rm(duplicates, duplicates_to_keep, remaining_duplicates)





#CREATE THE REPURCHASE VARIABLE 
#----------------------------------------------------------------

setorder(original_sample, gvkey, fyear)

#check variable availability 
original_sample[is.na(prstkc), .N]
original_sample[is.na(pstkrv), .N]


#define previous year
original_sample[, previous_fyear := shift(fyear), by = gvkey]

#create previous year value of the net number of prefer shared outstanding 
#make sure fiscal years are consecutive
original_sample[, previous_pstkrv := shift(pstkrv), by = gvkey]

original_sample[, preferred_stock_reduction := fifelse(
  !is.na(previous_fyear) &
    previous_fyear == fyear - 1 &
    !is.na(previous_pstkrv) &
    !is.na(pstkrv),
  pmax(previous_pstkrv - pstkrv, 0),
  0
)]

#draw zero lower limit for calculation for no negative repurchases
original_sample[, actual_repurchases :=
              pmax(prstkc - preferred_stock_reduction, 0)]





#AUDIT MISSING VALUES FOR REPURCHASES (only missing PRSTKC)
#----------------------------------------------------------------

#check missing values
original_sample[, .(
  missing_prstkc = sum(is.na(prstkc)),
  missing_pstkrv = sum(is.na(pstkrv)),
  missing_adjustment = sum(is.na(preferred_stock_reduction)),
  missing_actual_repurchases = sum(is.na(actual_repurchases))
)]



#CHECK IF CLEANING WORKED
#--------------------------------------------------------------

#check the count of observations
original_sample[, .(
  missing = sum(is.na(actual_repurchases)),
  non_missing = sum(!is.na(actual_repurchases)),
  zero_repurchases = sum(actual_repurchases == 0, na.rm = TRUE),
  positive_repurchases = sum(actual_repurchases > 0, na.rm = TRUE),
  negative_repurchases = sum(actual_repurchases < 0, na.rm = TRUE)
)]





#FILTER FOR MENIAL (<1% OF MARKET CAP) REPURCHASES 
#CREATES ADJUSTED REPURCHASES USED AS ROBUSTNESS
#----------------------------------------------------------------

#calculate market value of equity
original_sample[, market_cap := prcc_f * csho]


#apply 1% market capitalisation threshold
original_sample[, adjusted_repurchases := fifelse(
  is.na(actual_repurchases) | is.na(market_cap) |
    market_cap <= 0,
  NA_real_,
  fifelse(
    actual_repurchases < 0.01 * market_cap,
    0,
    actual_repurchases
  )
)]

#check observations
original_sample[, .(
  missing = sum(is.na(adjusted_repurchases)),
  repurchases_adjusted = sum(!is.na(adjusted_repurchases)),
  zero_repurchases_adjusted = sum(adjusted_repurchases == 0, na.rm = TRUE),
  positive_repurchases_adjusted = sum(adjusted_repurchases > 0, na.rm = TRUE),
  negative_repurchases_adjusted = sum(adjusted_repurchases < 0, na.rm = TRUE)
)]





#CREATE REPURCHASE INTENSITY VARIABLE 
#----------------------------------------------------------------

# Obtain total assets from the previous fiscal year
original_sample[, `:=`(
  lag_fyear_assets = shift(fyear),
  lag_at = shift(at)
), by = gvkey]

#check variable availability
original_sample[is.na(lag_at), .N]

# Retain lagged assets only when the previous observation is
# from the immediately preceding fiscal year
original_sample[
  is.na(lag_fyear_assets) | lag_fyear_assets != fyear - 1,
  lag_at := NA_real_
]



#calculate adjusted repurchase intensity 
original_sample[, adjusted_repurchase_intensity := fifelse(
  !is.na(adjusted_repurchases) &
    !is.na(lag_at) &
    lag_at > 0,
  adjusted_repurchases / lag_at,
  NA_real_
)]



# Adjusted repurchase intensity
original_sample[, .(
  total_observations = .N,
  missing = sum(is.na(adjusted_repurchase_intensity)),
  non_missing = sum(!is.na(adjusted_repurchase_intensity)),
  zero_repurchases = sum(adjusted_repurchase_intensity == 0, na.rm = TRUE),
  positive_repurchases = sum(adjusted_repurchase_intensity > 0, na.rm = TRUE),
  negative_repurchases = sum(adjusted_repurchase_intensity < 0, na.rm = TRUE)
)]
#----------------------------------------------------------------



# DESCRIPTIVE STATISTICS
#==================================================================

# 1. WINSORISE ADJUSTED REPURCHASE INTENSITY
#------------------------------------------------------------------

# Calculate 1st and 99th percentile cutoffs across the full sample
intensity_cutoffs <- quantile(
  original_sample$adjusted_repurchase_intensity,
  probs = c(0.01, 0.99),
  na.rm = TRUE
)

# Create winsorised intensity variable
original_sample[, adjusted_repurchase_intensity_w := pmin(
  pmax(
    adjusted_repurchase_intensity,
    intensity_cutoffs[1]
  ),
  intensity_cutoffs[2]
)]

# Inspect winsorisation thresholds
intensity_cutoffs

# Count observations affected by winsorisation
original_sample[, .(
  observations = sum(!is.na(adjusted_repurchase_intensity)),
  lower_winsorised = sum(
    adjusted_repurchase_intensity < intensity_cutoffs[1],
    na.rm = TRUE
  ),
  upper_winsorised = sum(
    adjusted_repurchase_intensity > intensity_cutoffs[2],
    na.rm = TRUE
  )
)]


# 2. FREQUENCY OF POSITIVE REPURCHASE YEARS PER FIRM
#------------------------------------------------------------------

# Calculate positive and observed repurchase years per firm
firm_counts <- original_sample[, .(
  observed_years = sum(!is.na(adjusted_repurchases)),
  positive_years = sum(adjusted_repurchases > 0, na.rm = TRUE)
), by = gvkey]

# Exclude firms without any available repurchase observations
firm_counts <- firm_counts[observed_years > 0]

# Count firms by number of positive-repurchase years
positive_year_distribution <- firm_counts[, .(
  number_of_firms = .N
), by = positive_years]

# Include categories with zero firms
all_years <- data.table(
  positive_years = 0:max(positive_year_distribution$positive_years)
)

positive_year_distribution <- merge(
  all_years,
  positive_year_distribution,
  by = "positive_years",
  all.x = TRUE
)

positive_year_distribution[
  is.na(number_of_firms),
  number_of_firms := 0
]

# Calculate percentage of firms
positive_year_distribution[, percentage_of_firms :=
                             100 * number_of_firms / sum(number_of_firms)
]

setorder(positive_year_distribution, positive_years)

positive_year_distribution


# 3. YEARLY REPURCHASE STATISTICS
#------------------------------------------------------------------

yearly_statistics <- original_sample[, .(
  
  # Number of observations with available adjusted repurchases
  observations = sum(!is.na(adjusted_repurchases)),
  
  # Number of positive-repurchase firm-years
  positive_firm_years = sum(
    adjusted_repurchases > 0,
    na.rm = TRUE
  ),
  
  # Aggregate gross repurchases
  gross_repurchases = if (any(!is.na(adjusted_repurchases))) {
    sum(adjusted_repurchases, na.rm = TRUE)
  } else {
    NA_real_
  },
  
  # Number of available intensity observations
  intensity_observations = sum(
    !is.na(adjusted_repurchase_intensity_w)
  ),
  
  # Mean intensity including zero-repurchase years
  mean_intensity = if (
    any(!is.na(adjusted_repurchase_intensity_w))
  ) {
    mean(adjusted_repurchase_intensity_w, na.rm = TRUE)
  } else {
    NA_real_
  },
  
  # Mean intensity conditional on positive repurchases
  mean_positive_intensity = if (
    any(adjusted_repurchase_intensity_w > 0, na.rm = TRUE)
  ) {
    mean(
      adjusted_repurchase_intensity_w[
        adjusted_repurchase_intensity_w > 0
      ],
      na.rm = TRUE
    )
  } else {
    NA_real_
  }
  
), by = fyear]

# Express intensity as a percentage
yearly_statistics[, `:=`(
  mean_intensity = mean_intensity * 100,
  mean_positive_intensity = mean_positive_intensity * 100
)]

setorder(yearly_statistics, fyear)

yearly_statistics


# 4. DESCRIPTIVE STATISTICS - FULL SAMPLE
#------------------------------------------------------------------

descriptive_stats <- function(data, variable, variable_name,
                              percentage = FALSE) {
  
  x <- data[[variable]]
  
  # Exclude missing observations
  x <- x[!is.na(x)]
  
  # Convert intensity ratios into percentages
  if (percentage) {
    x <- x * 100
  }
  
  data.table(
    variable = variable_name,
    N = length(x),
    mean = mean(x),
    median = median(x),
    SD = sd(x),
    P1 = as.numeric(quantile(x, 0.01)),
    P25 = as.numeric(quantile(x, 0.25)),
    P50 = as.numeric(quantile(x, 0.50)),
    P75 = as.numeric(quantile(x, 0.75)),
    P99 = as.numeric(quantile(x, 0.99)),
    minimum = min(x),
    maximum = max(x)
  )
}

# Full-sample descriptive statistics, including zeros
full_sample_statistics <- rbindlist(list(
  
  descriptive_stats(
    original_sample,
    "adjusted_repurchases",
    "Adjusted repurchases"
  ),
  
  descriptive_stats(
    original_sample,
    "adjusted_repurchase_intensity_w",
    "Winsorised adjusted repurchase intensity (%)",
    percentage = TRUE
  )
  
), use.names = TRUE)

full_sample_statistics


# 5. DESCRIPTIVE STATISTICS - POSITIVE REPURCHASES ONLY
#------------------------------------------------------------------

# Restrict sample to positive adjusted repurchases
positive_repurchases <- original_sample[
  adjusted_repurchases > 0
]

# Calculate conditional descriptive statistics
positive_sample_statistics <- rbindlist(list(
  
  descriptive_stats(
    positive_repurchases,
    "adjusted_repurchases",
    "Positive adjusted repurchase amounts"
  ),
  
  descriptive_stats(
    positive_repurchases,
    "adjusted_repurchase_intensity_w",
    "Positive winsorised adjusted repurchase intensity (%)",
    percentage = TRUE
  )
  
), use.names = TRUE)

positive_sample_statistics


# 6. FIRM-LEVEL OBSERVATION PERIOD
#------------------------------------------------------------------

firm_observation_summary <- original_sample[
  !is.na(adjusted_repurchases),
  .(observed_years = .N),
  by = gvkey
][, .(
  firms = .N,
  mean_observed_years = mean(observed_years),
  median_observed_years = median(observed_years),
  minimum_observed_years = min(observed_years),
  maximum_observed_years = max(observed_years)
)]

firm_observation_summary


# 7. SAMPLE SIZE SUMMARY
#------------------------------------------------------------------

# Restrict the sample to observations with available adjusted repurchases
original_sample <- original_sample[
  !is.na(adjusted_repurchases)
]

# Verify the restricted sample
original_sample[, .(
  observations = .N,
  firms = uniqueN(gvkey),
  positive_repurchases = sum(adjusted_repurchases > 0),
  zero_repurchases = sum(adjusted_repurchases == 0)
)]




sample_summary <- original_sample[, .(
  total_firm_years = .N,
  firms = uniqueN(gvkey),
  available_adjusted_repurchases =
    sum(!is.na(adjusted_repurchases)),
  positive_repurchase_firm_years =
    sum(adjusted_repurchases > 0, na.rm = TRUE),
  zero_repurchase_firm_years =
    sum(adjusted_repurchases == 0, na.rm = TRUE),
  available_winsorised_intensity =
    sum(!is.na(adjusted_repurchase_intensity_w))
)]

sample_summary


saveRDS(original_sample, file.path(DATA_FOLDER, "sample_with_repurchases.rds"))





