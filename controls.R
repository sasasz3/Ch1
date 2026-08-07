library(data.table)
library(readxl)
library(lubridate)
library(stringr)
library(fixest)
library(ggplot2)
library(DescTools)

source("config.R")


# ============================================================
# IMPORT CONTROLS
# ============================================================

controls <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "controls.xlsx")
  )
)


# ============================================================
# CLEAN COLUMN NAMES
# ============================================================

clean_and_rename(controls)

setnames(
  controls,
  tolower(names(controls))
)

# Correct the two headings affected by multiple parentheses
setnames(
  controls,
  old = c("loss", "net"),
  new = c("ni", "sale")
)


# ============================================================
# CLEAN IDENTIFIERS AND FISCAL YEARS
# ============================================================

controls[
  ,
  gvkey := str_pad(
    as.character(gvkey),
    width = 6,
    side = "left",
    pad = "0"
  )
]

controls[
  ,
  fiscal_year := as.integer(fyear)
]


# ============================================================
# KEEP STANDARD COMPUSTAT ANNUAL RECORDS
# ============================================================

controls <- controls[
  indfmt == "INDL" &
    datafmt == "STD" &
    consol == "C" &
    !is.na(gvkey) &
    gvkey != "" &
    !is.na(fiscal_year) &
    fiscal_year >= 2001L &
    fiscal_year <= 2025L
]


# ============================================================
# CHECK UNIQUENESS
# ============================================================

duplicate_firm_years <- controls[
  ,
  .N,
  by = .(
    gvkey,
    fiscal_year
  )
][
  N > 1L
]

print(duplicate_firm_years)







# ============================================================
# KEEP FULL CONTROL SOURCE INCLUDING 2001
# ============================================================

controls_all <- copy(controls)

setorder(
  controls_all,
  gvkey,
  fiscal_year
)


# ============================================================
# CREATE THE UNBALANCED ESTIMATION PANEL
# 2002–2025 ONLY
# ============================================================

panel <- copy(
  controls_all[
    between(
      fiscal_year,
      2002L,
      2025L
    )
  ]
)

setorder(
  panel,
  gvkey,
  fiscal_year
)



panel[
  ,
  .(
    observations = .N,
    firms = uniqueN(gvkey),
    first_fiscal_year = min(fiscal_year),
    last_fiscal_year = max(fiscal_year),
    mean_years_per_firm = round(
      .N / uniqueN(gvkey),
      2
    )
  )
]





# PLUG IN LAYOFF AND BUYBACK OBSERVATIONS 

# ============================================================
# COLLAPSE MAPPED EVENTS TO FIRM-FISCAL-YEAR
# ============================================================

layoff_yearly <- layoff_fiscal_events[
  !is.na(gvkey) &
    !is.na(fiscal_year),
  .(
    layoff_count = .N
  ),
  by = .(
    gvkey,
    fiscal_year
  )
]

buyback_yearly <- buyback_fiscal_events[
  !is.na(gvkey) &
    !is.na(fiscal_year),
  .(
    buyback_count = .N
  ),
  by = .(
    gvkey,
    fiscal_year
  )
]



# ============================================================
# ATTACH LAYOFFS AND BUYBACKS TO PANEL
# ============================================================

panel <- merge(
  panel,
  layoff_yearly,
  by = c("gvkey", "fiscal_year"),
  all.x = TRUE,
  sort = FALSE
)

panel <- merge(
  panel,
  buyback_yearly,
  by = c("gvkey", "fiscal_year"),
  all.x = TRUE,
  sort = FALSE
)

setorder(
  panel,
  gvkey,
  fiscal_year
)

panel[
  is.na(layoff_count),
  layoff_count := 0L
]

panel[
  is.na(buyback_count),
  buyback_count := 0L
]

panel[
  ,
  `:=`(
    layoff_year = layoff_count > 0L,
    buyback_year = buyback_count > 0L
  )
]