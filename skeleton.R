library(data.table)
library(readxl)
library(stringr)

fiscal_year_map_public <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "fiscal_year_map_public.xlsx")
  )
)

clean_and_rename(fiscal_year_map_public)

setnames(
  fiscal_year_map_public,
  tolower(names(fiscal_year_map_public))
)

fiscal_year_map_public[
  ,
  gvkey := str_pad(
    as.character(gvkey),
    width = 6,
    side = "left",
    pad = "0"
  )
]

fiscal_year_map_public[
  ,
  fiscal_year := as.integer(fyear)
]

# Keep standard industrial consolidated observations
fiscal_year_map_public <- fiscal_year_map_public[
  indfmt == "INDL" &
    datafmt == "STD" &
    consol == "C"
]

# ============================================================
# CREATE PUBLIC-COMPANY PANEL
# ============================================================

panel <- copy(
  fiscal_year_map_public[
    fiscal_year >= 2002L &
      fiscal_year <= 2025L &
      exchg != 0
  ]
)

setorder(
  panel,
  gvkey,
  fiscal_year
)

# Check uniqueness
panel[
  ,
  .N,
  by = .(gvkey, fiscal_year)
][N > 1L]

# Summary
panel[
  ,
  .(
    observations = .N,
    firms = uniqueN(gvkey),
    first_fiscal_year = min(fiscal_year),
    last_fiscal_year = max(fiscal_year),
    mean_years_per_firm = round(.N / uniqueN(gvkey), 2)
  )
]