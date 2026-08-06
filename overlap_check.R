library(data.table)
library(lubridate)

source("config.R")

capitaliq <- setDT(read_excel(file.path(DATA_FOLDER, "capitalIQ_raw.xlsx"), sheet = "Screening"))
compustat <- setDT(read_excel(file.path(DATA_FOLDER, "compustat.xlsx")))

setnames(capitaliq, "Key Developments By Date", "calendar_date")
setnames(capitaliq, "Company Name(s)", "company_name")

capitaliq[, calendar_date := as.Date(calendar_date)]
capitaliq[, gvkey :=sub("^GV_", "", gvkey)]
capitaliq[, gvkey := as.character(gvkey)]


#check overlap for gvkeys that appear in both WARN and capitalIQ dataset
cap_gvkeys <- unique(capitaliq[!is.na(gvkey), gvkey])
warn_gvkeys <- unique(layoff_events[!is.na(gvkey), gvkey])
sdc_gvkeys <- unique(buyback_events[!is.na(gvkey),gvkey])
comp_gvkeys <- unique(compustat[!is.na(gvkey),gvkey])


cat("Capital IQ firms:", length(cap_gvkeys), "\n")
cat("WARN firms:", length(warn_gvkeys), "\n")
cat("SDC firms:", length(sdc_gvkeys), "\n")


overlap_warn_cap <- intersect(cap_gvkeys, warn_gvkeys)
overlap_sdc_cap <- intersect(cap_gvkeys, sdc_gvkeys)
overlap_warn_sdc <- intersect(sdc_gvkeys, warn_gvkeys)


cat("Capital IQ ∩ WARN:", length(overlap_warn_cap), "\n")
cat("Capital IQ ∩ SDC:", length(overlap_sdc_cap), "\n")
cat("WARN ∩ SDC:", length(overlap_warn_sdc), "\n")


cap_not_warn <- setdiff(cap_gvkeys, warn_gvkeys)

cat("Capital IQ firms not in WARN:", length(cap_not_warn), "\n")

# Of those, how many are in SDC?
cap_not_warn_in_sdc <- intersect(cap_not_warn, sdc_gvkeys)

cat("Capital IQ firms not in WARN but in SDC:",
    length(cap_not_warn_in_sdc), "\n")

overlap_comp_cap <- intersect(cap_not_warn_in_sdc, comp_gvkeys)
cat("COMP ∩ CAP not WARN:", length(overlap_comp_cap), "\n")
cat("Percent of Capital IQ firms found in WARN:", round(100 * length(overlap_comp_cap) / length(cap_gvkeys), 2), "%\n")


# Extract unique company names for the selected gvkeys
cap_not_warn_in_sdc_names <- unique(
  capitaliq[
    gvkey %in% cap_not_warn_in_sdc,
    company_name
  ]
)

# Remove missing names if any
cap_not_warn_in_sdc_names <- cap_not_warn_in_sdc_names[!is.na(cap_not_warn_in_sdc_names)]

# Save to DATA_FOLDER
writeLines(
  cap_not_warn_in_sdc_names,
  file.path(DATA_FOLDER, "cap_not_warn_in_sdc_company_names.txt")
)