library(data.table)

source("config.R")

employment <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "compustat_employment.xlsx"),
    sheet = "Results"
  )
)