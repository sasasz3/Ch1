library(data.table)
library(readxl)
library(lubridate)
library(stringr)
library(fixest)
library(ggplot2)
library(DescTools)

source("config.R")

#import data: trading day calendar, layoff announcements, buyback announcements
#earnings announcements and permco matching respectively
cal <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "cal.xlsx"),
    sheet = "calendar"
  )
)

layoffs <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "layoffs_for_analysis.xlsx"),
    sheet = "layoffs"
  )
)

sdc <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "repurchases_for_analysis.xlsx"),
    sheet = "sdc"
  )
)

earnings <- fread(
  file.path(DATA_FOLDER, "rdq.csv")
)

link_table <- setDT(
  read_excel(
    file.path(DATA_FOLDER, "gvkey_permco.xlsx")
  )
)


