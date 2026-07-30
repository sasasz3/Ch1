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

#set correct names for earnings data
clean_and_rename <- function(DT) {
  current_names <- colnames(DT)
  
  #extract colnames from brackets
  new_names <- gsub(".*\\((.*)\\).*", "\\1", current_names)
  setnames(DT, current_names, new_names)
  
  # change rdq header to "date" for later
  if ("rdq" %in% colnames(DT)) {
    setnames(DT, "rdq", "date")
  }
}
clean_and_rename(earnings)

#set everything lowercase
lapply(list(cal, layoffs, sdc, earnings, link_table), function(x) {
  setnames(x, tolower(colnames(x)))
})


#fix gvkeys if leading zeros are lost during import and force them into str format
fix_gvkey <- function(DT) {
  if ("gvkey" %in% colnames(DT)) {
    DT[, gvkey := str_pad(as.character(gvkey), width = 6, side = "left", pad = "0")]
  }
}
lapply(list(cal, layoffs, sdc, earnings, link_table), fix_gvkey)

# standardise dates for all tables
tables_with_date <- list(cal, sdc, layoffs, earnings)
lapply(tables_with_date, function(x) x[, date := as.Date(date)])

link_table[, `:=`(
  linkdt = as.Date(linkdt),
  linkenddt = as.Date(linkenddt)
)]
link_table[is.na(linkenddt), linkenddt := as.Date("2026-12-31")]

#create trading day index to accurately calculate distance from layoff announcement
setorder(cal, date)
cal <- unique(cal, by = "date")
cal[, t_index := 1:.N]


#creating the bases for trading day calendar
all_gvkeys <- unique(c(sdc$gvkey, layoffs$gvkey))


#create panel using cross join function 
#creates a row for every comp (gvkey) for every trading day (date)
master_panel <- CJ(gvkey = all_gvkeys, date = cal$date)
master_panel <- merge(master_panel, cal[, .(date, t_index)], by = "date")
setkey(master_panel, gvkey, date)



# create new table and assign the nearest date to it from the trading calendar 
# only rolls forward so that weekend announcements will appear on Monday not Friday
#assigns calendar index for the date it found
layoff_lookup <- cal[layoffs, on = .(date), roll = -Inf, 
                     .(gvkey, date, layoff_t = t_index)] 
layoff_lookup <- unique(layoff_lookup, by = c("gvkey", "date"))
setkey(layoff_lookup, gvkey, date)


#for each firm-date in master panel find the nearest layoff 
#calculate distance by taking master panel index minus the layoff lookup index
#if date is before the layoff announcement distance will return minus and vice versa
master_panel[, nearest_layoff_t := layoff_lookup[master_panel, 
                                                 on = .(gvkey, date), 
                                                 roll = "nearest", 
                                                 x.layoff_t]]

# current index (master_panel) - index at nearest layoff (layoff_lookup)
master_panel[, dist_to_layoff := t_index - nearest_layoff_t]

#set distance for days where firm has never done layoff
master_panel[is.na(dist_to_layoff), dist_to_layoff := 99999]





#create buyback dummies by rolling sdc buybacks to nearest trading days
sdc_lookup <- cal[sdc, on = .(date), roll = -Inf, 
                  .(gvkey, date, is_buyback = 1)]
sdc_lookup <- unique(sdc_lookup, by = c("gvkey", "date"))

#merge the buybacks with the master panel where for each date is_buyback
#takes the value of 1 if buyback happened for specific firm and 0 otherwise
master_panel[sdc_lookup, on = .(gvkey, date), is_buyback := 1]
master_panel[is.na(is_buyback), is_buyback := 0]


#create earnings dummy 
earnings_lookup <- cal[earnings, on = .(date), roll = "nearest", .(gvkey, date)]
earnings_lookup <- unique(earnings_lookup, by = c("gvkey", "date"))

#identify earning days
master_panel[, is_earnings := 0]
master_panel[earnings_lookup, on = .(gvkey, date), is_earnings := 1]

#
#event summary
event_summary <- master_panel[is_buyback == 1 & abs(dist_to_layoff) <= 30, 
                              .(count = .N), 
                              by = dist_to_layoff][order(dist_to_layoff)]

print(event_summary)

#visual plot
window_size <- 125

#sum 'is_buyback' for every relative day index
plot_data <- master_panel[abs(dist_to_layoff) <= window_size, 
                          .(buyback_count = sum(is_buyback)), 
                          by = dist_to_layoff]

setorder(plot_data, dist_to_layoff)

#create plot 
ggplot(
  plot_data,
  aes(
    x = dist_to_layoff,
    y = buyback_count
  )
) +
  geom_line(
    color = "#2c3e50",
    linewidth = 1
  ) +
  geom_col(
    fill = "steelblue",
    alpha = 0.3
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Buyback Activity Surrounding Layoff Announcements",
    subtitle = paste0(
      "Aggregated across ",
      uniqueN(master_panel$gvkey),
      " firms (2002–2025)"
    ),
    x = "Trading Days from Layoff Announcement (t = 0)",
    y = "Total Number of Buyback Announcements"
  ) +
  annotate(
    "text",
    x = 5,
    y = max(plot_data$buyback_count, na.rm = TRUE),
    label = "Layoff Date",
    color = "red",
    hjust = 0
  ) +
  theme_minimal()




