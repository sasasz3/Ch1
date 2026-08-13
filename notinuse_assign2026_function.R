last_fiscal_year <- fiscal_year_map[ , .SD[which.max(calendar_end)],
                                     by = gvkey][ , .(
                                       gvkey,
                                       last_fyear = fyear,
                                       last_calendar_end = calendar_end
                                     )
                                     ]


assign_2026 <- function(events, last_fy) {
  
  events[
    last_fy,
    on = "gvkey",
    `:=`(
      last_fyear = i.last_fyear,
      last_calendar_end = i.last_calendar_end
    )
  ]
  
  events[
    is.na(fyear) &
      year(date) == 2025 &
      last_fyear == 2025 &
      date > last_calendar_end,
    fyear := 2026L
  ]
  
  events[
    ,
    c("last_fyear", "last_calendar_end") := NULL
  ]
  
  return(events)
}

#attach last fiscal years
warn_events <- assign_2026( warn_events, last_fiscal_year)
capiq_events <- assign_2026( capiq_events, last_fiscal_year)
sdc_events <- assign_2026( sdc_events,last_fiscal_year)

# remove everything that could not be matched 
warn_unmatched_events <- warn_events[is.na(fyear)]
capiq_unmatched_events <- capiq_events[is.na(fyear)]
sdc_unmatched_events <- sdc_events[is.na(fyear)]

warn_events <- warn_events[!is.na(fyear)]
capiq_events <- capiq_events[!is.na(fyear)]
sdc_events <- sdc_events[!is.na(fyear)]