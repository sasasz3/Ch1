library(data.table)



#cleaning and renaming messy columnames from Compustat mostly
clean_and_rename <- function(DT) {
  
  current_names <- colnames(DT)
  
  #strips the word found in the first parentheses
  new_names <- sub(
    "^[^(]*\\(([^)]*)\\).*$",
    "\\1",
    current_names
  )
  
  #sets everything to lowercase
  new_names <- tolower(new_names)
  
  setnames(
    DT,
    current_names,
    new_names
  )
}


#to fix gvkey if they were downloaded not as strings and lost their leading zeros
# if gvkey column present distorted values are fixed by putting back leading zeros
fix_gvkey <- function(DT) {
  if ("gvkey" %in% colnames(DT)) {
    DT[, gvkey := str_pad(as.character(gvkey), width = 6, side = "left", pad = "0")]
  }
}


#count the number of unique gvkeys and stores them as a list
get_firms <- function(DT) {
  
  object_name <- deparse(substitute(DT))
  
  #make sure it is not counting NA observations
  firms <- unique(
    DT$gvkey[!is.na(DT$gvkey)]
  )
  
  firms_name <- paste0(
    object_name,
    "_firms"
  )
  
  assign(
    firms_name,
    firms,
    envir = .GlobalEnv
  )
}


#find intersection of a certain variable between two objects 
find_intersection <- function(DT1, DT2, variable) {
  
  values1 <- unique(
    DT1[[variable]][!is.na(DT1[[variable]])]
  )
  
  values2 <- unique(
    DT2[[variable]][!is.na(DT2[[variable]])]
  )
  
  intersect(
    values1,
    values2
  )
}



#find union of a certain variable between two objects
find_union <- function(DT1, DT2, variable) {
  
  values1 <- unique(
    DT1[[variable]][!is.na(DT1[[variable]])]
  )
  
  values2 <- unique(
    DT2[[variable]][!is.na(DT2[[variable]])]
  )
  
  union(
    values1,
    values2
  )
}