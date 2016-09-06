## Functions used within the file "0-wiki_ufcbouts.R"
## Web Scrape Functions ----
## getTables ----
# ID the indices of specific tables within a page.
getTables <- function(htmltabl) {
  output <- vector()
  for (n in seq_len(length(htmltabl))) {
    rawtabl <- html_table(htmltabl[n], fill = TRUE)
    colnum <- ncol(rawtabl[[1]])
    rownum <- nrow(rawtabl[[1]])
    if (colnum == 8 && rownum >= 6 && 
        !is.na(rawtabl[[1]][1, 1]) && 
        rawtabl[[1]][1, 1] == "Weight class") {
      output <- c(output, n)
    }
  }
  return(output)
}


## getEventNames ----
# Extract the event names of each card found on a page.
getEventNames <- function(htmltabl, tableID) {
  output <- vector()
  for (t in tableID) {
    output <- c(output, htmltabl %>% 
                    extract2(t) %>% 
                    html_table(fill = TRUE) %>% 
                    colnames() %>% 
                    extract2(1))
  }
  return(output)
}


## getDate ----
# Extract the dates that the events took place, record the index of the 
# table within which the event date was found, and record and indication as to
# the format of the date.
getDateBouts <- function(htmltabl, tableID) {
  dateVect <- vector()
  indexVect <- vector()
  indicateVect <- vector()
  for (t in tableID) {
    infoVect <- htmltabl %>% 
      extract2(t) %>% 
      html_table(fill=TRUE) %>% 
      extract2(2)
    dateindex <- grep("\\d\\d\\d\\d-\\d\\d-\\d\\d", infoVect)
    dateindicate <- "A"
    if (length(dateindex) != 1) {
      datecomp <- vector()
      for (x in months) {
        datecomp <- c(datecomp, grep(x, infoVect))
      }
      dateindex <- unique(datecomp)
      dateindicate <- "B"
    }
    dateVect <- c(dateVect, infoVect[dateindex])
    indexVect <- c(indexVect, dateindex)
    indicateVect <- c(indicateVect, dateindicate)
  }
  return(list(dates = dateVect, index = indexVect, dateindicate = indicateVect))
}


## getVenue ----
# Extract the name of the venues.
getVenue <- function(htmltabl, tableID, index) {
  venueVect <- vector()
  for (t in seq_len(length(tableID))) {
    venueVect <- c(venueVect, htmltabl %>% 
                     extract2(tableID[t]) %>% 
                     html_table(fill = TRUE) %>% 
                     extract2(2) %>% 
                     extract2(index[t] + 1))
  }
  return(venueVect)
}


## getLoc ----
# Extract the city, state, country of each event.
getLoc <- function(htmltabl, tableID, index) {
  # Create vector of countries in which UFC events have taken place.
  cityVect <- vector()
  stateVect <- vector()
  countryVect <- vector()
  for (t in seq_len(length(tableID))) {
    geo <- htmltabl %>% 
      extract2(tableID[t]) %>% 
      html_table(fill = TRUE) %>% 
      extract2(2) %>% 
      extract2(index[t] + 2) %>% 
      strsplit(', ')
    if (length(geo[[1]]) == 3) {
      cityVect <- c(cityVect, geo[[1]][1])
      stateVect <- c(stateVect, geo[[1]][2])
      countryVect <- c(countryVect, geo[[1]][3])
    } else {
      if (geo[[1]][2] %in% countries) {
        cityVect <- c(cityVect, geo[[1]][1])
        stateVect <- c(stateVect, NA)
        countryVect <- c(countryVect, geo[[1]][2])
      } else {
        cityVect <- c(cityVect, geo[[1]][1])
        stateVect <- c(stateVect, geo[[1]][2])
        countryVect <- c(countryVect, "U.S.")
      }
    }
  }
  return(list(city = cityVect, state = stateVect, country = countryVect))
}


## appendDF ----
# Extract fight results from a single event as a df, then append previously
# extracted variables to that df, then rbind that df to the final output df (bouts).
appendDF <- function(htmltabl, resultsnum, nameVect, dateVect, venueVect, locVect) {
  outputdf <- data_frame()
  for (k in seq_len(length(resultsnum))) {
    holder <- htmltabl %>% 
      extract2(resultsnum[k]) %>% 
      html_table() %>% 
      as.data.frame(stringsAsFactors = FALSE)
    
    # Transformations to the fight outcomes table
    holder <- holder[-c(1), ]
    holder <- holder[(!is.na(holder[, 2]) & !is.na(holder[, 3])) & 
                       (holder[, 2] != "" & holder[, 3] != ""), ]
    colnames(holder) <- c("Weight", "FighterA", "VS", "FighterB", 
                          "Result", "Round", "Time", "Notes")
    # Append event name.
    holder$Event <- nameVect[k]
    # Append event date
    if (dateVect[[3]][k] == "A") {
      dateVect[[1]][k] <- regmatches(
        dateVect[[1]][k], regexpr("\\d\\d\\d\\d-\\d\\d-\\d\\d", dateVect[[1]][k]))
      holder$Date <- as.Date(dateVect[[1]][k], format = "%Y-%m-%d")
    }
    if (dateVect[[3]][k] == "B") {
      if (grepl("^[A-Z][a-z]", dateVect[[1]][k])) {
        holder$Date <- as.Date(dateVect[[1]][k], format = "%B %d, %Y")
      } else if (grepl("^\\d\\d", dateVect[[1]][k])) {
        holder$Date <- as.Date(dateVect[[1]][k], format = "%d %B %Y")
      } else {
        holder$Date <- NA
      }
    }
    # append venue name
    holder$Venue <- venueVect[k]
    # Scrape city, state, and country
    holder$City <- locVect[[1]][k]
    holder$State <- locVect[[2]][k]
    holder$Country <- locVect[[3]][k]
    # rbind to 
    outputdf <- rbind(outputdf, holder)
  }
  return(outputdf)
}


## Data Cleanup Functions ----
## utfConvert ----
# Text encoding transformations from utf-8 to ACSII.
utfConvert <- function(df, colnum) {
  if (!is.data.frame(df)) {stop("input df is not a dataframe")}
  if (!colnum %in% seq_len(ncol(df))) {stop("col index out of range")}
  x <- iconv(df[[colnum]], from="UTF-8", to="LATIN1")
  x <- iconv(x, to='ASCII//TRANSLIT')
  return(x)
}

## strEliminate ----
# Eliminate a string pattern within all elements of a character vector.
strEliminate <- function(df, colnum, string) {
  if (!is.data.frame(df)) {stop("input 'df' is not a dataframe")}
  if (!colnum %in% seq_len(ncol(df))) {stop("'colnum' index out of range")}
  if (length(string) > 1) {stop("parameter 'string' must be of length 1")}
  x <- sapply(df[[colnum]], function(x) gsub(string, "", x))
  return(x)
}

## toSeconds & boutSeconds ----
# Convert string with format "H:M:S" to seconds.
toSeconds <- function (x) {
  unlist(lapply(x, function(i) {
    i <- as.numeric(strsplit(i, ':', fixed=TRUE)[[1]])
    if (length(i) == 3) {
      i[1]*3600 + i[2]*60 + i[3]
    } else if (length(i) == 2) {
      i[1]*60 + i[2]
    } else if (length(i) == 1) {
      i[1]
    }
  }))
}

boutSeconds <- function(time, round) {
  if (time == "" || is.na(time)) {
    output <- NA
  } else if (round == 1 || is.na(round)) {
    output <- toSeconds(time)
  } else {
    output <- ((round - 1) * 300) + toSeconds(time)
  }
  return(output)
}

## vectSplit ----
# Splits a character string into two separate strings, based on open and closed
# paranthesis found within the input string.
vectSplit <- function(string) {
  if (!str_detect(string, "\\(")) {
    output <- list(mainresult = string, subresult = NA)
    return(output)
  }
  mainresult <- vector()
  subresult <- vector()
  x <- strsplit(string, "\\(")
  y <- trimws(x[[1]][1])
  z <- tolower(strsplit(x[[1]][2], "\\)")[[1]][1])
  output <- list(mainresult = y, subresult = z)
  return(output)
}