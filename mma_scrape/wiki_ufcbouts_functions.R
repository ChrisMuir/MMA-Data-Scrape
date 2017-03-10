## Functions used within the file "0-wiki_ufcbouts.R"
## Web Scrape Functions ----
## getTables ----
# ID the indices of specific tables within a page.
getTables <- function(htmltabl) {
  # Params:
  # htmltabl = list obj, elements of which are HTML tables.
  # 
  # Output is numeric vector indicating which tables within htmltabl are 
  # relevant.
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

## utfConvert ----
# Text encoding transformations from utf-8 to ACSII.
utfConvert <- function(vect) {
  # Params:
  # vect = character vector.
  # 
  # Output is character vector post encoding transformations.
  if (!is.vector(vect)) {stop("input vect is not a vector")}
  x <- iconv(vect, from="UTF-8", to="LATIN1")
  x <- iconv(x, to='ASCII//TRANSLIT')
  return(x)
}

## getEventNames ----
# Extract the event names of each card found on a page.
getEventNames <- function(htmltabl, tableID) {
  # Params:
  # htmltabl = list obj, elements of which are HTML tables.
  # tableID = numeric vector, table indices.
  #
  # Output is vector of event names associated with the tables of htmltabl.
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
# table within which the event date was found, and record an indication as to
# the format of the date.
getDateBouts <- function(htmltabl, tableID) {
  # Params:
  # htmltabl = list obj, elements of which are HTML tables.
  # tableID = numeric vector, table indices.
  #
  # Output is a list that contains the event dates, numeric index of the table 
  # in which each date was found, and char string indicating the format of 
  # the date string.
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
      for (x in allMonths) {
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
  # Params:
  # htmltabl = list obj, elements of which are HTML tables.
  # tableID = numeric vector, table indices.
  # index = numeric vector, table indices.
  #
  # Output is vector of event names.
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
  # Params:
  # htmltabl = list obj, elements of which are HTML tables.
  # tableID = numeric vector, table indices.
  # index = numeric vector, table indices.
  #
  # Output is list containing vectors of city names, state names, and country 
  # names.
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
# For each element of resultsnum, extract fight results from a single event as 
# a df, then append previously extracted variables to that df, then rbind that 
# df to the final output df.
appendDF <- function(htmltabl, resultsnum, nameVect, dateVect, venueVect, 
                     locVect, i) {
  # Params:
  # htmltabl = list obj, elements of which are HTML tables.
  # resultsnum = numeric vector, table indices.
  # nameVect = char vector, event names.
  # dateVect = list, events dates and date elements.
  # venueVect = char vector, venue names.
  # locVect = list, event locations.
  # i = integer, current iteration of the for loop.
  #
  # Output is a tidy dataframe of event info related to the current loop 
  # iteration.
  outputdf <- data_frame()
  for (k in seq_len(length(resultsnum))) {
    holder <- htmltabl %>% 
      extract2(resultsnum[k]) %>% 
      html_table() %>% 
      as.data.frame(stringsAsFactors = FALSE)
    
    # Transformations to the fight outcomes table
    holder <- holder %>% 
      magrittr::extract(!is.na(.[, 2]) & !is.na(.[, 3]), ) %>% 
      magrittr::extract(grepl("def", .[, 3]) | grepl("vs", .[, 3]), )
    
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
    # Append venue name
    holder$Venue <- venueVect[k]
    # Append city, state, and country
    holder$City <- locVect[[1]][k]
    holder$State <- locVect[[2]][k]
    holder$Country <- locVect[[3]][k]
    # Append event wikipedia link
    holder$wikilink <- i
    # rbind to 
    outputdf <- rbind(outputdf, holder)
  }
  return(outputdf)
}


## Data Cleanup Functions ----
## getInterimChampLabels ----
# If an interim championship belt is awarded as the result of a fight, record 
# the winners name in new variable.
getInterimChampLabels <- function(bouts, datafile = NULL) {
  # Params:
  # bouts = dataframe of newly scraped data.
  # datafile = loaded dataframe of previously scraped data, default is NULL.
  #
  # Output is vector of fighter names (interim belt winners).
  if (!is.null(datafile)) {
    FighterA <- c(bouts$FighterA, datafile$FighterA)
    FighterB <- c(bouts$FighterB, datafile$FighterB)
    Date <- c(bouts$Date, datafile$Date)
    Weight <- c(bouts$Weight, datafile$Weight)
    interimChampPost <- c(rep(NA, nrow(bouts)), datafile$interimChampPost)
  } else {
    FighterA <- bouts$FighterA
    FighterB <- bouts$FighterB
    Date <- bouts$Date
    Weight <- bouts$Weight
    interimChampPost <- rep(NA, nrow(bouts))
  }
  # id all obs indices in which an (ic) tag exists.
  ids <- c(grep("\\(ic)", FighterA, ignore.case = TRUE), 
           grep("\\(ic)", FighterB, ignore.case = TRUE)) %>% 
    .[order(.)]
  
  # Make list of all fighters that have an (ic) tag, an ex of each list element:
  # c("fighter", "fighter (ic)", "fighter (C)")
  name <- lapply(ids, function(x) c(
    c(FighterA[x], FighterB[x]) %>% 
      .[grepl("\\(ic)", ., ignore.case = TRUE)] %>% 
      gsub(" \\(ic)", "", ., ignore.case = TRUE), 
    c(FighterA[x], FighterB[x]) %>% 
      .[grepl("\\(ic)", ., ignore.case = TRUE)], 
    c(FighterA[x], FighterB[x]) %>% 
      .[grepl("\\(ic)", ., ignore.case = TRUE)] %>% 
      gsub(" \\(ic)", " \\(c)", ., ignore.case = TRUE)
  ))
  
  # For each obs that contains an (ic) tag, id the fighter that has it, and go 
  # back to that fighters last fight and put fighters name in col interimChampPost.
  # For each obs that contains (ic), if neither fighter has (c), then for the 
  # fighter with the (ic) tag, put his/her name in col interimChampPost.
  for (i in seq_len(length(ids))) {
    # If no presence of "(c)", then add the (ic) fighters name to col 
    # interimChampPost.
    if (all(!grepl("\\(c)|\\(UFC Champion)", c(FighterA[ids[i]], FighterB[ids[i]]), 
                   ignore.case = TRUE))) {
      interimChampPost[ids[i]] <- FighterA[ids[i]]
    }
    # For the (ic) fighter, ID the win just prior to the current iter of ids.
    icID <- which(FighterA %in% name[[i]] & Date < Date[ids[i]])[1]
    if (is.na(icID)) {next}
    # If the weightclass equals that of the current iter of ids, or either 
    # weightclass is NA, or the weight of the win prior is catchweight, then 
    # assign fighter in questions name to interimChampPost of the previous win.
    if (any(is.na(c(Weight[ids[i]], Weight[icID]))) || 
        Weight[ids[i]] == Weight[icID] || 
        grepl("catchweight", Weight[icID], ignore.case = TRUE)) {
      interimChampPost[icID] <- name[[i]][1]
    }
  }
  # Output
  if (!is.null(datafile)) {
    splitpoint <- nrow(bouts)
    boutsinterimChampPost <- interimChampPost[1:splitpoint]
    datafileinterimChampPost <- interimChampPost[(splitpoint+1):length(interimChampPost)]
    return(list(boutsinterimChampPost, datafileinterimChampPost))
  } else {
    return(interimChampPost)
  }
}


## getChampLabels ----
# If a championship belt is awarded as the result of a fight, record 
# the winners name in new variable.
getChampLabels <- function(bouts, datafile = NULL) {
  # Params:
  # bouts = dataframe of newly scraped data.
  # datafile = loaded dataframe of previously scraped data, default is NULL.
  #
  # Output is vector of fighter names (championship belt winners).
  if (!is.null(datafile)) {
    FighterA <- c(bouts$FighterA, datafile$FighterA)
    FighterB <- c(bouts$FighterB, datafile$FighterB)
    Date <- c(bouts$Date, datafile$Date)
    Weight <- c(bouts$Weight, datafile$Weight)
    champPost <- c(rep(NA, nrow(bouts)), datafile$champPost)
  } else {
    FighterA <- bouts$FighterA
    FighterB <- bouts$FighterB
    Date <- bouts$Date
    Weight <- bouts$Weight
    champPost <- rep(NA, nrow(bouts))
  }
  
  # id all obs indices in which a champion tag exists.
  ids <- c(grep("\\(c)|\\(UFC Champion)", FighterA, ignore.case = TRUE), 
           grep("\\(c)|\\(UFC Champion)", FighterB, ignore.case = TRUE)) %>% 
    .[order(.)]
  
  # Make list of all fighters that have an (c) tag, an ex of each list element:
  # c("fighter", "fighter (c)", "fighter (iC)")
  name <- lapply(ids, function(x) c(
    c(FighterA[x], FighterB[x]) %>% 
      .[grepl("\\(c)|\\(UFC Champion)", ., ignore.case = TRUE)] %>% 
      gsub(" \\(c)| \\(UFC Champion)", "", ., ignore.case = TRUE), 
    c(FighterA[x], FighterB[x]) %>% 
      .[grepl("\\(c)|\\(UFC Champion)", ., ignore.case = TRUE)], 
    c(FighterA[x], FighterB[x]) %>% 
      .[grepl("\\(c)|\\(UFC Champion)", ., ignore.case = TRUE)] %>% 
      gsub(" \\(c)| \\(UFC Champion)", " \\(ic)", ., ignore.case = TRUE)
  ))
  
  # For each obs that contains an (c) tag, id the fighter that has it, and go 
  # back to that fighters last fight and put fighters name in col champPost.
  # Also add bout winners name to col champPost of the current fight in question.
  for (i in seq_len(length(ids))) {
    # Add winner name of the fight to col champPost.
    champPost[ids[i]] <- FighterA[ids[i]]
    # For the (c) fighter, ID the win just prior to the current iter of ids.
    cID <- which(FighterA %in% name[[i]] & Date < Date[ids[i]])[1]
    if (is.na(cID)) {next}
    # If the weightclass equals that of the current iter of ids, or either 
    # weightclass is NA, or the weight of the win prior is catchweight, then 
    # assign fighter in questions name to interimChampPost of the previous win.
    if (any(is.na(c(Weight[ids[i]], Weight[cID]))) || 
        Weight[ids[i]] == Weight[cID] || 
        grepl("catchweight", Weight[cID], ignore.case = TRUE)) {
      champPost[cID] <- name[[i]][1]
    }
  }
  # Output
  if (!is.null(datafile)) {
    splitpoint <- nrow(bouts)
    boutschampPost <- champPost[1:splitpoint]
    datafilechampPost <- champPost[(splitpoint+1):length(champPost)]
    return(list(boutschampPost, datafilechampPost))
  } else {
    return(champPost)
  }
}


## toSeconds & boutSeconds ----
# Convert string with format "H:M:S" to seconds.
toSeconds <- function (x) {
  # Params:
  # x = char string, format "H:M:S".
  #
  # Output is numeric value (time converted to seconds).
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
# Combine output of func toSeconds with the round in which the fight ended to 
# get the total fight time in seconds.
# (This info will be used later to create a number of feature variables).
boutSeconds <- function(time, round) {
  # Params:
  # time = char string, format "H:M:S".
  # round = numeric value, rond in which the fight ended.
  #
  # Output is numeric value, total number of seconds a fight lasted.
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
  # Params:
  # string = char string.
  #
  # Output is a list, containing the input string split into two vectors.
  if (!str_detect(string, "\\(")) {
    output <- list(string, NA)
    return(output)
  }
  x <- strsplit(string, "\\(")
  y <- trimws(x[[1]][1])
  z <- tolower(strsplit(x[[1]][2], "\\)")[[1]][1])
  output <- list(y, z)
  return(output)
}

## mergeNames ----
# Goal is to do approximate string matching/merging for fighter names across all 
# variables that include fighter names (FighterA, FighterB, interimChampPost 
# and champPost). This function takes boutsdf as input, combines these four 
# variables into a single char vector, performs clustering and merging of 
# approximate values, then splits the single char vector back up and assigns 
# each portion to its original position within boutsdf.
mergeNames <- function(df) {
  # Params:
  # df = the boutsdf dataframe.
  # 
  # Output is boutsdf, but with approximate strings having been merged accross 
  # four variables (FighterA, FighterB, interimChampPost and champPost).
  all_fighters <- c(df$FighterA, 
                    df$FighterB, 
                    df$interimChampPost, 
                    df$champPost)
  
  markers <- c(rep("FighterA", nrow(df)), 
               rep("FighterB", nrow(df)), 
               rep("interimChampPost", nrow(df)), 
               rep("champPost", nrow(df)))
  
  all_fighters <- all_fighters %>% 
    {refinr::key_collision_merge(., bus_suffix = FALSE)} %>% 
    {refinr::n_gram_merge(., bus_suffix = FALSE)}
  
  df$FighterA <- all_fighters[markers == "FighterA"]
  df$FighterB <- all_fighters[markers == "FighterB"]
  df$interimChampPost <- all_fighters[markers == "interimChampPost"]
  df$champPost <- all_fighters[markers == "champPost"]
  return(df)
}
