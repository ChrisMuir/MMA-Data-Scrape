## Functions used within the file "1-wiki_ufcfighters.R"
## Web Scrape Functions ----
## utfConvert ----
# Text encoding transformations from utf-8 to ACSII.
utfConvert <- function(df, colnum) {
  if (!is.data.frame(df)) {stop("input df is not a dataframe")}
  if (!colnum %in% seq_len(ncol(df))) {stop("col index out of range")}
  x <- iconv(df[[colnum]], from="UTF-8", to="LATIN1")
  x <- iconv(x, to='ASCII//TRANSLIT')
  return(x)
}

## mmaIso ----
# Isolate the fighter's MMA win-loss record, emliminate any win-loss
# records from other, non-MMA sports.
mmaIso <- function(df, colnum) {
  # ID the index of the beginning of the fighters MMA w-l record.
  mmarec <- grep("^Mixed martial arts record$", df[[colnum]])
  # Create list of all strings seen within the MMA record.
  key <- c("Total", "Wins", "By knockout", "By submission", "By decision", 
           "Losses", "Draws", "By disqualification", "By DQ", "No contest", 
           "No contests", "Unknown")
  # Append the string "mma" to the strings in colnum for every row 
  # thats associated with the MMA record.
  while (mmarec > 0) {
    mmarec <- mmarec + 1
    df[[colnum]][mmarec] <- paste("mma", df[[colnum]][mmarec])
    if (!df[[colnum]][mmarec+1] %in% key) {
      mmarec <- 0
    }
  }
  # Edit the key to make it a regex-friendly vector of length 1, 
  # then eliminate all rows for which df$colnum is found within the key.
  key <- unname(sapply(key, function(x) paste0("^", x, "$"))) %>% 
    paste(., collapse = "|")
  df <- df[!grepl(key, df[[colnum]]), ]
  # Eliminate the "mma" strings that we appended earlier in this function.
  wins <- grep("mma Wins", df[[colnum]])
  losses <- grep("mma Losses", df[[colnum]])
  if ((losses - wins) > 0) {
    for (n in (wins+1):(losses-1)) {
      df[[colnum]][n] <- str_replace(df[[colnum]][n], "mma", "Wins")
    }
  }
  if ((nrow(df) - losses) > 0) {
    for (n in (losses+1):nrow(df)) {
      if (str_detect(df[[colnum]][n], "mma ") && 
          !str_detect(df[[colnum]][n], "Draw")) {
        df[[colnum]][n] <- str_replace(df[[colnum]][n], "mma", "Losses")
      }
    }
  }
  return(df)
}


## Data Cleanup Functions ----
## getName ----
# If input string from variable Name is NA, attempt to unpack a character 
# string from the cell specified by df, linkCol, and rowID.
getName <- function(df, linkCol, rowID, string) {
  output <- NA
  if (!is.na(string)) {
    stop (paste0("Input string at row index ", rowID, " is not NA"))
  } else {
    output <- tail(unlist(strsplit(df[[linkCol]][rowID], "/")), n = 1)
    output <- gsub("_", " ", output)
    if (str_detect(output, "\\(")) {
      output <- trimws(gsub("\\(.*\\)", "", output))
    }
  }
  return(output)
}

## getDateFighters ----
# Unpack and convert strings for birth date of the fighter.
getDateFighters <- function(string) {
  output <- NA
  if (!is.na(string)) {
    if (grepl("\\d\\d\\d\\d-\\d\\d-\\d\\d", string)) {
      datevect <- regmatches(string, 
                             regexpr("\\d\\d\\d\\d-\\d\\d-\\d\\d", 
                                     string))
      output <- as.Date(datevect, format = "%Y-%m-%d")
    } else if (grepl("\\w+ \\d+, \\d\\d\\d\\d", string)) {
      datevect <- regmatches(string, 
                             regexpr("\\w+ \\d+, \\d\\d\\d\\d", 
                                     string))
      output <- as.Date(datevect, format = "%B %d, %Y")
    } else if (grepl("\\d+ \\w+ \\d\\d\\d\\d", string)){
      datevect <- regmatches(string, 
                             regexpr("\\d+ \\w+ \\d\\d\\d\\d", 
                                     string))
      output <- as.Date(datevect, format = "%d %B %Y")
    }
  }
  return(output)
}

## getDivision ----
# Two separate functions. getDivision attempts to lookup a fighters current
# division within the df bouts (derived from file "0-wiki_ufcbouts.R"). 
# If that fails, getDivision2 attempts to ID the fighter's current division 
# from the variable Division within fighters.
getDivision <- function(string) {
  divs <- bouts[which(bouts$FighterA == string | bouts$FighterB == string),'Weight']
  divs <- divs[!grepl("Catchweight", divs)]
  if (any(!is.na(divs)) && length(divs) >= 1) {
    output <- divs[1]
  } else {
    output <- NA
  }
  return(output)
}

getDivision2 <- function(string) {
  classes <- c("Super Heavyweight", "Heavyweight", "Light Heavyweight", 
               "Middleweight", "Welterweight", "Lightweight", "Featherweight", 
               "Bantamweight", "Flyweight", "Strawweight")
  lbs <- c("265+", "265", "205", "185", "170", "155", "145", "135", "125", "115")
  output <- NA
  if (!is.na(string)) {
    if (grepl("\r\n|\n", string)) {
      string <- unlist(strsplit(string, "\r\n|\n"))
    } else if (grepl(",|, ", string)) {
      string <- unlist(strsplit(string, ",|, "))
    }
    if (length(string) == 1) {
      output <- trimws(gsub("\\(.*\\)", "", string))
    } else if (any(grepl("[A-Za-z]resent|[A-Za-z]urrent", string))) {
      id <- grep("[A-Za-z]resent|[A-Za-z]urrent", string)
      if (length(id) == 1) {
        output <- trimws(gsub("\\(.*\\)", "", string[id]))
      }
    } else if (any(grepl("2016", string))) {
      id <- grep("2016", string)
      if (length(id) == 1) {
        output <- trimws(gsub("\\(.*\\)", "", string[id]))
      }
    } else if (any(grepl("[A-Za-z]ormer", string))) {
      id <- grep("[A-Za-z]ormer", string)
      if (length(string[-id]) == 1) {
        output <- trimws(gsub("\\(.*\\)", "", string[-id]))
      }
    }
  }
  if (output %in% lbs) {
    id <- grep(output, lbs, fixed = TRUE)
    output <- classes[id]
  }
  if (!output %in% classes) {
    output <- NA
  }
  return(output)
}

## colIndex ----
# Return the index of a column, given a column name and df.
colIndex <- function(colname, df) {
  string <- paste0("^", colname, "$")
  x <- grep(string, colnames(df))
  return(x)
}

## getHeight ----
# Unpack and convert height strings to height in inches.
getHeight <- function(string) {
  output <- NA
  if (!is.na(string)) {
    x <- unlist(strsplit(str_extract(string, "\\d+ ft \\d+ in"), " "))
  }
  if (!is.na(x) && length(x) > 1 && x[2] == "ft") {
    if (length(x) > 3 && x[4] == "in") {
      output <- (as.double(x[1]) * 12) + as.double(x[3])
    } else {
      output <- (as.double(x[1]) * 12)
    }
  }
  return(output)
}

## getReach ----
# Unpack and convert reach strings to reach in inches.
# (Reach is a measure of wingspan, finger tip to finger tip).
getReach <- function(string) {
  output <- NA
  if (!is.na(string) && grepl("^\\d\\d.* in", string)) {
    output <- unname(sapply(string, function(x) regmatches(x, regexpr("^\\d\\d.* in", x))))
    output <- as.double(strsplit(output, " ")[[1]][1])
  } else if (!is.na(string) && grepl("\\(\\d\\d.* in\\)", string)) {
    output <- unname(sapply(string, function(x) regmatches(x, regexpr("\\(\\d\\d.* in\\)", x))))
    output <- as.double(strsplit(str_extract(output, "\\d\\d.* in"), " ")[[1]][1])
  } else if (!is.na(string) && grepl("^\\d\\d.\\d$|^\\d\\d$", string)) {
    output <- as.double(string)
  }
  return(output)
}

## getWeight ----
# Unpack and convert weight strings to weight in lbs.
getWeight <- function(string) {
  output <- NA
  if (!is.na(string)) {
    output <- regmatches(string, regexpr("\\d\\d\\d.* lb", string))
    output <- as.double(strsplit(output, " ")[[1]][1])
  }
  return(output)
}

## colRename ----
# Allows for renaming of a single column or multiple columns within a df.
# currentName and newName need to be character vectors of the same length,
# and the indices of each correspond to one another, i.e. the function will
# attempt to apply newName[3] to the column of df that has currentName[3].
colRename <- function(df, currentName, newName) {
  if (!is.data.frame(df)) {
    stop ("Parameter df needs to be a dataframe")
  }
  if (length(currentName) != length(newName)) {
    stop ("Parameter currentName and newName need to be the same length")
  }
  if (length(currentName) < 1 || is.na(currentName) || 
      length(newName) < 1 || is.na(newName)) {
    stop ("One or both of the col name parameters is empty")
  }
  for (i in seq_len(length(currentName))) {
    if (!currentName[i] %in% colnames(df)) {
      warning (paste0(
        "col name ", "'", currentName[i], "'", " doesn't exist within dataframe"))
    } else {
      colnames(df)[colIndex(currentName[i], df)] <- newName[i]
    }
  }
  return(df)
}

