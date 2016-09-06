## Wiki UFC Fight Results Scraper.
## This will scrape the results of every UFC fight from Wikipedia.
## Outputs are data_frame object "bouts", and vector "fightlinks".

## Install packages and do some prep work ----
# Read in required packages & files.
library(rvest)
library(dplyr)
library(magrittr)
library(stringr)
source("~/mma_scrape/wiki_ufcbouts_func_calls.R")

# Pull html from wiki page of all UFC events.
cards <- read_html("https://en.wikipedia.org/wiki/List_of_UFC_events")

# Extract all url strings.
cardlinks <- cards %>% 
  html_nodes('td:nth-child(2) a') %>% 
  html_attr('href') %>% 
  unique()

# Remove links for fight events that were canceled and thus have no 
# fight results.
cardlinks <- cardlinks[!grepl("UFC_176|UFC_151", cardlinks)]

# Append each url string to form a complete url for each event.
cardlinks <- unname(
  sapply(cardlinks, function(x) paste0("https://en.wikipedia.org", x)))

# Create vector of all of the months of the year.
months <- c("January ", "February ", "March ", "April ", 
            "May ", "June ", "July ", "August ", "September ", 
            "October ", "November ", "December ")

# Create vector of countries that have hosted UFC events.
countries <- cards %>% 
  html_nodes('td:nth-child(5)') %>% 
  html_text() %>% 
  sapply(., function(x) tail(strsplit(x, ", ")[[1]], n=1)) %>% 
  unname() %>% 
  unique() %>% 
  c(., "England")

## Start the scraping ----
# Scraping is all contained within the for loop below.
# Object fighterlinks will house urls for each fighter the scraper encounters
# that has a wiki page. Those urls will be used to facilitate scraping within
# the file "1-wiki_ufcfighters.R".
# Turn off warnings, they will be turned back on after the loop finishes.
oldw <- getOption("warn")
options(warn = -1)
bouts <- data_frame()
fighterlinks <- vector()
for (i in cardlinks) {
  # Read html of the specific fight card.
  html <- read_html(i)
  
  # Record wiki links of all the fighters on the card.
  fighterlinks <- c(fighterlinks, html %>% 
                      html_nodes('.toccolours td a') %>% 
                      html_attr('href'))
  
  # Extract all tables within page, then do control flow and NA checks.
  tables <- html %>% 
    html_nodes('table')
  # If tables is all NA's or empty, skip to the next iteration of cardlinks.
  if (all(is.na(tables)) || length(tables) < 0) {next}
  # If tables contains any NA's, eliminate them and continue.
  if (any(is.na(tables))) {
    tables <- tables[!is.na(tables)]
  }
  
  # ID fight results tables and info tables for each card within a single url.
  resultsnum <- getTables(tables)
  infonum <- resultsnum - 2
  for (t in seq_len(length(infonum))) {
    test <- tables %>% 
      extract2(infonum[t]) %>% 
      html_table(fill = TRUE) %>% 
      colnames() %>% 
      extract2(1)
    if (test == "X1") {
      infonum[t] <- infonum[t] - 1
    }
  }
  
  # Get vector of event names and check to ensure each one dosen't 
  # already exist in the bouts df. If it does, then delete it from 
  # nameVect and edit resultsnum and infonum to delete the associated 
  # table index from those two vectors.
  nameVect <- getEventNames(tables, infonum)
  for (w in nameVect) {
    if (w %in% unique(bouts$Event)) {
      nameVect <- nameVect[-grep(w, nameVect)]
      resultsnum <- resultsnum[-grep(w, nameVect)]
      infonum <- infonum[-grep(w, nameVect)]
    }
  }
  
  # Check to make sure at least one table has been positively IDed.
  if (is.na(resultsnum) || length(resultsnum) < 1 || resultsnum == 0) {next}
  
  # Extract the dates that the events took place, record the index of the 
  # table within which the event date was found, and record and indication as to
  # the format of the date.
  dateVect <- getDateBouts(tables, infonum)
  
  # Get vector of venue names.
  venueVect <- getVenue(tables, infonum, dateVect[[2]])
  
  # Get city, state, and country, saved as elements of a list.
  locVect <- getLoc(tables, infonum, dateVect[[2]])
  
  # Create holder df, which will house all scraped data for the events on
  # a single page, then rbind them to bouts df. 
  holderdf <- appendDF(tables, resultsnum, nameVect, dateVect, venueVect, locVect)
  bouts <- rbind(bouts, holderdf)
}

# Turn global warnings back on
options(warn = oldw)


## Transformations and Clean Up ----
# Reset the row indices
rownames(bouts) <- NULL

# Normalize encoding for strings (to facilitate matching).
# The raw non-US fighter/location strings are very messy.
# Two step process for each variable, first step converts from utf-8 to LATIN1. 
# Second step converts to ASCII//TRANSLIT, thereby eliminating accent marks.
bouts$Weight <- utfConvert(bouts, 1)
bouts$FighterA <- utfConvert(bouts, 2)
bouts$FighterB <- utfConvert(bouts, 4)
bouts$Event <- utfConvert(bouts, 9)
bouts$Venue <- utfConvert(bouts, 11)
bouts$City <- utfConvert(bouts, 12)

# Eliminate all observations associated with future UFC events.
bouts <- bouts[bouts$Result != "" & !is.na(bouts$Result), ]

# Change all values of "N/A" and to be NA.
bouts[bouts[sapply(bouts, is.character)] == "N/A" & 
        !is.na(bouts[sapply(bouts, is.character)]), grep("N/A", bouts)] <- NA

# Eliminate variable "Notes".
bouts <- bouts[, -c(which(colnames(bouts)=="Notes"))]

# Create a new variable, "Belt". For all championship fights, record within 
# Belt the winner, weight class, and whether it was for Interim or Championship.
bouts$Belt <- NA
for (i in seq_len(nrow(bouts))) {
  if (grepl("\\(ic)", bouts$FighterA[i]) || 
      grepl("\\(ic)", bouts$FighterB[i])) {
    bouts$Belt[i] <- paste(
      bouts$FighterA[i], bouts$Weight[i], "Interim", sep = ", ")
  }
  if (grepl("\\(c)|\\(UFC Champion)", bouts$FighterA[i]) || 
      grepl("\\(c)|\\(UFC Champion)", bouts$FighterB[i])) {
    bouts$Belt[i] <- paste(
      bouts$FighterA[i], bouts$Weight[i], "Champion", sep = ", ")
  }
}

# Eliminate all championship tags from strings within variables 
# FighterA, FighterB, and Belt.
bouts$FighterA <- strEliminate(
  bouts, 2, " \\(Fighter)| \\(c)| \\(ic)| \\(UFC Champion)| \\(Pride Champion)")
bouts$FighterB <- strEliminate(
  bouts, 4, " \\(Fighter)| \\(c)| \\(ic)| \\(UFC Champion)| \\(Pride Champion)")
bouts$Belt <- strEliminate(
  bouts, 14, " \\(Fighter)| \\(c)| \\(ic)| \\(UFC Champion)| \\(Pride Champion)")

# Add variable "TotalSeconds" showing total seconds of fight time for each bout.
bouts$Round <- as.numeric(bouts$Round)
bouts$TotalSeconds <- NA
bouts$TotalSeconds <- unname(
  apply(
    bouts[, c('Time', 'Round')], 1, function(x) boutSeconds(x[1], as.double(x[2]))))

# Split the results variable into two seperate variables ("Results" & "Subresult")
bouts$Subresult <- NA
for (i in seq_len(nrow(bouts))) {
  x <- vectSplit(bouts$Result[i])
  bouts$Result[i] <- x[[1]]
  bouts$Subresult[i] <- x[[2]]
}

# For all fights that ended in a draw, edit variable subresult 
# to include the word "draw".
for (i in seq_len(nrow(bouts))) {
  if (bouts$Result[i] == "Draw" && 
      !grepl("draw", bouts$Subresult[i]) && 
      !is.na(bouts$Subresult[i])) {
    bouts$Subresult[i] <- paste0(bouts$Subresult[i], " draw")
  } else if (bouts$Result[i] == "Draw" && 
             is.na(bouts$Subresult[i])) {
    bouts$Subresult[i] <- "draw"
  }
}

# For any remaining NA's within variable Subresult, attempt to 
# unpack info from variable Result to fill in that NA gap.
for (i in seq_len(nrow(bouts))) {
  if (is.na(bouts$Subresult[i]) && 
      grepl("[Uu]nanimous|[Ss]plit|[Mm]ajority", bouts$Result[i])) {
    holder <- unlist(strsplit(bouts$Result[i], " "))
    if (any(grepl("[Dd]raw", holder)) && length(holder) > 1) {
      bouts$Result[i] <- "Draw"
      bouts$Subresult[i] <- tolower(paste(holder[1], holder[2]))
    } else if (any(grepl("[Dd]ecision", holder)) && length(holder) > 1) {
      bouts$Result[i] <- "Decision"
      bouts$Subresult[i] <- tolower(holder[1])
    }
  }
}

# Clean up variables Result and Subresult by combining similar catigories of data
bouts[grepl("[Ss]ubmission", bouts$Result), ]$Result <- "Submission"
bouts[grepl("dq|DQ", bouts$Result), ]$Result <- "Disqualification"
bouts[bouts$Subresult == "rear naked choke" & 
        !is.na(bouts$Subresult), ]$Subresult <- "rear-naked choke"

# Add variables for all types of over/under, ITD, and ended in r1-r5
# For these variables, r = "round", ITD = "inside the distance".
id <- ncol(bouts)
bouts$over1.5r <- ifelse(bouts$TotalSeconds > 450, 1, 0)
bouts$over2.5r <- ifelse(bouts$TotalSeconds > 750, 1, 0)
bouts$over3.5r <- ifelse(bouts$TotalSeconds > 1050, 1, 0)
bouts$over4.5r <- ifelse(bouts$TotalSeconds > 1350, 1, 0)
bouts$ITD <- ifelse(
  !grepl("^Decision|Draw|No Contest", bouts$Result), 1, ifelse(
    grepl("No Contest", bouts$Result) & 
      (bouts$TotalSeconds != 900 & bouts$TotalSeconds != 1500), 1, 0))
bouts$r1Finish <- ifelse(bouts$Round == 1, 1, 0)
bouts$r2Finish <- ifelse(bouts$Round == 2, 1, 0)
bouts$r3Finish <- ifelse(bouts$ITD == 1 & bouts$Round == 3, 1, 0)
bouts$r4Finish <- ifelse(bouts$Round == 4, 1, 0)
bouts$r5Finish <- ifelse(bouts$ITD == 1 & bouts$Round == 5, 1, 0)
bouts[bouts$Result == "No Contest" & bouts$Subresult != "overturned" & 
        !is.na(bouts$Result) & !is.na(bouts$Subresult), (id + 1):ncol(bouts)] <- NA
bouts[is.na(bouts$Round), (id + 1):ncol(bouts)] <- NA

# Reorder the variables
goodCols <- c("Weight", 
              "FighterA",	
              "VS",	
              "FighterB",	
              "Result",	
              "Subresult", 
              "Round", 
              "Time",	
              "TotalSeconds",	
              "Event", 
              "Date",	
              "Venue", 
              "City",	
              "State", 
              "Country", 
              "Belt",	
              "over1.5r",	
              "over2.5r",	
              "over3.5r",	
              "over4.5r",	
              "ITD", 
              "r1Finish",	
              "r2Finish",	
              "r3Finish",	
              "r4Finish",	
              "r5Finish")
bouts <- subset(bouts, select = goodCols)

# Save objects that will be sourced and used in other scripts within this repo.
save(bouts, fighterlinks, file = "~/mma_scrape/0-ufc_bouts.RData")
