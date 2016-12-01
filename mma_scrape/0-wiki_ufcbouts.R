## Wiki UFC Fight Results Scraper.
## This will scrape the results of every UFC fight from Wikipedia.
## Outputs are data_frame object "boutsdf", and vector "fighterlinksvect".

## Install packages and do some prep work ----
# Read in required packages & files.
library(rvest)
library(dplyr)
library(magrittr)
library(stringr)

source("~/mma_scrape/wiki_ufcbouts_functions.R")
datafile <- "~/mma_scrape/0-ufc_bouts.RData"
if (file.exists(datafile)) {
  load("~/mma_scrape/0-ufc_bouts.RData")
}

# Pull html from wiki page of all UFC events.
cards <- xml2::read_html("https://en.wikipedia.org/wiki/List_of_UFC_events")

# Extract all url strings.
cardlinks <- cards %>% 
  html_nodes('td:nth-child(2) a') %>% 
  html_attr('href') %>% 
  unique()

# Remove links for fight events that were canceled and thus have no 
# fight results.
cardlinks <- cardlinks[!grepl("UFC_176|UFC_151|Lamas_vs._Penn", cardlinks)]

# Append each url string to form a complete url for each event.
cardlinks <- unname(
  sapply(cardlinks, function(x) paste0("https://en.wikipedia.org", x)))

# If the wiki_ufcbouts DB already exists as an RData file, edit cardlinks to 
# only include urls that do not appear in the existing wiki_ufcbouts DB 
# (intention is to only scrape fight results that are new and haven't previously 
# been scraped).
if (file.exists(datafile)) {
  cardlinks <- cardlinks[!cardlinks %in% unique(boutsdf$wikilink)]
}

# Create vector of all of the months of the year 
# (with a single trailing space appended to each month).
allMonths <- paste0(
  months(seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), "month")), " ")

# Create vector of countries that have hosted UFC events.
countries <- cards %>% 
  html_nodes('td:nth-child(5)') %>% 
  html_text() %>% 
  sapply(., function(x) tail(strsplit(x, ", ")[[1]], n=1)) %>% 
  unname() %>% 
  c(., "England") %>% 
  unique()

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
  html <- xml2::read_html(i)
  
  # Record wiki links of all the fighters on the card.
  fighterlinks <- c(fighterlinks, html %>% 
                      html_nodes('.toccolours td a') %>% 
                      html_attr('href'))
  
  # Extract all tables within page, then do NA checks.
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
  nameVect <- getEventNames(tables, infonum) %>% 
    sapply(., function(x) utfConvert(x)) %>% 
    unname()
  if(any(nameVect %in% unique(c(bouts$Event, boutsdf$Event)))) {
    id <- which(nameVect %in% unique(c(bouts$Event, boutsdf$Event)))
    nameVect <- nameVect[-id]
    resultsnum <- resultsnum[-id]
    infonum <- infonum[-id]
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
  holderdf <- appendDF(tables, resultsnum, nameVect, dateVect, venueVect, 
                       locVect, i)
  bouts <- rbind(bouts, holderdf)
}

# Turn global warnings back on
options(warn = oldw)


## Transformations and Clean Up ----
# If the wiki_ufcbouts DB already exists as an RData file, eliminate elements 
# of the newly scraped fighterlinks that appear in fighterlinksvect.
if (file.exists(datafile)) {
  fighterlinks <- fighterlinks[!fighterlinks %in% unique(fighterlinksvect)]
}

# Eliminate all "TBA" and "TBD" entries within fighterlinks.
fighterlinks <- fighterlinks[!grepl("wiki/TBA|wiki/TBD", fighterlinks)]

# Reset the row indices
rownames(bouts) <- NULL

# Eliminate all observations associated with future UFC events.
bouts <- bouts[bouts$Result != "" & !is.na(bouts$Result), ]

# Change all values of "N/A" and to be NA.
bouts[bouts[sapply(bouts, is.character)] == "N/A" & 
        !is.na(bouts[sapply(bouts, is.character)]), 
      grep("N/A", bouts, fixed = TRUE)] <- NA

# Eliminate variable "Notes".
bouts <- bouts[, -c(which(colnames(bouts) == "Notes"))]

# Create a new variable, "Belt". For all championship fights, record within 
# Belt the winner, weight class, and whether it was for Interim or Championship.
bouts$Belt <- NA
# Interim
id <- c(grep("\\(ic)", bouts$FighterA, ignore.case = TRUE), 
        grep("\\(ic)", bouts$FighterB, ignore.case = TRUE)) %>% 
  .[order(.)]
bouts$Belt[id] <- paste(
  bouts$FighterA[id], bouts$Weight[id], "Interim", sep = ", ")
# Champion
id <- c(grep("\\(c)|\\(UFC Champion)", bouts$FighterA, ignore.case = TRUE), 
        grep("\\(c)|\\(UFC Champion)", bouts$FighterB, ignore.case = TRUE)) %>% 
  .[order(.)]
bouts$Belt[id] <- paste(
  bouts$FighterA[id], bouts$Weight[id], "Champion", sep = ", ")

# Eliminate all championship tags from strings within variables 
# FighterA, FighterB, and Belt.
id <- which(colnames(bouts) %in% c("FighterA", "FighterB", "Belt"))
bouts[, id] <- lapply(
  bouts[, id], function(x) 
    gsub(" \\(Fighter)| \\(c)| \\(ic)| \\(UFC Champion)| \\(Pride Champion)", 
         "", x, ignore.case = TRUE))


# Specify encoding for strings (to facilitate matching).
# The raw non-US fighter/location strings are very messy, and the application of 
# accent marks is inconsistent. Two step process for each variable, first step 
# converts from utf-8 to LATIN1. Second step converts to ASCII//TRANSLIT, 
# thereby eliminating accent marks.
id <- which(colnames(bouts) %in% c("Weight", "FighterA", "FighterB", "Event", 
                                   "Venue", "City", "Belt"))
bouts[, id] <- lapply(bouts[, id], function(x) utfConvert(x))

# Add variable "TotalSeconds" showing total seconds of fight time for each bout.
bouts$Round <- as.double(bouts$Round)
bouts$TotalSeconds <- mapply(function(x, y) 
  boutSeconds(x, y), bouts$Time, bouts$Round, USE.NAMES = FALSE)

# Split results variable into two seperate variables ("Results" & "Subresult")
rsltsplit <- unname(sapply(bouts$Result, function(x) vectSplit(x)))
bouts$Result <- unlist(rsltsplit[1, ])
bouts$Subresult <- unlist(rsltsplit[2, ])

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
      grepl("unanimous|split|majority", bouts$Result[i], ignore.case = TRUE)) {
    holder <- unlist(strsplit(bouts$Result[i], " "))
    if (any(grepl("draw", holder, ignore.case = TRUE, fixed = TRUE)) && 
        length(holder) > 1) {
      bouts$Result[i] <- "Draw"
      bouts$Subresult[i] <- tolower(paste(holder[1], holder[2]))
    } else if (any(grepl("decision", holder, ignore.case = TRUE, fixed = TRUE)) && 
               length(holder) > 1) {
      bouts$Result[i] <- "Decision"
      bouts$Subresult[i] <- tolower(holder[1])
    }
  }
}

# Clean up variables Result and Subresult by combining similar values.
if (any(grepl("submission", bouts$Result, fixed = TRUE))) {
  bouts[grepl("submission", bouts$Result, fixed = TRUE), ]$Result <- "Submission"
}
if (any(grepl("dq|DQ", bouts$Result))) {
  bouts[grepl("dq|DQ", bouts$Result), ]$Result <- "Disqualification"
}
if (!is.na(match("rear naked choke", bouts$Subresult))) {
  bouts[which(bouts$Subresult == "rear naked choke"), ]$Subresult <- 
    "rear-naked choke"
}

# Add variables for all types of over/under, ITD, and ended in r1-r5.
# For these variables, r = "round", ITD = "inside the distance".
id <- ncol(bouts)
bouts$over1.5r <- ifelse(bouts$TotalSeconds > 450, 1, 0)
bouts$over2.5r <- ifelse(bouts$TotalSeconds > 750, 1, 0)
bouts$over3.5r <- ifelse(bouts$TotalSeconds > 1050, 1, 0)
bouts$over4.5r <- ifelse(bouts$TotalSeconds > 1350, 1, 0)
bouts$ITD <- ifelse(
  !grepl("^Decision|Draw|No Contest", bouts$Result), 1, ifelse(
    grepl("No Contest", bouts$Result) & 
      !bouts$TotalSeconds %in% c(900, 1500), 1, 0))
bouts$r1Finish <- ifelse(bouts$Round == 1, 1, 0)
bouts$r2Finish <- ifelse(bouts$Round == 2, 1, 0)
bouts$r3Finish <- ifelse(bouts$ITD == 1 & bouts$Round == 3, 1, 0)
bouts$r4Finish <- ifelse(bouts$Round == 4, 1, 0)
bouts$r5Finish <- ifelse(bouts$ITD == 1 & bouts$Round == 5, 1, 0)
bouts[which(bouts$Result == "No Contest" & 
              bouts$Subresult != "overturned"), (id + 1):ncol(bouts)] <- NA
bouts[is.na(bouts$Round), (id + 1):ncol(bouts)] <- NA

# Reorder the variables, and eliminate unwanted variables.
if (file.exists(datafile)) {
  goodCols <- colnames(boutsdf)
} else {
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
                "wikilink", 
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
}
bouts <- subset(bouts, select = goodCols)

# If updating an existing UFC bouts dataset, remove any observations within the 
# newly scraped data in which the event name appear in the existing UFC bouts 
# dataset.
if (file.exists(datafile)) {
  bouts <- bouts[!bouts$Event %in% unique(boutsdf$Event), ]
}

## Write results to file ----
# If UFC bouts dataset already exists in directory "mma_scrape", append the 
# newly scraped results to objects "boutsdf" and "fighterlinksvect", then save 
# both as an RData file.
# Otherwise, save the newly scraped results as an RData file.
# The RData file will be sourced at the top of R file "1-wiki_ufcfighters.R", 
# and that file will make use of object "fighterlinksvect".
if (!file.exists(datafile)) {
  boutsdf <- bouts
  fighterlinksvect <- fighterlinks
  save(boutsdf, fighterlinksvect, file = "~/mma_scrape/0-ufc_bouts.RData")
} else if (identical(colnames(bouts), colnames(boutsdf))) {
  boutsdf <- rbind(bouts, boutsdf)
  fighterlinksvect <- c(fighterlinks, fighterlinksvect)
  save(boutsdf, fighterlinksvect, file = "~/mma_scrape/0-ufc_bouts.RData")
} else {
  writeLines(c("ERROR: rbind of new data with old data failed,", 
               "columns of the two dataframes do not allign."))
}
