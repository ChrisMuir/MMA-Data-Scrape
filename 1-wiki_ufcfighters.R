## Wiki UFC Fighter Details Scraper.
## This will scrape the details of every fighter that: 1) has ever fought in the
## UFC and 2) has a Wikipedia page. Fighter details include: age, height, reach,
## wins/losses, nationality, team/camp association, etc.
## Output is data_frame object "fighters".

## Load packages and do some prep work ----
# Read in required packages and files.
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
source("~/mma_scrape/wiki_ufcfighters_func_calls.R")
load("~/mma_scrape/wiki_ufcbouts.RData")

# Start with object "fighterlinks", a vector of partial url strings that
# that the wiki_ufcbouts scraper encountered while running.
# Trim down to only keep unique values.
fighterlinks <- unique(fighterlinks)

# Eliminate specific string patterns.
x <- !grepl("/wiki/[A-Z][A-Z][A-Z]_|cite_note|/w/index.php|/wiki/\\d\\d\\d\\d_", 
            fighterlinks)
fighterlinks <- fighterlinks[x]

# Append each url string to form a complete url for each fighter.
fighterlinks <- unname(sapply(fighterlinks, function(x) paste0(
  "https://en.wikipedia.org", x)))

## Start the scraping ----
## Scraping is all contained within the for loop below.
# Turn off warnings, they will be turned back on after the loop finishes.
fighters <- data_frame()
oldw <- getOption("warn")
options(warn = -1)
for (i in fighterlinks) {
  # Read html and extract all tables within the page.
  tables <- read_html(i) %>% 
    html_nodes('table') %>% 
    html_table(fill = TRUE)
  
  # ID the correct table (fight results and fighter info).
  tab1 <- 0
  for (k in seq_len(length(tables))) {
    for (w in seq_len(ncol(tables[[k]]))) {
      if (any(grepl("^Mixed martial arts record$", tables[[k]][[w]]))) {
        tab1 <- k
        id <- w
      }
    }
  }
  
  # Check to make sure at least one table has been positively IDed.
  if (is.na(tab1) || length(tab1) < 1 || tab1 == 0) {next}
  
  # Name the selected table, eliminate unnecessary columns, append fighter name 
  # and wiki link to the df, name columns, clean up text encoding.
  holderdf <- tables[[tab1]]
  if(ncol(holderdf) > 2) {
    holderdf <- holderdf[, c(id, (id + 1))]
  }
  holderdf[nrow(holderdf)+1, ] <- c("Name", colnames(holderdf)[1])
  holderdf[nrow(holderdf)+1, ] <- c("wikilink", i)
  colnames(holderdf) <- c("col1", "col2")
  holderdf$col1 <- utfConvert(holderdf, 1)
  holderdf$col2 <- utfConvert(holderdf, 2)
  
  # Isolate the fighter's MMA win-loss record, emliminate any win-loss
  # records from other, non-MMA sports.
  holderdf <- mmaIso(holderdf, 1)
  
  # Eliminate rows that contain NAs, empty values, or are duplicates.
  holderdf <- na.omit(holderdf)
  holderdf <- holderdf[holderdf$col1 != "", ]
  row.names(holderdf) <- NULL
  if (anyDuplicated(holderdf$col1) > 0) {
    holderdf <- holderdf[-anyDuplicated(holderdf$col1), ]
  }
  
  # Use spread to transpose df fighter, such that all values in col1 are now 
  # headers, then use full_join to append it to the output df fighters.
  if(nrow(fighters) > 0) {
    fighters <- suppressMessages(full_join(fighters, spread(holderdf, col1, col2)))
  } else if (any(grepl("^mma Total$", holderdf$col1)) == TRUE) {
    fighters <- spread(holderdf, col1, col2)
  }
}

# Turn global warnings back on
options(warn = oldw)


## Data Transformations/Cleanup ----
# Eliminate wiki citations (brackets with text/numbers).
for(i in seq_len(ncol(fighters))) {
  fighters[[i]] <- fighters[[i]] %>% 
    gsub("\\[.*\\]", "", .)
}

# For observations in which Name is NA, attempt to unpack the fighter name
# from variable wikilink.
for (i in seq_len(nrow(fighters))) {
  if (is.na(fighters$Name[i])) {
    fighters$Name[i] <- getName(
      fighters, "wikilink", i, fighters$Name[i])
  }
}

# Unpack and convert strings to birth date of the fighter within variable Born.
fighters$Born <- unname(
  sapply(fighters$Born, function(x) getDateFighters(x)))
fighters$Born <- as.Date(fighters$Born, origin = "1970-01-01")

# For each fighter, differentiate between current division and all previous 
# divisions. Achieve this by looking up the division of the fighter's most
# recent UFC fight within df bouts, this value will be appended to new variable
# "current Division". The scraped variable "Division" within fighters is a
# compilation of all the divisions the fighter has ever fought in throughout
# their MMA career.
fighters$`Current Division` <- NA
for (i in seq_len(nrow(fighters))) {
  x <- getDivision(fighters$Name[i])
  if (is.na(x) || length(x) < 1) {
    x <- getDivision2(fighters$Name[i])
  }
  fighters$`Current Division`[i] <- x
}

# For each fighter, unpack and convert height strings to height in inches.
fighters$Height <- unname(
  sapply(fighters$Height, function(x) getHeight(x)))
colnames(fighters)[colIndex("Height", fighters)] <- "Height in Inches"

# For each fighter, unpack and convert reach strings to reach in inches.
fighters$Reach <- unname(
  sapply(fighters$Reach, function(x) getReach(x)))
names(fighters)[colIndex("Reach", fighters)] <- "Reach in Inches"

# For each fighter, unpack and convert weight strings to weight in lbs.
fighters$Weight <- suppressWarnings(
  unname(sapply(fighters$Weight, function(x) getWeight(x))))
colnames(fighters)[colIndex("Weight", fighters)] <- "Weight in Pounds"

# Within all columns of df fighters, replace line-split text with a comma, 
# and eliminate all quotation marks.
for(i in seq_len(ncol(fighters))) {
  fighters[[i]] <- fighters[[i]] %>% 
    gsub("\r\n|\n", ", ", .) %>% 
    gsub("\"", "", .)
}

# Rename some of the variables.
namesold <- c("mma Losses", "mma Wins", "mma Total", "Losses No contests", "mma Draws")
namesnew <- c("Total Losses", "Total Wins", "Total Fights", "No Contest", "Draw")
fighters <- colRename(fighters, namesold, namesnew)

# Compile the names of the columns that we want to keep in a vector (goodCols), 
# then subset fighters and reorder cols such that the variables of 
# fighters match the names in goodCols, as well as the order in which they 
# apear in goodCols.
goodCols <- c("Name",	
              "Current Division",	
              "Total Fights",	
              "Total Wins",	
              "Wins By knockout",	
              "Wins By submission",	
              "Wins By decision",	
              "Wins By disqualification",	
              "Wins Unknown",	
              "Total Losses",	
              "Losses By knockout",	
              "Losses By submission",	
              "Losses By decision",	
              "Losses By disqualification",	
              "Losses Unknown",	
              "No Contest",	
              "Draw",	
              "Born",	
              "Height in Inches",	
              "Reach in Inches", 
              "Team",	
              "Trainer", 
              "Weight in Pounds",	
              "Division",	
              "Other names", 
              "Rank",	
              "wikilink",	
              "Years active",	
              "Fighting out of", 
              "Notable relatives", 
              "Residence", 
              "Style", 
              "Nationality", 
              "Notable school(s)", 
              "Stance",	
              "University",	
              "Wrestling", 
              "Website", 
              "Teacher(s)",	
              "Ethnicity", 
              "Notable students",	
              "Children",	
              "Spouse",	
              "Occupation",	
              "Died",	
              "Allegiance",	
              "Battles/wars",	
              "Service/branch",	
              "Unit",	
              "Years of service",	
              "Alma mater",	
              "Doctoral advisor",	
              "Fields",	
              "Notable awards",	
              "Thesis",	
              "Other work",	
              "Notable club(s)", 
              "College", 
              "Status")

fighters <- subset(fighters, select = goodCols)

# For columns 3 - 17, convert to numeric, then replace all NA's with zeros.
for (i in 3:17) {
  if (sum(is.na(fighters[[i]])) > 0) {
    fighters[is.na(fighters[[i]]), ][[i]] <- 0
  }
  fighters[[i]] <- as.double(fighters[[i]])
}

# Save objects that will be sourced and used in other scripts within this repo.
save(fighters, file = "~/mma_scrape/1-ufc_fighters.RData")