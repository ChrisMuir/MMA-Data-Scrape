## Wiki UFC Fighter Details Scraper.
## This will scrape the details of every fighter that: 1) has ever fought in the
## UFC and 2) has a Wikipedia page. Fighter details include: age, height, reach,
## wins/losses, nationality, team/camp association, etc.
## Output is data_frame object "fighters".

## Load packages and do some prep work ----
# Read in required packages and files.
devtools::install_github("ChrisMuir/refinr")
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
source("./mma_scrape/wiki_ufcfighters_functions.R")
load("./mma_scrape/0-ufc_bouts.RData")

# Start with object "fighterlinksvect", a vector of partial url strings that
# that the wiki_ufcbouts scraper encountered while running.
# Trim down to only keep unique values.
fighterlinksvect <- unique(fighterlinksvect)

# Eliminate specific string patterns.
ids <- !grepl("/wiki/[A-Z][A-Z][A-Z]_|cite_note|/w/index.php|/wiki/\\d\\d\\d\\d_", 
            fighterlinksvect)
fighterlinksvect <- fighterlinksvect[ids]

# Append each url string to form a complete url for each fighter.
fighterlinksvect <- sapply(
  fighterlinksvect, function(x) paste0("https://en.wikipedia.org", x), 
  USE.NAMES = FALSE)

## Start the scraping ----
## Scraping is all contained within the for loop below.
# Turn off warnings, they will be turned back on after the loop finishes.
fighters <- data_frame()
oldw <- getOption("warn")
options(warn = -1)
for (i in fighterlinksvect) {
  # Read html and extract all tables within the page.
  tables <- xml2::read_html(i) %>% 
    html_nodes('table')
  if (is(tryCatch(html_table(tables, fill = TRUE), 
                  error=function(e) e), "error")) {
    id <- vector()
    for (k in seq_len(length(tables))) {
      x <- tryCatch(html_table(tables[k], fill = TRUE), error=function(e) e)
      if (is(x, "error")) {
        id <- c(id, match(tables[k], tables))
      }
    }
    tables <- tables[-id]
  }
  tables <- html_table(tables, fill = TRUE)
  
  # ID the correct table (fight results and fighter info).
  tab1 <- 0
  for (k in seq_len(length(tables))) {
    for (w in seq_len(ncol(tables[[k]]))) {
      if (any(grepl("^Mixed martial arts record$", tables[[k]][[w]]))) {
        tab1 <- k
        id <- w
        break
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
  holderdf <- holderdf[complete.cases(holderdf), ]
  holderdf <- holderdf[holderdf$col1 != "", ]
  row.names(holderdf) <- NULL
  if (anyDuplicated(holderdf$col1) > 0) {
    holderdf <- holderdf[-anyDuplicated(holderdf$col1), ]
  }
  
  # Use spread to transpose df fighter, such that all values in col1 are now 
  # headers, then use full_join to append it to the output df fighters.
  if(nrow(fighters) > 0) {
    fighters <- suppressMessages(
      full_join(fighters, spread(holderdf, col1, col2)))
  } else if (any(grepl("^mma Total$", holderdf$col1))) {
    fighters <- spread(holderdf, col1, col2)
  }
}

# Turn global warnings back on
options(warn = oldw)


## Data Transformations/Cleanup ----
# Eliminate variables that contain too many NA's 
# (that have less than five non-NA values).
fighters <- fighters[, which(
  unname(lapply(fighters, function(x) sum(!is.na(x))) > 5))]

# Eliminate wiki citations (brackets with text/numbers).
fighters <- as_data_frame(lapply(fighters, function(x) gsub("\\[.*\\]", "", x)))

# For observations in which Name is NA, attempt to unpack the fighter name
# from variable wikilink.
fighters$Name <- sapply(seq_len(nrow(fighters)), function(x) {
  if (is.na(fighters$Name[x])) {
    return(getName(fighters, "wikilink", x, fighters$Name[x]))
  } else {
    return(fighters$Name[x])
  }
}, USE.NAMES = FALSE)

# Unpack and convert strings to birth date of the fighter within variable Born.
fighters$Born <- fighters$Born %>% 
  sapply(., function(x) getDateFighters(x), USE.NAMES = FALSE) %>% 
  as.Date(., origin = "1970-01-01")

# For each fighter, differentiate between current division and all previous 
# divisions. Achieve this by looking up the division of the fighter's most
# recent UFC fight within boutsdf, this value will be appended to new variable
# "current Division". The scraped variable "Division" within df fighters is a
# compilation of all the divisions the fighter has ever fought in throughout
# their MMA career.
if (exists("boutsdf")) {
  fighters$`Current Division` <- sapply(fighters$Name, function(i) {
    x <- getDivision(i, boutsdf)
    if (is.na(x) || length(x) < 1) {
      x <- getDivision2(i)
    }
    return(x)
  }, USE.NAMES = FALSE)
} else {
  fighters$`Current Division` <- sapply(fighters$Name, getDivision2, 
                                        USE.NAMES = FALSE)
}

# For each fighter, unpack and convert height strings to height in inches.
fighters$Height <- sapply(fighters$Height, getHeight, USE.NAMES = FALSE)
colnames(fighters)[which(colnames(fighters) == "Height")] <- "Height in Inches"

# For each fighter, unpack and convert reach strings to reach in inches.
fighters$Reach <- sapply(fighters$Reach, getReach, USE.NAMES = FALSE)
colnames(fighters)[which(colnames(fighters) == "Reach")] <- "Reach in Inches"

# For each fighter, unpack and convert weight strings to weight in lbs.
suppressWarnings(
  fighters$Weight <- sapply(fighters$Weight, getWeight, USE.NAMES = FALSE))
colnames(fighters)[which(colnames(fighters) == "Weight")] <- "Weight in Pounds"

# Within all columns of df fighters, replace line-split text with a comma, 
# and eliminate all quotation marks.
for(i in seq_len(ncol(fighters))) {
  fighters[[i]] <- fighters[[i]] %>% 
    gsub("\r\n|\n", ", ", .) %>% 
    gsub("\"", "", .)
}

# For all observations in which the fighter has zero mma wins, recode w-l 
# record from cols "Wins Losses" and "Wins Wins" to the correct cols.
if (any(colnames(fighters) == "Wins Losses")) {
  id <- which(!is.na(fighters$`Wins Losses`))
  for (i in id) {
    if (is.na(fighters$`mma Wins`[i])) {
      fighters$`mma Wins`[i] <- fighters$`Wins Wins`[i]
    }
    if (is.na(fighters$`mma Losses`[i])) {
      fighters$`mma Losses`[i] <- fighters$`Wins Losses`[i]
    }
  }
}


# Rename some of the variables.
namesold <- c("mma Losses", "mma Wins", "mma Total", "Losses No contests", 
              "mma Draws")
namesnew <- c("Total Losses", "Total Wins", "Total Fights", "No Contest", 
              "Draw")
fighters <- colRename(fighters, namesold, namesnew)

# Compile the names of the columns that we want to keep in a vector (goodCols), 
# then subset fighters df and reorder cols such that the variables of 
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
              "Died")

fighters <- subset(fighters, select = goodCols) %>% 
  .[order(.$Name), ]

# For columns 3 - 17, replace all NA's with zeros, then convert to numeric.
for (i in 3:17) {
  if (sum(is.na(fighters[[i]])) > 0) {
    fighters[is.na(fighters[[i]]), ][[i]] <- 0
  }
  fighters[[i]] <- as.double(fighters[[i]])
}

# Convert Height, Reach and Weight to numeric.
fighters[, c(19, 20, 23)] <- lapply(
  fighters[, c(19, 20, 23)], function(x) as.double(x))

# Additional text clean up. This step will merge values that are equivalent yet
# have different strings, i.e. "James Te-Huna" and "James Te Huna". This step 
# is applied to all fighter names across both fighters df and boutsdf, as well 
# as variables Team, Other names, Rank, Fighting out of, Residence, Style, and 
# Nationality. Once this step is complete, save objects fighters, boutsdf and 
# fighterlinksvect to RData files.
ids <- which(colnames(fighters) %in% c("Team", "Other names", "Rank", 
                                      "Fighting out of", "Residence", "Style", 
                                      "Nationality"))
fighters[, ids] <- lapply(
  fighters[, ids], function(x) {
    x %>% 
      {refinr::key_collision_merge(., bus_suffix = FALSE)} %>% 
      {refinr::n_gram_merge(., bus_suffix = FALSE)}
})
if (exists("boutsdf")) {
  dfs <- mergeFighterNames(fighters, boutsdf)
  fighters <- dfs[["fighters"]]
  boutsdf <- dfs[["boutsdf"]]
  save(fighters, file = "./mma_scrape/1-utf_fighters.RData")
  save(boutsdf, fighterlinksvect, file = "./mma_scrape/0-ufc_bouts.RData")
} else {
  fighters$Name <- fighters$Name %>% 
    {refinr::key_collision_merge(., bus_suffix = FALSE)} %>% 
    {refinr::n_gram_merge(., bus_suffix = FALSE)}
  save(fighters, file = "./mma_scrape/1-utf_fighters.RData")
}
