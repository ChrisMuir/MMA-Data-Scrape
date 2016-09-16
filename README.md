UFC/MMA Scrape README File
========

Objective
---------

The purpose of this project is to scrape historical MMA data on fights and fighters, 
clean the data, and create new feature variables to make it as useful as possible. 
Project was written in R, using package rvest for scraping.

Files
-----

1. `0-wiki_ufcbouts.R` scrapes the results of every UFC fight from Wikipedia.
2. `1-wiki_ufcfighters.R` scrapes the details of every fighter that has ever fought 
in the UFC and has a Wikipedia page. Fighter details include: age, height, reach, wins/losses, nationality, team/camp association, etc.
3. `wiki_ufcbouts_functions.R` houses all custom functions used in ufcbouts scrape file.
4. `wiki_ufcfighters_functions.R` houses all custom functions used in ufcfighters scrape file.

Instructions
------------

Save folder `mma_scrape` to your current working directory. There are two scrape files 
(`0-wiki_ufcbouts.R` and `1-wiki_ufcfighters.R`), file `0` must be run prior to running 
file `1`. The two function call files provide functions used for scraping and are 
sourced at the top of each of the scrape files. The output of each scrape file is 
a dataframe saved as an .RData file to the folder `mma_scrape`.

Notes
-----

The majority of the code is performing text clean up, text extraction, tidying 
variables and creating new feature variables. I'm planning on add more to this in the near future
(scraping historical judging data for all MMA fights, merging of datasets, date specific
scraping to allow for updating an existing scraped dataset).