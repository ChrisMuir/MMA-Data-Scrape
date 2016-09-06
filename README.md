UFC/MMA Scrape README File
========

Objective
---------

The purpose of this project is to scrape historical MMA data on fights and fighters, 
and clean up the data to make it as useful as possible. Project was written in R, 
using package rvest for scraping.

Files
-----

1. `0-wiki_ufcbouts.R` scrapes the results of every UFC fight from Wikipedia.
2. `1-wiki_ufcfighters.R` scrapes the details of every fighter that has ever fought 
in the UFC and has a Wikipedia page. Fighter details include: age, height, reach, wins/losses, nationality, team/camp association, etc.
3. `wiki_ufcbouts_func_calls.R` houses all custom functions used in ufcbouts scrape file.
4. `wiki_ufcfighters_func_calls.R` houses all custom functions used in ufcfighters scrape file.