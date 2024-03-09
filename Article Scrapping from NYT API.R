# Clean environment
rm(list=ls())

# Set Working Directory
setwd("~/GitHub/Coding-Sample")

# Load Libraries
library(tidyverse)
library(jsonlite) # Because the extracted data is in json format
library(nytimes) # <- https://github.com/mkearney/nytimes

#---------------------Article Scrapping from NYT API---------------------------#

#--- Get data from NY Times ---#

NYTIMES_KEY <- "********************************" # The Key we need to access the API

# Search parameters

term <- "crime+chicago" 
begin_date <- "20200101" # We need data from 2020 to 2023
end_date <- "20231231"

# Set up the baseurl
baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY)

# Defining scrapping parameters
total_articles_limit <- 1000

articles_per_page <- 10 # This is the maximum per page
maxPages <- ceiling(total_articles_limit / articles_per_page) - 1


# Getting the articles
pages <- list() # Where we are going to store the articles
articles_collected <- 0

# Loop to call the API
for(i in 0:maxPages){
  if(articles_collected >= total_articles_limit) { 
    break # This is for the code to stop scrapping when we reach the articles_limit
  }
  
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) |>
    data.frame()
  message("Retrieving page ", i)
  
  if(nrow(nytSearch) > 0) {
    articles_collected <- articles_collected + nrow(nytSearch)
    pages[[i+1]] <- nytSearch 
  } else {
    
    break
  }
  
  Sys.sleep(60)  # We need this pause because we cannot retrieve more than 5 articles per minute, so NYT suggest making a pause between requests
}

# Turn json pages into a dataset
nyt_articles_chi <- bind_rows(pages)

list_cols <- sapply(nyt_articles_chi, is.list) # Since it is a json, it has lists, that won't allow us to turn it into a .csv
list_col_names <- names(list_cols[list_cols]) 
nyt_articles_chi <- nyt_articles_chi2[, !list_cols] # So we identify the list variables and drop them

# Save as .csv
write.csv(nyt_articles_chi, file = "nyt_articles_chi.csv", row.names = FALSE)

