# Clean environment
rm(list=ls())

# Set Working Directory
setwd("~/GitHub/Coding-Sample")

# Set path
path <- "~/GitHub/Coding-Sample"

# Load Libraries
library(tidyverse)
library(rvest)
library(lubridate)
library(tidytext)
library(textdata)
library(sentimentr)
library(countrycode)

#--------------------------- Scrapping Function -------------------------------#

scrape_who_africa <- function(year, month) {
  art_list <- list()
  continue_run <- TRUE
  i <- 0
  
  while(continue_run) {
    
    url_base <- "https://www.afro.who.int/news/news-releases?page="
    page_url <- paste0(url_base, i)
    response <- read_html(paste0(page_url))
    
    # Get links
    url_links <- response |>
      html_elements("a") |>
      html_attr("href")
    
    # Get text
    url_text <- response |> 
      html_elements("a") |> 
      html_text()
    
    url_links <- url_links[grepl("Read more", url_text)]
    
    # Get dates
    url_dates <- response |>
      html_elements(".date") |>
      html_text () |>
      str_extract("[0-9]{1,2} [A-Za-z]+ [0-9]{4}") |>
      as.Date("%d %B %Y")
    
    # Build a dataset with links and dates
    articles <- data.frame(date = url_dates,
                           link = url_links)
    
    art_list[[i+1]] <- articles |>
      filter(date >= as.Date(paste(year, month, 01, sep = "-")))
    
    i <- i +1
    
    if(!all(url_dates >= as.Date(paste(year, month, 01, sep = "-")))) {
      continue_run <- FALSE
    }
  }
  
  who_articles <- do.call(rbind, art_list)
  
  # Save them in .csv
  write.csv(who_articles, file = "who_articles.csv", row.names = FALSE)
  
  for(i in 1:nrow(who_articles)) {
    other_response <- read_html(paste0("https://www.afro.who.int/", who_articles$link[i]))
    
    article_text <- other_response |>
      html_elements(".col-md-9 p") |>
      html_text()
    
    file_name <- file(paste0("who", "-", i, ".txt"))
    writeLines(article_text, file_name)
    close(file_name)
    
    i <- i + 1
    
  }
  
}


#--- Getting all the news releases of WHO Africa from September 2023 to today ---#

# Define from which date we need the files

scrape_who_africa(2023, 9)

# Adding files to a build a data frame

articles <- tibble(file_name = character(), text = character())

# Loop to call every txt file and add it into the data frame

for(i in 1:30) {
  file_path <- paste0("who-", i, ".txt")
  
  text_content <- readLines(file_path, warn = FALSE) # Googled how to use read txt to put them into a data frame
  text_content <- paste(text_content, collapse = " ")
  
  articles <- bind_rows(articles, tibble(file_name = file_path, text = text_content))
}

#--- Sentiment Analysis---#

setwd("~/GitHub/problem-set-3-caigodelcielo/plots/")

## Overall Sentiment Analysis ##

# Prepare data for Sentiment Analysis - Tokenize

who_articles_tokens <- unnest_tokens(articles, word_tokens, text, token = "words")
who_articles_tokens <- anti_join(who_articles_tokens, stop_words, by = c("word_tokens" = "word"))

# Summary information

top_words_2 <- head(count(who_articles_tokens, word_tokens, sort = TRUE))
view(top_words_2)

# Describe the sentiment of the article

sentiment_nrc   <- get_sentiments("nrc") |>
  rename(nrc = sentiment)
sentiment_afinn <- get_sentiments("afinn") |>
  rename(afinn = value)
sentiment_bing  <- get_sentiments("bing") |>
  rename(bing = sentiment)

who_articles_tokens <- who_articles_tokens |>
  left_join(sentiment_nrc,
            by = c("word_tokens" = "word")) |>
  left_join(sentiment_afinn,
            by = c("word_tokens" = "word")) |>
  left_join(sentiment_bing,
            by = c("word_tokens" = "word"))

# Plot sentiment - Overall

sentiment_plot1 <- ggplot(data = filter(who_articles_tokens, !is.na(afinn))) +
  geom_histogram(aes(afinn), fill = "mediumslateblue", stat = "count") +
  scale_x_continuous(n.breaks = 7) +
  labs(title = "WHO news releases",
       subtitle = "From September 2023 to date",
       x = "Sentiment", y = "Count") 

ggsave("question2_plot_1.png", 
       plot = sentiment_plot1, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

sentiment_plot2 <- ggplot(data = filter(who_articles_tokens, !is.na(bing))) +
  geom_histogram(aes(bing), fill = "seagreen2", stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "WHO news releases",
       subtitle = "From September 2023 to date",
       x = "Sentiment", 
       y = "Count")

ggsave("question2_plot_2.png", 
       plot = sentiment_plot2, 
       width = 10, 
       height = 8, 
       units = "in",
       dpi = 300)

# Define country for specific sentiment analysis

country_names <- countrycode::codelist$country.name.en
country_names <- tolower(country_names)

country_names <- data.frame(country = country_names)

countries_discussed <- who_articles_tokens |>
  semi_join(country_names, by = c("word_tokens" = "country")) |>
  select(word_tokens) |>
  group_by(word_tokens) |> # Grouping by country to see individual countries
  summarize(n = n()) |>
  arrange(desc(n)) # Ordering them to see how much each country is mentioned

print(countries_discussed) # I will select Kenya

# Select data for sentiment analysis of news from Kenya (selected country)

kenya_who <- list()

for (i in 1:nrow(articles)) {
  if (grepl("Kenya", articles$text[i], ignore.case = TRUE)) {
    kenya_who[[length(kenya_who) + 1]] <- articles$text[i]
  }
}

# Convert the list to a data frame (if desired)
kenya_who_texts <- data.frame(text = unlist(kenya_who))

kenya_who_tokens <- unnest_tokens(kenya_who_texts, word_tokens, text, token = "words")
kenya_who_tokens <- anti_join(kenya_who_tokens, stop_words, by = c("word_tokens" = "word"))

# Summary information

top_words_3 <- head(count(kenya_who_tokens, word_tokens, sort = TRUE))
view(top_words_3)

# Sentiment Analysis - Kenya

kenya_who_tokens <- kenya_who_tokens |>
  left_join(sentiment_nrc,
            by = c("word_tokens" = "word")) |>
  left_join(sentiment_afinn,
            by = c("word_tokens" = "word")) |>
  left_join(sentiment_bing,
            by = c("word_tokens" = "word"))

# Plot sentiment - Kenya

sentiment_plot3 <- ggplot(data = filter(kenya_who_tokens, !is.na(nrc))) +
  geom_histogram(aes(nrc), fill = "indianred", stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "WHO news releases",
       subtitle = "Kenya",
       x = "Sentiment", y = "Count") 

ggsave("question2_plot_3.png", 
       plot = sentiment_plot3, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

sentiment_plot4 <- ggplot(data = filter(kenya_who_tokens, !is.na(bing))) +
  geom_histogram(aes(bing), fill = "orchid3", stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "WHO news releases",
       subtitle = "Kenya",
       x = "Sentiment", 
       y = "Count")

ggsave("question2_plot_4.png", 
       plot = sentiment_plot4, 
       width = 10, 
       height = 8, 
       units = "in",
       dpi = 300)



