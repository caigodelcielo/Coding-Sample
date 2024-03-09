
# Clean environment
rm(list=ls())

# Set Working Directory
setwd("~/GitHub/Coding-Sample")

# Set path
path <- "~/GitHub/Coding-Sample"

# Load Libraries
library(tidyverse)
library(readr)
library(rvest)
library(lubridate)
library(tidytext)
library(textdata)
library(sentimentr)
library(wesanderson)

#---------------------Sentiment Analysis NYT - Chicago-------------------------#

###---Prepare Data---###

# Loading Data
chicago_articles <- read.csv("nyt_articles_chi.csv")

# Filtering articles
chicago_pre <- chicago_articles |>
  # Keeping only the types and sections of articles that are relevant
  filter(response.docs.document_type == "article") |>
  # Selecting variables
  select(response.docs.abstract, response.docs.snippet, response.docs.lead_paragraph, 
         response.docs.pub_date, response.docs.word_count, response.docs.headline.main) 

# Renaming variables
  names(chicago_pre) <- gsub("response.docs.", "", names(chicago_pre)) 
  # removing a part of the variable names that come from NYT API

# Preparing data for analysis
  chicago_pre <- chicago_pre |> 
    # Create new date variables
    mutate(pub_date = as.Date(ymd_hms(pub_date)),
           year = as.factor(year(pub_date)), 
           month = month(pub_date), 
           day = day(pub_date),
           id = str_pad(row_number(), width = 3, pad = "0")) |>
    # Modifying dataset for analysis
    pivot_longer(cols = c(abstract, snippet, lead_paragraph),
                 names_to = "type",
                 values_to = "text")
  
# Create different datasets for different type of text (to try which one works best)
  
  # Dataset for abstracts
  chicago_articles_abstract <- chicago_pre |>
    filter(type == "abstract") |>
    select(pub_date, text, year, month, day, id)

  # Dataset for snippets  
  chicago_articles_snippet <- chicago_pre |>
    filter(type == "snippet") |>
    select(pub_date, text, year, month, day, id)

    # Dataset for lead_paragraphs  
  chicago_articles_leadp <- chicago_pre |>
    filter(type == "lead_paragraph") |>
    select(pub_date, text, year, month, day, id)
  
  # Load Sentiments
  sentiment_nrc   <- get_sentiments("nrc") |>
    rename(nrc = sentiment)
  sentiment_afinn <- get_sentiments("afinn") |>
    rename(afinn = value)
  sentiment_bing  <- get_sentiments("bing") |>
    rename(bing = sentiment)
  
  
####---Sentiment Analysis---####
  
  #### Lead Paragraph ####
  
  # Tokenize - Words
  word_tokens_leadp <- chicago_articles_leadp |>
    unnest_tokens(word_tokens,  text, token = "words") |>
    anti_join(stop_words, by = c("word_tokens" = "word"))
  
  # Describe the sentiment of the text
  word_tokens_leadp <- word_tokens_leadp|>
    left_join(sentiment_nrc,
              by = c("word_tokens" = "word")) |>
    left_join(sentiment_afinn,
              by = c("word_tokens" = "word")) |>
    left_join(sentiment_bing,
              by = c("word_tokens" = "word"))
  
  # Save as .csv
  write.csv(word_tokens_leadp, file = "data/leadp_words_sentiment.csv", row.names = FALSE)
  
  # Plot sentiment - Lead Paragraph
  
  leadp_words_afinn <- ggplot(data = filter(word_tokens_leadp, !is.na(afinn))) +
    geom_histogram(aes(afinn), fill = "violet", stat = "count") +
    scale_x_continuous(n.breaks = 7) +
    labs(title = "Crime in Chicago NYT Articles",
         subtitle = "From 2020 to 2023",
         x = "Sentiment", y = "Count") 
  
  ggsave("images/leadp_words_afin.png", 
         plot = leadp_words_afinn, 
         width = 10, 
         height = 8, 
         units = "in", 
         dpi = 300)
  
  
### Data per day ###
  
  # Lead P
  
  leadp_tokens_day <- word_tokens_leadp |>
    group_by(id, pub_date) |>
    summarize(afinn_mean = mean(afinn, na.rm = TRUE))
  
  # Save as .csv
  write.csv(leadp_tokens_day, file = "data/leadp_tokens_day.csv", row.names = FALSE)
  
### Data per year

# Plot
    palette <- wes_palette("Zissou1", n = length(unique(word_tokens_leadp$year)), type = "discrete")
  
  leadp_words_afinn_yearly <- ggplot(data = filter(word_tokens_leadp, !is.na(afinn)), 
                                        aes(x = afinn, fill = year)) +
    geom_histogram(position = "dodge", binwidth = 1) +
    scale_fill_manual(values = palette) + 
    scale_x_continuous(n.breaks = 7) +
    labs(title = "Crime in Chicago NYT Articles",
         subtitle = "Sentiment Distribution by Year (2020 to 2023)",
         x = "Sentiment Score (Afinn)", y = "Count") +
    theme_minimal() 
  
  print(leadp_words_afinn_yearly)
  
  ggsave("images/leadp_words_afinn_yearly.png", 
         plot = leadp_words_afinn_yearly, 
         width = 15, 
         height = 8, 
         units = "in", 
         dpi = 300)
  
#######################################################################################
  
# Other options to analyze
  
  #- Abstract -#
  
  # Tokenize - Words
  word_tokens_abstract <- chicago_articles_abstract |>
    unnest_tokens(word_tokens,  text, token = "words") |>
    anti_join(stop_words, by = c("word_tokens" = "word"))
  

  word_tokens_abstract <- word_tokens_abstract |>
    left_join(sentiment_nrc,
              by = c("word_tokens" = "word")) |>
    left_join(sentiment_afinn,
              by = c("word_tokens" = "word")) |>
    left_join(sentiment_bing,
              by = c("word_tokens" = "word"))
  
  # Plot sentiment - Abstract, Overall
  
  abstract_words_afinn_overall <- ggplot(data = filter(word_tokens_abstract, !is.na(afinn))) +
    geom_histogram(aes(afinn), fill = "mediumslateblue", stat = "count") +
    scale_x_continuous(n.breaks = 7) +
    labs(title = "Crime in Chicago NYT Articles",
         subtitle = "From 2020 to 2023",
         x = "Sentiment", y = "Count")
  
  ggsave("images/abstract_words_afin_overall.png", 
         plot = abstract_words_afinn_overall, 
         width = 10, 
         height = 8, 
         units = "in", 
         dpi = 300)
  
  
  #- Snippet -#
  
  # Tokenize - Words
  word_tokens_snippet <- chicago_articles_snippet |>
    unnest_tokens(word_tokens,  text, token = "words") |>
    anti_join(stop_words, by = c("word_tokens" = "word"))
  
  # Describe the sentiment of the text
  word_tokens_snippet <- word_tokens_snippet|>
    left_join(sentiment_nrc,
              by = c("word_tokens" = "word")) |>
    left_join(sentiment_afinn,
              by = c("word_tokens" = "word")) |>
    left_join(sentiment_bing,
              by = c("word_tokens" = "word"))
  

  # Plot sentiment - Snippet
  
  snippet_words_afinn <- ggplot(data = filter(word_tokens_snippet, !is.na(afinn))) +
    geom_histogram(aes(afinn), fill = "cornflowerblue", stat = "count") +
    scale_x_continuous(n.breaks = 7) +
    labs(title = "Crime in Chicago NYT Articles",
         subtitle = "From 2020 to 2023",
         x = "Sentiment", y = "Count")
  
  ggsave("images/snippet_words_afin.png", 
         plot = snippet_words_afinn, 
         width = 10, 
         height = 8, 
         units = "in", 
         dpi = 300)
  
  

  ### Images per year ###
  
  # Joint version
  
  palette <- wes_palette("GrandBudapest2", n = length(unique(word_tokens_abstract$year)), type = "discrete")
  
  abstract_words_afinn_yearly <- ggplot(data = filter(word_tokens_abstract, !is.na(afinn)), 
                                         aes(x = afinn, fill = year)) +
    geom_histogram(position = "dodge", binwidth = 1) +
    scale_fill_manual(values = palette) + 
    scale_x_continuous(n.breaks = 7) +
    labs(title = "Crime in Chicago NYT Articles",
         subtitle = "Sentiment Distribution by Year (2020 to 2023)",
         x = "Sentiment Score", y = "Count") +
    theme_minimal() 
  
  print(abstract_words_afinn_yearly)
  
  ggsave("images/abstract_words_afin_yearly.png", 
         plot = abstract_words_afinn_yearly, 
         width = 10, 
         height = 8, 
         units = "in", 
         dpi = 300)
  
  # Wrapped Version
  
  abstract_words_afinn_yearly <- ggplot(data = filter(word_tokens_abstract, !is.na(afinn)),
                                         aes(x = afinn)) +
    geom_histogram(fill = "mediumslateblue", binwidth = 1) + 
    facet_wrap(~ year, ncol = 2, scales = "free_y") + 
    scale_x_continuous(n.breaks = 7) +
    labs(title = "Crime in Chicago NYT Articles",
         subtitle = "Sentiment Distribution by Year (2020 to 2023)",
         x = "Sentiment Score", y = "Count") +
    theme_minimal() 
  
  # Display the plot
  print(abstract_words_afinn_yearly)
  
    ggsave("images/abstract_words_afin_yearly2.png", 
         plot = abstract_words_afinn_yearly, 
         width = 10, 
         height = 8, 
         units = "in", 
         dpi = 300)
  

  

    
      