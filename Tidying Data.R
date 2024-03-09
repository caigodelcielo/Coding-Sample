# Clean environment
rm(list=ls())

# Set Working Directory
setwd("~/GitHub/Coding-Sample")

# Set path
path <- "~/GitHub/Coding-Sample"

# Load Libraries
library(tidyverse)
library(dplyr)


# Load files

total_data <- paste0(path, "SAEMP25N total.csv")
industry_data <- paste0(path, "SAEMP25N by industry.csv")

total_data <- read_csv(total_data, skip = 4)
industry_data <- read_csv(industry_data, skip = 4)

# Tidy data

industry_data <- industry_data |>
  filter(!is.na(GeoName)) |>
  mutate(`2000` = as.numeric(`2000`),
         `2017` = as.numeric(`2017`)) |>
  pivot_longer(c(`2000`, `2017`),
               names_to = "year",
               values_to = "total") |>
  pivot_wider(names_from = "Description", 
              values_from = "total") 

total_data <- total_data |>
  filter(!is.na(GeoName)) |>
  pivot_longer(c('2000', '2017'), 
               names_to = "year", 
               values_to = "total")

# Merge 

employment_data <- full_join(industry_data, total_data, by = c("GeoName", "GeoFips", "year")) |>
  select(-GeoFips, -LineCode, -`By industry`) |>
  rename(state = GeoName)

# Employment shares

industries <- names(employment_data)[3:12]

employment_shares <- employment_data |>
  pivot_longer(cols = all_of(industries), 
               names_to = "industry", 
               values_to = "employment") |>
  filter(!is.na(employment))

shares <- function (data, numerator, denominator) {
  data |>
    mutate(shares = (.data[[numerator]] / .data[[denominator]]))
}

employment_shares_data <- shares(employment_shares, "employment", "total")

employment_shares_data <- employment_shares_data |>
  pivot_wider(names_from = "industry",
              values_from = "shares") |>
  select(-total, -employment)

# Export File

df <- paste0(path, "data.csv")
write.csv(employment_shares_data, file = df)


# Create the function
top_states <- function(years, industry, number) {
  employment_shares_data |>
    filter(year == years) |>
    arrange(desc(.data[[industry]]))
}

# Get the top
top_states_man_2000 <- top_states("2000", 'Manufacturing', 5)

top_states_names <- top_states_man_2000$state

top_states_manufacturing <- data.frame()

# Compare 2000 vs 2017

for (state in top_states_names) {
  state_data <- employment_shares_data |>
    filter(state == state) |>
    filter(!is.na(Manufacturing)) |>
    select(state, year, Manufacturing)

 top_states_manufacturing <- rbind(top_states_manufacturing, state_data)
}

# Plot
top_states_manufacturing |>
ggplot(aes(x = state, y = Manufacturing, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Manufacturing Share Comparison: 2000 vs 2017",
       x = "State",
       y = "Manufacturing Share",
       fill = "Year") +
  scale_fill_manual(values = c("2000" = "cornflowerblue", "2017" = "orchid"))

# Create a for loop

lists_d <- list(
  list("year" = "2000", "industry" = 'Manufacturing', "number" = 5),
  list("year" = "2000", "industry" = 'Farm employment', "number" = 10),
  list("year" = "2017", "industry" = 'Information', "number" = 15)
)

for (list in lists_d) {
  top_states_data <- top_states(list$year, list$industry, list$number)
  
  top_states_names <- top_states_data$state
  
  top_states_industry <- data.frame()
  

  for (state_name in top_states_names) {
    state_data <- employment_shares_data |>
      filter(state == state_name, !is.na(.data[[list$industry]])) |>
      select(state, year, .data[[list$industry]])
    
    top_states_industry <- rbind(top_states_industry, state_data)
  }
  
  ggplot_object <- ggplot(top_states_industry, aes(x = state, y = .data[[list$industry]], 
                                                   fill = as.factor(year))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste(list$industry, "Share Comparison:", list$year),
         x = "State",
         y = paste(list$industry, "Share"),
         fill = "Year") +
    scale_fill_manual(values = c("2000" = "cornflowerblue", "2017" = "orchid"))
  
  print(ggplot_object)
  
  ggsave(paste0(list$industry, "_Share_Comparison_", list$year, ".png"),
         plot = ggplot_object, width = 10, height = 8, dpi = 300)
}

