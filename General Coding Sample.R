###############################################################################
#-------------------------------Coding Sample --------------------------------#
###############################################################################

#-------------------------------Data Cleaning---------------------------------#

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

#---------------------------------Function------------------------------------#

# Function to calculate time elapsed in processing data
take_time <- function(state_name) {
  
  # Partitioned data
  
  start_part <- Sys.time()
  
  nursing_part_parq |>
    filter(state == state_name) |>
    group_by(scope_severity) |>
    summarize(deficiencies_severity = n()) |>
    collect()
  
  end_part <- Sys.time()
  total_time_part <- end_part-start_part
  
  # Non-Partitioned data
  
  start_nonpart <- Sys.time()
  
  nursing_parquet |>
    filter(state == state_name) |>
    group_by(scope_severity) |>
    summarize(deficiencies_severity = n()) |>
    collect()
  
  end_nonpart <- Sys.time()
  
  total_time_nonpart <- end_nonpart-start_nonpart
  
  # Time Difference
  
  difference <- total_time_nonpart - total_time_part
  
  # Print Results
  
  print(paste("It took", total_time_part, "for partitioned data"))
  print(paste("It took", total_time_nonpart, "for non-partitioned data"))
  print(paste("The difference between both is", difference))
  
  # Save results
  results <- c(total_time_part, total_time_nonpart, difference) # added this for the next question
  return (results)
}

# Test the function
take_time("IL")

#---------------------------------Plotting------------------------------------#

provider_sum <- provider_sum |>
  mutate(cost_normalized = mean_payment_wt / mean_patient_casemix_wt)

provider_sum |>
  ggplot() +
  geom_freqpoly(aes(x = cost_normalized, color = "Cost Normalized"), 
                linewidth = 1, binwidth = 50, show.legend = TRUE) +
  geom_freqpoly(aes(x = mean_payment_wt, color = "Average cost"), 
                linewidth = 1, binwidth = 50, show.legend = TRUE) +
  scale_color_manual(values = c(
    "Cost Normalized" = "cornflowerblue", 
    "Average cost" = "coral"),
    labels = c("Cost Normalized", "Average cost")) +
  labs(x = "Cost ($)",
       y = " ",
       title = "Comparison between cost normalized and average cost") +
  scale_x_continuous(limits = c(0, 6000), 
                     breaks = seq(0, 6000, by = 1000),
                     labels = scales::dollar_format())

#---------------------------Statistical Analysis-------------------------------#

# Simple Regression
regression9 <- felm(gross_village_product ~  female_leader + post + 
                      treatment_post|0|0|village_id, data = question9)
summary(regression9)


# Fixed Effects
question9 <- question9 |>
  mutate(D = case_when(!is.na(female_election_year) & year >= 2005 ~ 1,
                       !is.na(female_election_year) & year < 2005 ~ 0,
                       is.na(female_election_year) ~ 0,
                       TRUE ~ as.numeric(NA)))


fe_regression9 <- felm(gross_village_product ~ D|village_id + 
                         year|0|village_id + post, data = question9)

summary(fe_regression9)

# 1) Estimating the first effect:
  

yolo_dta <- maroons |>
  filter(county == "YOLO") |>
  mutate(treatment = ifelse(marginal_property_tax_rate == 50, 1, 0),
         year = year_of_home_purchase - 1979,
         interaction = treatment*year)

yolo <- lm(evades_taxes_yn ~ treatment + year + interaction, data = yolo_dta)
summary(yolo)

# 2) Estimating the second effect

alameda_dta <- maroons |>
  filter(county == "ALAMEDA") |>
  mutate(treatment = ifelse(marginal_property_tax_rate == 50, 1, 0),
         year = year_of_home_purchase - 1979,
         interaction = treatment*year)

# Reduced form

alameda_reduced <- lm(evades_taxes_yn ~ treatment, data = alameda_dta)
summary(alameda_reduced)

# First Stage

alameda_1ststage <- ivreg(evades_taxes_yn ~ treatment + year | interaction , data = alameda_dta)
summary(alameda_1ststage)

# Treatment effects over time

question11 <- program_eval|>
  mutate(ever_treated = case_when(!is.na(female_election_year) ~ 1,
                                  is.na(female_election_year) ~ 0,
                                  TRUE ~ as.numeric(NA)),
         post = ifelse(!is.na(female_election_year), year - 
                         female_election_year, 0),
         D = factor(female_leader * post),
         D = relevel(D, ref = "-1")) |>
  filter(!(female_leader == 1 & (female_election_year < 2005 | 
                                   female_election_year >= 2010)))

regression11 <- felm(gross_village_product ~ D|village_id 
                     + year|0|village_id + year, data = question11)

summary(regression11)

res <- as.data.frame(summary(regression11)$coefficients)
res$low <- res$Estimate - qnorm(1 - 0.05/2)*res$`Cluster s.e.`
res$high <- res$Estimate + qnorm(1 - 0.05/2)*res$`Cluster s.e.`
res$event_time_value <- c(-8:-2, 0:10)

res <- res |>
  select(Estimate, event_time_value, low, high)

omitted <- data.frame("Estimate" = 0,
                      "event_time_value" = -1,
                      "low" = 0,
                      "high" = 0)
res <- res |>
  rbind(omitted)

res |>
  ggplot(aes(x = event_time_value, y = Estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = high), fill = "purple", alpha = 0.4) +
  scale_x_continuous(breaks = res$event_time_value) +
  labs(x = "Event time",
       title = "Treatment effects over time",
       subtitle = "Females elected between 2005 and 2009") 