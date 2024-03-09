# Clean environment
rm(list=ls())

# Set Working Directory
setwd("~/GitHub/Coding-Sample")

# Set path
path <- "~/GitHub/Coding-Sample"

# Load libraries
library(sf)
library(tidyverse)
library(spData)
library(rnaturalearth)
library(lubridate)
library(wesanderson)


#------------------------------Spatial Data------------------------------------#

# Load data

homicides <- read.csv(paste0(path, "/Violence_Reduction_-_Victims_of_Homicides_and_Non-Fatal_Shootings_20240131.csv"))
shotspotter <- read.csv(paste0(path, "/Violence_Reduction_-_Shotspotter_Alerts_20240131.csv"))

police_districts <- st_read(file.path(path,
                                      "/Boundaries - Police Districts (current)/geo_export_abd0bfc6-8bdf-495f-b323-f2d06990ce40.shp"))

# Clean data

police_districts <- police_districts |>
  rename(district = dist_num) |>
  mutate(district = as.integer(district))

homicides <- homicides |>
  rename_with(tolower) |>
  mutate(datetime = mdy_hms(date),
         date = as.Date(datetime),
         time = format(datetime, "%H:%M:%S"),
         year = year(date)) |>
  filter(year >= 2017,
         incident_primary == "HOMICIDE")

homicides_district <- homicides |>
  group_by(district) |>
  summarize(total_homicides = n())

homicides_district <- full_join(homicides_district, police_districts, by = "district",
                              keep = TRUE, relationship = "one-to-one")

shotspotter <- shotspotter |>
  rename_with(tolower) |>
  mutate(datetime = mdy_hms(date),
         date = as.Date(datetime),
         time = format(datetime, "%H:%M:%S"),
         year = year(date)) |>
  filter(year >= 2017)

shots_district <- shotspotter |>
  group_by(district) |>
  summarize(total_shootings = n())

shots_district <- full_join(shots_district, police_districts, by = "district",
                                keep = TRUE, relationship = "one-to-one")


# Plot 1: Homicides
pal <- wes_palette("Zissou1", 100, type = "continuous")

homicides_plot <- ggplot(data = homicides_district) +
  geom_sf(aes(fill = total_homicides, geometry = geometry)) +
  theme_void() +
  labs(title = "Total homicides per district - cumulative",
       subtitle = "From 2017 to 2024",
       caption = "Source: Chicago Data Portal",
       fill = "Total Homicides") +
  scale_fill_gradientn(colours = pal)

print(homicides_plot)

ggsave("homicides_per_district.png", 
       plot = homicides_plot, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

# Plot 2: Shootings

shootings_plot <- ggplot(data = shots_district) +
  geom_sf(aes(fill = total_shootings, geometry = geometry)) +
  theme_void() +
  labs(title = "Total shootings per district - cummulative",
       subtitle = "From 2017 to 2024",
       caption = "Source: Chicago Data Portal",
       fill = "Total Shootings") +
  scale_fill_gradientn(colours = pal)

print(shootings_plot)

ggsave("shootings_per_district.png", 
       plot = shootings_plot, 
       width = 10, 
       height = 8, 
       units = "in", 
       dpi = 300)

