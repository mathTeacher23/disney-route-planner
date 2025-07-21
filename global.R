library(shiny)
library(leaflet)
library(sf)
library(geosphere)
library(readr)
library(dplyr)

# Load Disney data
pathname <- "utils/disney.csv"
disney_df <- read_csv(pathname, show_col_types = FALSE)

# Prepare dropdown option names
disney_df <- disney_df %>%
  mutate(DisplayName = paste(Location, "-", Id))

location_lookup <- disney_df %>%
  distinct(Location, .keep_all = TRUE) %>%
  select(Location, Latitude, Longitude, Class, Park_Proximity, Transportation_Access)

