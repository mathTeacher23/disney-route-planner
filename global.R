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

# Define walkable groups
walkable_groups <- list(
  c("Pop Century", "Riviera", "Caribbean Beach", "Art of Animation"),
  c("All Star Sports", "All Star Movies", "All Star Music"),
  c("Port Orleans Riverside", "Port Orleans French Quarter"),
  c("Grand Floridian", "Polynesian"),
  c("Contemporary", "Magic Kingdom"),
  c("Boardwalk Inn", "Beach Club", "Yacht Club", "Epcot", "Swan", "Dolphin"),
  c("Saratoga Springs", "Disney Springs")
)

# # Flatten into a data.frame of pairs (symmetric)
# walkable_pairs <- do.call(rbind, lapply(walkable_groups, function(group) {
#   expand.grid(from = group, to = group, stringsAsFactors = FALSE)
# })) |> 
#   dplyr::filter(from != to)
# 
# # Initialize empty Walkable_To
# location_lookup$Walkable_To <- ""
# 
# for (group in walkable_groups) {
#   for (loc in group) {
#     others <- setdiff(group, loc)  # all except current loc
#     location_lookup$Walkable_To[location_lookup$Location == loc] <- paste(others, collapse = ", ")
#   }
# }


all_locations <- location_lookup$Location
walkable_flat <- unique(unlist(walkable_groups))

missing <- setdiff(all_locations, walkable_flat)
print(missing)

location_lookup$Walkable_To[location_lookup$Location %in% missing] <- NA

# Reset column
location_lookup$Walkable_To <- ""

# Assign walkable groups exactly
for (group in walkable_groups) {
  for (loc in group) {
    others <- setdiff(group, loc)
    location_lookup$Walkable_To[location_lookup$Location == loc] <- paste(others, collapse = ", ")
  }
}

# location_lookup %>% 
#   select(Location, Walkable_To) %>% 
#   arrange(Location) %>% 
#   print(n=Inf)
