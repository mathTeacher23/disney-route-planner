library(shiny)
library(leaflet)
library(sf)
library(geosphere)
library(readr)
library(dplyr)
library(bslib)
library(shinyjs)
library(htmltools)
library(htmlwidgets)

# Load Disney data
pathname <- "utils/disney.csv"
disney_df <- read_csv(pathname, show_col_types = FALSE)

location_lookup <- disney_df %>%
  filter(Class %in% c("Resort", "Park", "Shopping Plaza")) %>%
  select(Location, Latitude, Longitude, Class, Park_Proximity, Transportation_Access)

#write.csv(location_lookup, "/Users/andrewcasanova/Documents/data_science_projects/rshiny_projects/DISNEY_ROUTE_PLANNER_APP/utils/disney_lookup.csv", row.names = FALSE)

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
resort_list <- c('All Star Movies','All Star Music','All Star Sports', 'Animal Kingdom Lodge', 'Art of Animation', 'Beach Club', 'Boardwalk Inn', 
                 'Caribbean Beach', 'Contemporary', 'Coronado Springs','Dolphin', 'Grand Floridian', 'Old Key West', 'Polynesian', 'Pop Century', 
                 'Port Orleans French Quarter', 'Port Orleans Riverside', 'Riviera', 'Saratoga Springs', 'Swan', 'Wilderness Lodge', 'Yacht Club')

blurb_df <- data.frame(Location = c('Disney Springs','Magic Kingdom','Animal Kingdom','Hollywood Studios','Epcot','All Star Movies', 'All Star Music',
                                    'All Star Sports', 'Animal Kingdom Lodge', 'Art of Animation', 'Beach Club', 'Boardwalk Inn', 'Caribbean Beach', 
                                    'Contemporary', 'Coronado Springs','Dolphin', 'Grand Floridian', 'Old Key West', 'Polynesian', 'Pop Century', 
                                    'Port Orleans French Quarter', 'Port Orleans Riverside', 'Riviera', 'Saratoga Springs','Swan', 'Wilderness Lodge', 'Yacht Club'),
                       hyperlinks = c('<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>',
                                      '<br> <a href="https://disneyworld.disney.go.com"> Visit Disney World Website!</a>'),
                       Description = c('<center><h5><b> Disney Springs </h5></b></center> <br> <center><img src="map_disney_springs.jpg" width = "100%" height = "100%"></center> <br>Disney Springs is an outdoor shopping, dining, and entertainment complex at the Walt Disney World Resort in Lake Buena Vista, Florida, near Orlando. It opened on March 22, 1975, and has been expanded and renamed over the years.',
                                       '<center><h5><b> Disney Worlds Magic Kingdom Park </h5></b></center> <br> <center><img src="map_magic_kingdom.jpg" width = "100%" height = "100%"></center><br> Magic Kingdom Park, previously known as Walt Disney World Magic Kingdom and The Magic Kingdom, is a theme park at the Walt Disney World Resort in Bay Lake, Florida, near Orlando, Florida!',
                                       '<center><h5><b> Disney Worlds Animal Kingdom Park </h5></b></center> <br> <center><img src="map_animal_kingdom.jpg" width = "100%" height = "100%"></center><br> Disneys Animal Kingdom Theme Park is a zoological theme park at the Walt Disney World Resort in Bay Lake, Florida, near Orlando. Owned and operated by The Walt Disney Company through its Parks, Experiences and Products division, it is the largest theme park in the world, covering 580 acres.',
                                       '<center><h5><b> Disney Worlds Hollywood Studios Park </h5></b></center> <br> <center><img src="map_hollywood_studios.jpg" width = "100%" height = "100%"></center><br> Disneys Hollywood Studios is a theme park at the Walt Disney World Resort in Bay Lake, Florida, near Orlando. It is owned and operated by The Walt Disney Company through its Parks, Experiences and Products division.',
                                       '<center><h5><b> Disney Worlds EPCOT Park </h5></b></center> <br> <center><img src="map_epcot.jpg" width = "100%" height = "100%"></center><br>Epcot, stylized in all uppercase as EPCOT, is a theme park at the Walt Disney World Resort in Bay Lake, Florida. It is owned and operated by The Walt Disney Company through its Parks, Experiences and Products division.',
                                       '<center><h5><b> All Star Movies Resort </h5></b></center> <br> <center><img src="map_all_star_movies.jpg" width = "100%" height = "100%"></center><br> Disneys All Star Movies Resort is one of five resorts in the Value Resort category, along with Disneys All Star Sports Resort, Disneys All Star Music Resort, Disneys Pop Century Resort and Disneys Art of Animation Resort.',
                                       '<center><h5><b> All Star Music Resort </h5></b></center> <br> <center><img src="map_all_star_music.jpg" width = "100%" height = "100%"></center><br> Disneys All Star Music Resort is one of five resorts in the Value Resort category, along with Disneys All Star Sports Resort, Disneys All Star Movies Resort, Disneys Pop Century Resort and Disneys Art of Animation Resort.',
                                       '<center><h5><b> All Star Sports Resort </h5></b></center> <br> <center><img src="map_all_star_sports.jpg" width = "100%" height = "100%"></center><br>Disneys All Star Sport Resort is one of five resorts in the Value Resort category, along with Disneys All Star Music Resort, Disneys All Star Movies Resort, Disneys Pop Century Resort and Disneys Art of Animation Resort.', 
                                       '<center><h5><b> Animal Kingdom Lodge Resort </h5></b></center> <br><center><img src="map_animal_kingdom_lodge.jpg" width = "100%" height = "100%"></center><br> Disneys Animal Kingdom Lodge is an African-themed deluxe resort at the Walt Disney World Resort in Orlando, Florida. It opened on April 16, 2001. The resort is owned and operated by Disney Parks, Experiences and Products.', 
                                       '<center><h5><b> Art of Animation Resort </h5></b></center> <br> <center><img src="map_art_of_animation.jpg" width = "100%" height = "100%"></center><br>Disneys Art of Animation Resort is a resort within Walt Disney World resort in Lake Buena Vista, Florida. It is located where construction on the unfinished half of Disneys Pop Century Resort was started but later abandoned after the September 11 attacks. It is considered a value resort.', 
                                       '<center><h5><b> Beach Club Resort </h5></b></center> <br> <center><img src="map_beach_club.jpg" width = "100%" height = "100%"></center><br>Disneys Beach Club Resort is a beach-themed deluxe resort at the Walt Disney World Resort. It opened on November 19, 1990. The resort is owned and operated by Disney Parks, Experiences and Products. ', 
                                       '<center><h5><b> Boardwalk Inn Resort </h5></b></center> <br> <center><img src="map_boardwalk_inn.jpg" width = "100%" height = "100%"></center><br>Disneys Boardwalk Inn Resort is a hotel and entertainment complex at the Walt Disney World resort, in Bay Lake, Florida, near Orlando, Florida. First opened in 1996, the Boardwalk Resort lies in the Epcot Resort Area, alongside Crescent Lake, and is situated between Epcot and Disneys Hollywood Studios.', 
                                       '<center><h5><b> Caribbean Beach Resort </h5></b></center> <br> <center><img src="map_caribbean_beach.jpg" width = "100%" height = "100%"></center><br>Disneys Caribbean Beach Resort is a resort located within the Walt Disney World Resort. It is located in the Epcot Resort Area of Walt DIsney World, close to the waterpark Typhoon Lagoon and is classified as a moderately priced resort.', 
                                       '<center><h5><b> Contemporary Resort </h5></b></center> <br> <center><img src="map_contemporary.jpg" width = "100%" height = "100%"></center><br>Disneys Contemporary Resort, originally to be named Tempo Bay Hotel and previously the Contemporary Resort Hotel, is a resort located at the Walt Disney World Resort in Bay Lake, Florida.', 
                                       '<center><h5><b> Coronado Springs Resort </h5></b></center> <br> <center><img src="map_coronado_springs.jpg" width = "100%" height = "100%"></center><br>Disneys Coronado Springs Resort is a resort hotel at the Walt Disney World Resort that opened on August 1, 1997. The resort is located in the Animal Kingdom Resort Area. Its theme is American colonial Spanish and southwestern American. This hotel is categorized as a moderate resort.',
                                       '<center><h5><b> The Dolphin Hotel </h5></b></center> <br> <center><img src="map_dolphin.jpg" width = "100%" height = "100%"></center><br>The Walt Disney World Dolphin is a resort hotel located between Epcot and Disneys Hollywod Studios in the Walt Disney World Resort in Bay Lake, Florida, and across from its sister resort, the Walt Disney World Swan.', 
                                       '<center><h5><b> The Grand Floridian </h5></b></center> <br> <center><img src="map_grand_floridian.jpg" width = "100%" height = "100%"></center><br>Disneys Grand Floridian Resort & Spa is a Victorian themed hotel and spa located at Walt Disney World Resort in Florida. The property opened on June 28, 1988, as the Grand Floridian Beach Resort. The name changed to Disneys Grand Floridian Resort & Spa during the fall of 1997 and is classified as a deluxe resort.', 
                                       '<center><h5><b> Old Key West Resort </h5></b></center> <br> <center><img src="map_old_key_west.jpg" width = "100%" height = "100%"></center><br>Disneys Old Key West Resort is a Disney Vacation Club resort located at the Walt Disney World Resort in Lake Buena Vista, Florida. It opened on December 20, 1991, and was formerly known as Disneys Vacation Club resort until January 1996 when it was renamed.', 
                                       '<center><h5><b> Polynesian Resort </h5></b></center> <br> <center><img src="map_polynesian.jpg" width = "100%" height = "100%"></center><br>Disneys Polynesian Village Resort is a Disney-owned adn operated resort located at the Walt Disney World Resort. It began operation on October 1, 1971 as one of Walt Disney World Resorts first two on-site hotels. The resort has a South Seas theme, and originally opened with 492 rooms.', 
                                       '<center><h5><b> Pop Century Resort </h5></b></center> <br> <center><img src="map_pop_century.jpg" width = "100%" height = "100%"></center><br>Disneys Pop Century Resort is a resort located at the Walt Disney World Resort in Lake Buena Vista, Florida, opened on December 14, 2003. It is the fourth value-priced resort in the complex, following Disneys All Star Movies, All Star Music, and All Star Sports Resorts in the 1990s.', 
                                       '<center><h5><b> Port Orleans French Quarter Resort </h5></b></center> <br> <center><img src="map_french_quarter.jpg" width = "100%" height = "100%"></center><br>Disneys Port Orleans Resort French Quarter is a resort located at the Walt Disney World Resort in Lake Buena Vista, Florida. Its sister resort is the Port Orleans Riverside Resort. It is themed to look like New Orleans in the Disney Springs area and owned and operated by Disney Parks, Experiences and Products.', 
                                       '<center><h5><b> Port Orleans Riverside Resort </h5></b></center> <br> <center><img src="map_riverside.jpg" width = "100%" height = "100%"></center><br>Disneys Port Orleans Riverside is a resort located at the Walt Disney World Resort in Lake Buena Vista, Florida. Its sister resort is the Port Orleans French Quarter Resort. It is themed to look like New Orleans in the Disney Springs area and owned and operated by Disney Parks, Experiences and Products.', 
                                       '<center><h5><b> The Riviera Resort </h5></b></center> <br> <center><img src="map_riviera.jpg" width = "100%" height = "100%"></center><br>Disneys Riviera Resort is a Disney Vacation Club resort at the Walt Disney World Resort in Bay Lake, Florida. It was built by Disney Parks, Experiences, and Products between Epcot and Disneys Hollywood Studios. It is the first newly constructed resort to be served by the Disney Skyliner gondola system and the 15th DIsney Vacation Club property to be built. The resort is themed after Walt Disneys trips and experiences in Europe and a love of the French and Italian Riviera. It opened on December 16, 2019.', 
                                       '<center><h5><b> Saratoga Springs Resort </h5></b></center> <br> <center><img src="map_saratoga_springs.jpg" width = "100%" height = "100%"></center><br>Disneys Saratoga Springs Resort & Spa is a Disney Vacation Club resort at the Walt Disney World Resort. The resort is the seventh Disney Vacation Club resort and is situated on the former site of the Disney Institute. It first opened on May 17, 2004 and was built in three phases.',
                                       '<center><h5><b> The Swan Hotel </h5></b></center> <br> <center><img src="map_swan.jpg" width = "100%" height = "100%"></center><br>The Walt Disney World Swan is a resort hotel located between Epcot and Disneys Hollywood Studios in the Walt Disney World Resort in Bay Lake, Florida, and across from its sister resort, the Walt Disney World Dolphin', 
                                       '<center><h5><b> Wilderness Lodge at Fort Wilderness Resort </h5></b></center> <br><center><img src="map_wilderness_lodge.jpg" width = "100%" height = "100%"></center><br>Disneys Wilderness Lodge is a resort hotel located at the Walt Disney World Resort in Lake Buena Vista, Florida. Opening on May 28, 1994, the resort is owned and operated by Disney Parks, Experiences and Products. Disneys Wilderness Lodge is located in the Magic Kingdom Resort Area on Bay Lake. The resort is also located near Disneys Fort Wilderness Resort & Campground. ', 
                                       '<center><h5><b> Yach Club Resort </h5></b></center> <br> <center><img src="map_yacht_club.jpg" width = "100%" height = "100%"></center><br>Disneys Yacht Club Resort is a New England nautical-themed resort at Walt Disney World. First opened on November 5, 1990, it is one of several Epcot Area Resorts. Disneys Yacht Club is located next to a sister resort, Disneys Beach Club Resort, and across Crescent Lake from Disneys BoardWalk Resort.' ))

#query_list <- lapply(blurb_df$Location, function(x) gsub(' ', '%20', x))

blurb_df$hyperlinks <-c('<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Disney%20Springs"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Magic%20Kingdom"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Animal%20Kingdom"> Visit Disney World Website!</a>',             
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Hollywood%20Studios"> Visit Disney World Website!</a>',            
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Epcot"> Visit Disney World Website!</a>',           
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=All%20Star%20Movies"> Visit Disney World Website!</a>',          
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=All%20Star%20Music"> Visit Disney World Website!</a>',         
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=All%20Star%20Sports"> Visit Disney World Website!</a>',        
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Animal%20Kingdom%20Lodge"> Visit Disney World Website!</a>',       
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Art%20of%20Animation"> Visit Disney World Website!</a>',      
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Beach%20Club"> Visit Disney World Website!</a>',     
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Boardwalk%20Inn"> Visit Disney World Website!</a>',    
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Caribbean%20Beach"> Visit Disney World Website!</a>',   
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Contemporary"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Coronado%20Springs"> Visit Disney World Website!</a>', 
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Dolphin"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Grand%20Floridian"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Old%20Key%20West"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Polynesian"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Pop%20Century"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Port%20Orleans%20French%20Quarter"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Port%20Orleans%20Riverside"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Riviera"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Saratoga%20Springs"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Swan"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Wilderness%20Lodge"> Visit Disney World Website!</a>',
                        '<br> <a href="https://disneyworld.disney.go.com/search/?searchQuery=Yacht%20Club"> Visit Disney World Website!</a>')

blurb_df$Description <- paste0(blurb_df$Description, blurb_df$hyperlinks)

