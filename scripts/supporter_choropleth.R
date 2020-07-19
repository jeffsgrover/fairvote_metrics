# install.packages('rgdal')
# install.packages('tidyverse')
# install.packages('here')
# install.packages('sf')

library(rgdal)
library(tidyverse)
library(here)
library(sf)

# Reading in data
# Data source: https://www.ilhousedems.com/redistricting/?page_id=554
# Code source: https://rpubs.com/huanfaChen/ggplotShapefile

house_shp <-  readOGR(dsn   = here('data/ilga_shapefiles'), 
                      layer = 'PA 97-6 House Districts')
senate_shp <- readOGR(dsn   = here('data/ilga_shapefiles'),
                      layer = 'PA 97-6 Senate Districts')
# plot(house_shp)
# plot(senate_shp)

summary(house_shp@data) # Need to use @, not $

ggplot() + geom_polygon(data = house_shp, 
                        aes(x = long, y = lat, group = group), 
                        colour = "black", fill = NA)

# Reading in supporter data
# Locations geocoded by https://geocoding.geo.census.gov/geocoder/locations/addressbatch?form
# Geocoder documentation = https://www.census.gov/programs-surveys/geography/technical-documentation/complete-technical-documentation/census-geocoder.html 
# accessed July 18, 2020
supporter_data <- read_csv(here('data/addresses_with_coords.csv'))
supporter_data <- supporter_data %>% 
  filter(supporter_data$coords!='0') %>%
  separate(coords, sep=',', into=c('lon','lat'))
supporter_data$lat <- parse_number(supporter_data$lat)
supporter_data$lon <- parse_number(supporter_data$lon)

state_map <- ggplot() + 
  geom_point(data = supporter_data, 
             aes(x=lon, y=lat)) +
  geom_polygon(data = house_shp,
               aes(x=long, y=lat, group=group),
               color='black', fill=NA) +
  coord_equal() +
  theme_void()

state_map

chicago_map <- state_map +
  lims(x = c(-88.4, max(supporter_data$lon)),
       y = c(41.3,  max(supporter_data$lat)))
chicago_map
# Clean up weird lines

# Calculate which district a supporter is inside
# Code source: https://mattherman.info/blog/point-in-poly/
## Convert data to sf objects--this is getting annoying; maybe try another way
supporter_sf <- supporter_data %>%
  st_as_sf(
    coords = c("lon", "lat"),
    # crs = 2028,        # NAD83(HARN) / Illinois East
    stringsAsFactors = FALSE,
    remove = TRUE
  )
house_sf <- house_shp %>%
  st_as_sf(
    coords = c('long','lat'),
    # crs = 2028,
    stringsAsFactors = FALSE,
    remove = TRUE
  )

ggplot() + 
  geom_sf(data = supporter_sf) +
  geom_sf(data = house_sf) +
  theme_bw()

supporter_house_districts <- st_join(supporter_sf, house_sf, join = st_within)
