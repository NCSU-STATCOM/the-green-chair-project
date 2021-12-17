library(rgdal)
library(raster)
library(tidyverse)
library(readxl)
library(viridis)
library(rgeos)
library(sf)
library(osmdata)
library(ggmap)

###################################################
# Let's plot what we have for now. 
school_merge_csv <- read.csv("school_merge.csv")
school_merge_df <- st_as_sf(school_merge_csv, coords = c("X", "Y"))
merge_shp <- st_set_crs(school_merge_df, 4326)
mad_map <- get_map(getbb("Wake County North Carolina"), maptype = "toner-background")

big_streets <- getbb("Wake County North Carolina")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
  osmdata_sf()

med_streets <- getbb("Wake County North Carolina")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


small_streets <- getbb("Wake County North Carolina")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

#river <- getbb("Wake County North Carolina")%>%
#  opq()%>%
#  add_osm_feature(key = "waterway", value = "river") %>%
#  osmdata_sf()

railway <- getbb("Wake County North Carolina")%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

q <- ggmap(mad_map) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .5)

q1 <- q + 
  theme_void() + # get rid of background color, grid lines, etc.
  labs(title = "Wake County") +
  geom_sf(data = merge_shp$geometry, inherit.aes = FALSE,
          size = 0.5)
q1
