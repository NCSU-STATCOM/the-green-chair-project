
setwd("helpful_code")

library(rgdal)
library(ggplot2)
library(viridis)
library(rgeos)
library(maptools)





### Mapping Point-Referenced Data

# using 2019 counties shapefile from https://www.census.gov/cgi-bin/geo/shapefiles/index.php 
# too large to upload to GitHub; download it on your own if you want
# It's uploaded to the Google Drive for The Green Chair Project
counties_shp <- readOGR("tl_2019_us_county.shp")

counties_centroids <- gCentroid(counties_shp, byid = TRUE, id = counties_shp@data$GEOID)

centroids_df <- as.data.frame(counties_centroids)



# Plotting (randomly generated) life expectancies for each county (and county-equivalents around the world),
# at the county's centroid

le_df <- data.frame(long=centroids_df$x, lat=centroids_df$y, Y=rnorm(nrow(centroids_df), 77, 10))
ggplot(le_df, aes(long, lat)) +
  borders("state") +
  geom_point(aes(colour = Y)) +
  scale_colour_gradientn(colours = viridis(10)) +
  xlab("")+ylab("")+labs(title="Life expectancy")





### Mapping choropleths (regions colored according to a value)

# There's a package called choroplethrzip (https://arilamstein.com/creating-zip-code-choropleths-choroplethrzip/)
# that should be useful for zip code mapping



# First way, using choroplethr package

library(choroplethr)

nc_info <- get_tract_map("north carolina")

outcome_df <- data.frame(region = nc_info$region, 
                         value = rnorm(nrow(nc_info), 77, 10))

tract_choropleth(outcome_df, state_name = "north carolina")



# Second way, using census tract shapefile

library(dplyr)

shp_nc <- readOGR(dsn = "tl_2010_37_tract10/tl_2010_37_tract10.shp")

outcome_df <- data.frame(GEOID = shp_nc$GEOID10, 
                         Prevalence = rnorm(nrow(shp_nc@data), 77, 10))

shp_nc@data <- left_join(shp_nc@data, outcome_df, by = c("GEOID10" = "GEOID"))

shp_nc_df <- broom::tidy(shp_nc, region = "GEOID10")

shp_nc_df <- shp_nc_df %>% left_join(shp_nc@data, by = c("id" = "GEOID10"))

map_NC <- ggplot() + geom_polygon(data = shp_nc_df, aes(x = long, y = lat, group = group, fill = Prevalence)) + theme_void() + 
  ggtitle("Prevalence") + theme(plot.title = element_text(size=28)) +
  scale_fill_viridis()
map_NC # takes a while to render


