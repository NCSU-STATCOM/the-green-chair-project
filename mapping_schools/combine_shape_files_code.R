### 0. Code for combining shape files of all the schools in NC
### Download all the shapefiles from https://www.nconemap.gov
library(rgdal)
library(raster)
library(tidyverse)
library(readxl)
library(sf)

public_shp_df <- st_read(dsn = "Public_Schools.shp", stringsAsFactors = F)
non_public_shp_df <- st_read(dsn = "Non-Public_Schools.shp", stringsAsFactors = F)
uni_shp_df <- st_read(dsn = "Colleges_and_Universities.shp", stringsAsFactors = F)

#write.csv(public_shp_df, "NC_Public_Schools.csv", row.names = F)
#write.csv(non_public_shp_df, "NC_Non-Public_Schools.csv", row.names = F)
#write.csv(uni_shp_df, "NC_Colleges_and_Universities.csv", row.names = F)


### 1-2. Rename & remove columns in the three shape files
### Eyeballing the three files, we select common variables in all three files
public_shp_df <- public_shp_df %>% dplyr::select(c("SCHOOL_NAM", "PHYS_ADDR", "PHYS_CITY", "PHYS_ZIP", "COUNTY", "geometry"))
non_public_shp_df <- non_public_shp_df %>% dplyr::select(c("SchoolName", "Address", "City", "Zipcode", "County", "geometry"))
uni_shp_df <- uni_shp_df %>% dplyr::select(c("NAME", "ADDRESS", "CITY", "ZIP", "COUNTY", "geometry"))
colnames(public_shp_df) <- colnames(uni_shp_df) <- c("SchoolName", "Address", "City", "Zipcode", "County", "geometry")

### re-convert the data.frames to a shape file
#coordinates(public_shp_df) <- coordinates(non_public_shp_df) <- coordinates(uni_shp_df) <- ~ coords.x1 + coords.x2

# merge the three shape files into one
shp <- union_all(public_shp_df, non_public_shp_df)
#shp <- raster::union(public_shp_df, non_public_shp_df)
#shp <- raster::union(shp, uni_shp_df)
#shp_df <- as.data.frame(shp)
write.csv(shp, "NC_schools.csv", row.names = F)

