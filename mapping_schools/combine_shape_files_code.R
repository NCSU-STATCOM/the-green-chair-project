### 0. Code for combining shape files of all the schools in NC
### Download all the shapefiles from https://www.nconemap.gov
library(rgdal)
library(raster)
library(tidyverse)
library(readxl)


public_shp <- readOGR(dsn = "Public_Schools.shp", stringsAsFactors = F)
non_public_shp <- readOGR(dsn = "Non-Public_Schools.shp", stringsAsFactors = F)
uni_shp <- readOGR(dsn = "Colleges_and_Universities.shp", stringsAsFactors = F)

public_shp_df <- as.data.frame(public_shp)
non_public_shp_df <- as.data.frame(non_public_shp)
uni_shp_df <- as.data.frame(uni_shp)

#write.csv(public_shp_df, "NC_Public_Schools.csv", row.names = F)
#write.csv(non_public_shp_df, "NC_Non-Public_Schools.csv", row.names = F)
#write.csv(uni_shp_df, "NC_Colleges_and_Universities.csv", row.names = F)


### 1-2. Rename & remove columns in the three shape files
### Eyeballing the three files, we select common variables in all three files
public_shp_df <- public_shp_df %>% dplyr::select(c("SCHOOL_NAM", "PHYS_ADDR", "PHYS_CITY", "PHYS_ZIP", "COUNTY", "coords.x1", "coords.x2"))
non_public_shp_df <- non_public_shp_df %>% dplyr::select(c("SchoolName", "Address", "City", "Zipcode", "County", "coords.x1", "coords.x2"))
uni_shp_df <- uni_shp_df %>% dplyr::select(c("NAME", "ADDRESS", "CITY", "ZIP", "COUNTY", "coords.x1", "coords.x2"))
colnames(public_shp_df) <- colnames(uni_shp_df) <- c("SchoolName", "Address", "City", "Zipcode", "County", "coords.x1", "coords.x2")

### re-convert the data.frames to a shape file
coordinates(public_shp_df) <- coordinates(non_public_shp_df) <- coordinates(uni_shp_df) <- ~ coords.x1 + coords.x2

# merge the three shape files into one
shp <- raster::union(public_shp_df, non_public_shp_df)
shp <- raster::union(shp, uni_shp_df)
shp_df <- as.data.frame(shp)
write.csv(shp_df, "NC_schools.csv", row.names = F)

