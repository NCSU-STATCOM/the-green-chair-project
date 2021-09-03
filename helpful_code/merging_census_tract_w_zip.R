
### Example code for merging zip code data with census tract data.

# I didn't upload the flood_risk or the places_dat_wide data sets, but it suffices to say that flood_risk contains
# several numeric flood risk variables for each zip code, and places_dat_wide is the census tract data that
# I want to merge the flood risk data into



# reading in the ZCTA crosswalk

# use colClasses to read identifiers with leading zeros

zcta_crosswalk <- read.csv("zcta_crosswalk/zcta_tract_rel_10.txt", 
                           colClasses = c(rep("character", 5), rep("numeric", 20)))

# I focus on TRHUPCT, "The Percentage of Total Housing Unit Count of the 2010 Census Tract represented by the record" 
# to merge the flood risk zip code data with the rest of the data in terms of census tracts

# May need to use different variable, like ZPOPPCT, for your use case.

# mini EDA
trhupct_summary<- zcta_crosswalk %>% group_by(GEOID) %>% summarise(trhupct_sum = sum(TRHUPCT), trpoppct_sum = sum(TRPOPPCT), 
                                                                   trareapct_sum = sum(TRAREAPCT))

# most census tracts are wholly accounted for by the zip codes.
mean(trhupct_summary$trhupct_sum >= 99)

# all the flood risk zip codes are accounted for within the crosswalk.
all(flood_risk$zipcode %in% zcta_crosswalk$ZCTA5)

# approach: take a weighted mean of the non-missing flood risk values of the ZCTAs within each tract.

flood_risk_colnames_subset <- colnames(flood_risk)[(startsWith(colnames(flood_risk), "pct_") | 
                                                      startsWith(colnames(flood_risk), "avg_risk_")) & 
                                                     !endsWith(colnames(flood_risk), "fs_fema_difference_2020") & 
                                                     !endsWith(colnames(flood_risk), "fema_sfha")]

merged_flood_risk_mat <- matrix(NA, nrow = nrow(places_dat_wide), ncol = length(flood_risk_colnames_subset))

no_f_dat <- 0

merged_mat_idx <- 1

zcta_crosswalk$GEOID <- as.numeric(zcta_crosswalk$GEOID)

for (fip in places_dat_wide$fips) {
  
  one_tract_mult_zip <- zcta_crosswalk[zcta_crosswalk$GEOID == fip, names(zcta_crosswalk) %in% c("ZCTA5", "TRHUPCT")]
  
  one_tract_mult_zip <- rename(one_tract_mult_zip, zipcode = ZCTA5)
  
  one_tract_mult_zip_flood_risk <- merge(one_tract_mult_zip, flood_risk, by = "zipcode")
  
  # # zip codes making up the tract
  # zips <- one_tract_mult_zip$ZCTA5
  
  # # corresponding TRHUPCT, i.e. percentage of housing units covered by zip code within the tract,
  # # turned back into decimal
  # trhupcts <- one_tract_mult_zip$TRHUPCT[one_tract_mult_zip$ZCTA5 %in% zip_flood_risk_mat$zipcode] / 100
  
  
  
  col_idx <- 1
  
  for (coln in flood_risk_colnames_subset) {
    
    if (nrow(one_tract_mult_zip_flood_risk) == 0) {
      
      merged_flood_risk_mat[merged_mat_idx, col_idx] <- NaN
      
      no_f_dat <- no_f_dat + 1
      
    } else {
      
      merged_flood_risk_mat[merged_mat_idx, col_idx] <- sum(one_tract_mult_zip_flood_risk[coln] * 
                                                              one_tract_mult_zip_flood_risk$TRHUPCT, na.rm = T) / 
        sum(one_tract_mult_zip_flood_risk$TRHUPCT[!is.na(one_tract_mult_zip_flood_risk[coln])])
      
    }
    
    col_idx <- col_idx + 1
    
  }
  
  merged_mat_idx <- merged_mat_idx + 1
  
}

colnames(merged_flood_risk_mat) <- flood_risk_colnames_subset

saveRDS(merged_flood_risk_mat, file = here("intermediary_data/merged_flood_risk_mat_sw_states_census_tract.rds")) 

flood_health <- data.frame(places_dat_wide, merged_flood_risk_mat)

