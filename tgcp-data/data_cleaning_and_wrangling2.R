
library(dplyr)

# Continuation of Yinqiao's Data cleaning and wrangling.R



# Agency Names

agency_clean <- read_excel("AgencyNames.xlsx", range = cell_cols("A:C"))

greenchair3 <- left_join(greenchair2, agency_clean, by = "Agency")

# Remove previous "Agency" variable and reorder columns

greenchair3 <- greenchair3 %>% relocate(Agency_Clean_Short, Agency_Clean_Full, 
                                        .after = Timestamp) %>% select(-Agency)



# Clean zip codes

##Cleaning zipcodes--Changes Zip codes with only 4 numbers to add a zero at end; 
##                      if 6 numbers or is zero, changed to NA
for (i in 1:length(greenchair3$ClientZipCode)){
  if (is.na(greenchair3$ClientZipCode[i])==FALSE){
    if (nchar(as.character(greenchair3$ClientZipCode[i]))==4){
      greenchair3$ClientZipCode[i]<-greenchair3$ClientZipCode[i]*10
    }
    if (nchar(as.character(greenchair3$ClientZipCode[i]))==6 | greenchair3$ClientZipCode[i] == 0){
      greenchair3$ClientZipCode[i]<-NA
    }
  }
}



# Remove personally identifiable information

##Removing Anything Ending with FirstName, LastName, or Birthday
greenchair3 <- greenchair3 %>% select(!ends_with(c("FirstName", "LastName", "Birthday")))

## Saving as rds object (preserves factor type)
saveRDS(greenchair3, file = "cleaned_STATCOM_data.rds")

##Writes csv for dataframe created
write.csv(x=greenchair3, file = "cleaned_STATCOM_data.csv", row.names = F)





