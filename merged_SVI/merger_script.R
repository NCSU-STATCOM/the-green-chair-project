##STATCOM--CDC Census Tract and GC Data Merger
##Script For Merging Green Chair data with CDC data


library(dplyr)

dir<-"/Users/Alvin/Documents/NCSU_Fall_2021/TheGreenChairProject/the-green-chair-project/merged_SVI/"

##External CDC data
file_18<-"NorthCarolina.csv"

NC_18<-read.csv(paste0(dir,file_18))

###Zip to Tract Data
zip_tract_file<-"zcta_tract_rel_10.txt"
zt_data<-read.csv(paste0(dir,zip_tract_file))

###Green Chair Data
# used reading_in_data.R, set GC_data to tgcp_demog
green_chair_file<-"STATCOM_data.xlsx - Program Referrals.csv"
GC_data<-read.csv(paste0(dir,green_chair_file))

GC_data$ClientZipCode <- as.numeric(GC_data$ClientZipCode)

##Cleaning zipcodes--Changes Zip codes with only 4 numbers to add a zero at end; 
##                      if 6 numbers or zip not in census or zip not in NC, changed to NA
for (i in 1:length(GC_data$ClientZipCode)){
  if (is.na(GC_data$ClientZipCode[i])==FALSE){
    if (nchar(as.character(GC_data$ClientZipCode[i]))==4){
      GC_data$ClientZipCode[i]<-GC_data$ClientZipCode[i]*10
    }
    if (nchar(as.character(GC_data$ClientZipCode[i]))==6|
             (GC_data$ClientZipCode[i]%in%zt_data$ZCTA5==FALSE)|
             (37%in%zt_data$STATE[zt_data$ZCTA5==GC_data$ClientZipCode[i]]==FALSE)){
      GC_data$ClientZipCode[i]<-NA
    }
  }
}

##Removing Anything Ending with FirstName, LastName, or Birthday
keep<-colnames(GC_data)[!endsWith(colnames(GC_data), "FirstName")&!endsWith(colnames(GC_data), "LastName")&
          !endsWith(colnames(GC_data),"Birthday")]

GC_data<-GC_data[colnames(GC_data)%in%keep]

##Creating New Summary Variables By ZPOPPCT--The Percentage of Total Population of the ZCTA
##                                              represented by the record
ZPOPPCT_summary<- zt_data %>% group_by(ZCTA5) %>% summarise(ZPOPPCT_sum = sum(ZPOPPCT))

##Yes, most zip codes are wholly accounted for by the census tracts
mean(ZPOPPCT_summary$ZPOPPCT_sum>=99)


##Actual merger loop
NC_18_col_subset<-colnames(NC_18[,8:ncol(NC_18)])

merged_GC_mat <- matrix(NA, nrow = nrow(GC_data), ncol = length(NC_18_col_subset))

no_f_dat <- 0

merged_mat_idx <- 1

##Replace -999 Null values with NA
NC_18[NC_18==-999]<-NA


for (zip in GC_data$ClientZipCode){
  if (!is.na(zip)){
    ##Grabs the geoids/tracts and percent of pop. covered by that geoid, for this zipcode
    one_zip_mult_tract <- zt_data[zt_data$ZCTA5 == zip, names(zt_data) %in% c("GEOID", "ZPOPPCT")]
    one_zip_mult_tract<-rename(one_zip_mult_tract, FIPS="GEOID")
  }
  else{
    one_zip_mult_tract<-data.frame(FIPS=NaN,ZPOPPCT=NaN)
  }
  
  
  ##Gets NC_18 for These Tracts
  one_zip_mult_tract_GC <- merge(one_zip_mult_tract, NC_18, by = "FIPS")
  
  col_idx<-1
  for (coln in NC_18_col_subset){
    if (nrow(one_zip_mult_tract_GC) == 0) {
      merged_GC_mat[merged_mat_idx, col_idx] <- NaN
      
      no_f_dat <- no_f_dat + 1
      
    }
    ##There are tracts and percent pop. covered by the zip code for this tract
    else {
      ##Take weighted sum
      merged_GC_mat[merged_mat_idx, col_idx] <- sum(one_zip_mult_tract_GC[coln] * 
                                                      one_zip_mult_tract_GC$ZPOPPCT, na.rm = T) / 
        sum(one_zip_mult_tract_GC$ZPOPPCT[!is.na(one_zip_mult_tract_GC[coln])])
    }
    col_idx <- col_idx + 1
  }
  merged_mat_idx <- merged_mat_idx+1
}

colnames(merged_GC_mat) <- NC_18_col_subset

##Merged dataset
new_GC_data<-data.frame(GC_data,merged_GC_mat)

##Writes csv for dataframe create
write.csv(x=new_GC_data, file=paste(dir,"merged_CDC_GC.csv",sep=""))



# adding SVIs to the cleaned dataset

cleaned_STATCOM_data <- readRDS("cleaned_STATCOM_data.rds")

cleaned_STATCOM_data_SVI <- data.frame(cleaned_STATCOM_data, merged_GC_mat)

# removing extraneous SVI variables
cleaned_STATCOM_data_SVI2 <- cleaned_STATCOM_data_SVI %>% select(!(starts_with("E_") & !ends_with(c("TOTPOP", "HU", "HH")))) %>%
  select(!starts_with(c("MP_", "M_", "EPL_", "SPL_", "RPL_", "F_")))

saveRDS(cleaned_STATCOM_data_SVI2, file = "cleaned_STATCOM_data_SVI.rds")



# data_dir<-"/Users/Alvin/Documents/NCSU_Fall_2021/TheGreenChairProject/the-green-chair-project/merged_SVI/"
# 
# GC_SVI_file<-"merged_CDC_GC.csv"
# 
# GC_SVI<-read.csv(paste0(data_dir,GC_SVI_file))
# GC_SVI[GC_SVI==""]<-NA
# 
# # GC_SVI$Timestamp<-as.Date(GC_SVI$Timestamp, format ="%d-%b-%Y")
# 
# GC_SVI$Veteran[GC_SVI$Veteran=="Yes"]<-1
# GC_SVI$Veteran[GC_SVI$Veteran!="1" | is.na(GC_SVI$Veteran)]<-0
# GC_SVI$Veteran<-as.numeric(GC_SVI$Veteran)
# 
# GC_SVI$Incarcerated[GC_SVI$Incarcerated=="Yes"]<-1
# GC_SVI$Incarcerated[GC_SVI$Incarcerated!="1"| is.na(GC_SVI$Incarcerated)]<-0
# GC_SVI$Incarcerated<-as.numeric(GC_SVI$Incarcerated)
# 
# GC_SVI$Disability[GC_SVI$Disability=="Yes"]<-1
# GC_SVI$Disability[GC_SVI$Disability!="1"| is.na(GC_SVI$Disability)]<-0
# GC_SVI$Disability<-as.numeric(GC_SVI$Disability)
# 
# bed_req<-GC_SVI[,colnames(GC_SVI)[endsWith(colnames(GC_SVI), "BedReq")]]
# 
# for (i in 1:nrow(bed_req)){
#   for (j in 1:ncol(bed_req)){
#     if (is.na(bed_req[i,j])){
#       bed_req[i,j]<-"0"
#     }
#     else{
#       if (startsWith(bed_req[i,j], "Yes")){
#         bed_req[i,j]<-"1"
#       }
#       else{
#         bed_req[i,j]<-"0"
#       }
#     }
#   }
# }
# 
# ##Assume that all NAs for those who have put down bed request are Nos
# 
# bed_req<-as.data.frame(lapply(bed_req,as.numeric))
# 
# GC_SVI<-cbind(GC_SVI, totalBedNeeded=rowSums(bed_req))



# exploring SVIs

GC_SVI_EP <- GC_SVI %>% select(!(starts_with("E_") & !ends_with(c("TOTPOP", "HU", "HH")))) %>%
  select(!starts_with(c("MP_", "M_", "EPL_", "SPL_", "RPL_", "F_")))

summary(GC_SVI_EP[, 101:121])

apply(GC_SVI_EP[, 101:121], 2, function(x) var(x, na.rm = T))
  
  


