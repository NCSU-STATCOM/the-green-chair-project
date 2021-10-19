# Data cleaning and wrangling
## Read in the raw data
library(readxl)
library(here)
greenchair <- read_excel(here("STATCOM_data.xlsx"), na= "NA")
nms <- colnames(greenchair)
ct <- ifelse(grepl("ClientZipCode|QueenBeds|Cribs|TwinBeds|TotalHHNumber", nms), "numeric", "guess")
greenchair<- as.data.frame(read_excel(here("STATCOM_data.xlsx"), na= "NA", col_types = ct))
str(greenchair[,1:21])
str(greenchair[,118:128])

## Column A_Variable_Assign a column name
colnames(greenchair)[1] <- "ID"

## Column B_Variable_"Timestamp"
greenchair$Timestamp <- as.Date(greenchair$Timestamp)

## Column G_Variable_"ClientAge"
greenchair$ClientGender[which(greenchair$ClientAge == "Adult (age 18 or over) Male")] <- "Male"
greenchair$ClientGender[which(greenchair$ClientAge == "Adult (age 18 or over) Female")] <- "Female"
greenchair$ClientAge[is.na(greenchair$ClientAge)] <- "N/A" 
greenchair$ClientAge <- sub(pattern = ".*(Adult).*", replacement = "\\1", greenchair$ClientAge)
greenchair$ClientAge <- sub(pattern = ".*(Child).*", replacement = "\\1", greenchair$ClientAge)
greenchair$ClientAge <- sub(pattern = ".*(Audlt).*", replacement = "\\1", greenchair$ClientAge)
greenchair$ClientAge <- sub(pattern = "Audlt", replacement = "Adult", greenchair$ClientAge)
greenchair$ClientAge[which(greenchair$ClientAge == "Male" | greenchair$ClientAge == "Female")] <- "N/A"
greenchair$ClientAge[which(greenchair$ClientAge < 18)] <- "Child"
greenchair$ClientAge[which(greenchair$ClientAge != "Adult" 
                           & greenchair$ClientAge != "Child" 
                           & greenchair$ClientAge != "N/A")] <- "Adult"
greenchair$ClientAge <- as.factor(greenchair$ClientAge)

## Column H_Variable_"ClientGender"
greenchair$ClientGender[is.na(greenchair$ClientGender)] <- "N/A"
greenchair$ClientGender <- as.factor(greenchair$ClientGender)

## Column I_Variable_"Ethnicity"
greenchair$Ethnicity[which(greenchair$Ethnicity == "Hispanic Latino/ Latinx"
                           | greenchair$Ethnicity == "Hispanic Latino/ LatinX" 
                           | greenchair$Ethnicity == "Hispanic Latino/LatinX"
                           | greenchair$Ethnicity == "Hispanic/Latino")] <- "Hispanic or Latino"
greenchair$Ethnicity[which(greenchair$Ethnicity == "Non- Hispanic or Latino/Latinx"
                           | greenchair$Ethnicity == "Non-Hispanic Latino/ LatinX " 
                           | greenchair$Ethnicity == "NOn-Hispanic Latino/ LatinX"
                           | greenchair$Ethnicity == "Non-Hispanic Latino/LatinX"
                           | greenchair$Ethnicity == "Non-Hispanic Latino/ LatinX")] <- "Not Hispanic or Latino"
greenchair$Ethnicity[which(greenchair$Ethnicity == "N/A (Community Housing)"
                           | greenchair$Ethnicity == "N/A Community Housing" 
                           | greenchair$Ethnicity == "N/A Spaces & Places"
                           | greenchair$Ethnicity == "Unknown")] <- "N/A"
greenchair$Ethnicity[is.na(greenchair$Ethnicity)] <- "N/A"
greenchair$Ethnicity <- as.factor(greenchair$Ethnicity)

## Column J_Variable_"Race"
greenchair$Race[which(greenchair$Race == "American Indian or Alaskan Native"
                      | greenchair$Race == "American Indian or Native Alaskan")] <- "American Indian or Alaska Native"
greenchair$Race[which(greenchair$Race == "Black/African-American")] <- "Black or African American"
greenchair$Race[which(greenchair$Race == "Multiple races"
                      | greenchair$Race == "Black or African American,
                      American Indian or Alaskan Native"
                      | greenchair$Race == "Black or African American, Other"
                      | greenchair$Race == "Native Hawaiian or Other Pacific Islander, Other"
                      | greenchair$Race == "White, Black or African American"
                      | greenchair$Race == "White, Other")] <- "Multiracial"
greenchair$Race[which(greenchair$Race == "N/A (Community Housing)"
                      | greenchair$Race == "N/A Community Housing" 
                      | greenchair$Race == "N/A Spaces & Places")] <- "N/A"

## Column K_Variable_"Veteran"
greenchair$Veteran[which(greenchair$Veteran != "Yes" 
                         & greenchair$Veteran != "No")] <- "N/A"
greenchair$Veteran[which(greenchair$Veteran == "N/A Community Housing")] <- "N/A"
greenchair$Veteran[is.na(greenchair$Veteran)] <- "N/A"
greenchair$Veteran <- as.factor(greenchair$Veteran)

## Column L_Variable_"Incarcerated"
greenchair$Incarcerated[which(greenchair$Incarcerated != "Yes" 
                              & greenchair$Incarcerated != "No")] <- "N/A"
greenchair$Incarcerated[is.na(greenchair$Incarcerated)] <- "N/A"
greenchair$Incarcerated <- as.factor(greenchair$Incarcerated)

## Column M_Variable_"Disability"
greenchair$Disability[which(greenchair$Disability== "Unknown")] <- "N/A"
greenchair$Disability[is.na(greenchair$Disability)] <- "N/A"
greenchair$Disability <- as.factor(greenchair$Disability)

## Column N_Variable_"AnnualIncomeAmount"
greenchair$AnnualIncomeAmount <- as.numeric(greenchair$AnnualIncomeAmount)
greenchair$AnnualIncomeAmount[is.na(greenchair$AnnualIncomeAmount)] <- NA
summary(greenchair$AnnualIncomeAmount)

## Column O_Variable_"TotalHHNumber"
greenchair$TotalHHNumber <- as.numeric(greenchair$TotalHHNumber)
greenchair$TotalHHNumber[is.na(greenchair$TotalHHNumber)] <- NA
summary(greenchair$TotalHHNumber)

## Column DO_Variable_"QueenBeds"
greenchair$QueenBeds[is.na(greenchair$QueenBeds)] <- NA
summary(greenchair$QueenBeds)

## Column DP_Variable_"Assistance"
greenchair$Assistance <- sub(pattern = ".*(Disaster).*", replacement = "Disaster Relief", greenchair$Assistance)
greenchair$Assistance <- sub(pattern = ".*(Transition).*", replacement = "\\1", greenchair$Assistance)
greenchair$Assistance <- sub(pattern = ".*(transition).*", replacement = "\\1", greenchair$Assistance)
greenchair$Assistance[which(greenchair$Assistance== "transition")] <- "Transition"
greenchair$Assistance[which(greenchair$Assistance== "0")] <- "Transition"
greenchair$Assistance[is.na(greenchair$Assistance)] <- "N/A"
greenchair$Assistance <- as.factor(greenchair$Assistance)

## Column DQ_Variable_"Circumstance"
greenchair$'COVID-19'[which(greenchair$Circumstance == "COVID-19"
                            | greenchair$Circumstance =="Fire-COVID"
                            | greenchair$Circumstance =="Homelessness-COVID"
                            | greenchair$Circumstance == "Low Income-COVID"
                            | greenchair$Circumstance == "Client is relocating for other disaster 
                            (fire, flooding, pests)-COVID")] <- "COVID"
greenchair$Assistance[which(greenchair$Circumstance == "community referrals, 
                            support students for academic success")] <- "Sweeter Dreams"
greenchair$Incarcerated[grepl(pattern = "(incarceration)|(Incarceration)", greenchair$Circumstance)]<- "Yes"
greenchair$Disability[which(greenchair$Circumstance == "Disability")] <- "Yes"
greenchair$Disability[which(greenchair$Circumstance == 
                              "Client is in subsidized housing intended for low income elderly individuals with a disability")] <- "Yes"
greenchair$Circumstance[which(greenchair$Circumstance == "Client is re-entering after incarceration")] <- "N/A"
greenchair$Circumstance <- gsub("(Client is re-entering after incarceration)", 
                                "",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is exiting homelessness)", 
                                "Homelessness",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is exiting a mental health facility)", 
                                "Mental Health Facility",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client in subsized housing intended for low income)", 
                                "Low Income",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is relocating for other disaster \\(fire, flooding, pests))", 
                                "Other Disaster",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client experienced flooded apartment and has no furnishing)", 
                                "Other Disaster",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is exiting a health care facility)", 
                                "Health Care Facility",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is exiting a substance use recovery facility/ program)", 
                                "Recovery Facility",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is fleeing Domestic Violence)", 
                                "Domestic Violence",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is relocating after a natural disaster \\(hurricane, tornado, etc.))", 
                                "Natural Disaster",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is housed and does not meet the other referral reasons)", 
                                "Housed",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is housed)", "Housed",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is exiting foster care)", "Foster Care",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client is resettling in the U.S.)", "Resettling",greenchair$Circumstance)
greenchair$Circumstance <- gsub("(Client settling into home)", "Resettling",greenchair$Circumstance)
greenchair$Circumstance <- gsub("Family established into new home.", "Resettling",greenchair$Circumstance)
greenchair$Circumstance[which(greenchair$Circumstance == "Sweeter Dreams"
                              | greenchair$Circumstance =="Community Housing"
                              | greenchair$Circumstance == "community referrals, support students for academic success")] <- "N/A"
greenchair$Circumstance <- sub(pattern = "COVID-19", replacement = "N/A", greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Fire-COVID", 
                               replacement = "Other Disaster", 
                               greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Fire", 
                               replacement = "Other Disaster", 
                               greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Client is relocating for other disaster [(fire, flooding, pests)]-COVID", 
                               replacement = "Other Disaster", 
                               greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Pest Infestation", 
                               replacement = "Other Disaster", 
                               greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Low Income-COVID", 
                               replacement = "Low Income", 
                               greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Homelessness-COVID", 
                               replacement = "Homelessness", 
                               greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Disability", replacement = "N/A", greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Client is in subsidized housing intended for low income elderly individuals with a disability",
                               replacement = "N/A", greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Job loss*", replacement = "Job Loss", greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "relocation", replacement = "Relocation", greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Re-entry after incarceration", 
                               replacement = "Re-entry after Incarceration", greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "Re-entry after Incarceration",
                               replacement = "N/A", greenchair$Circumstance)
greenchair$Circumstance <- sub(pattern = "(Addiction/Recovery)|(Addition / Recovery)",
                               replacement = "N/A", greenchair$Circumstance)
greenchair$Circumstance <- gsub(", $", "", greenchair$Circumstance)
greenchair$Circumstance[which(greenchair$Circumstance == "referrals to services")] <- "Referrals"
greenchair$Circumstance[is.na(greenchair$Circumstance)] <- "N/A"
greenchair$Circumstance <- as.factor(greenchair$Circumstance)

## Column DR_Variable_"HomeSize"
greenchair$HomeSize <- sub(pattern = ".*(Child).*", replacement = "\\N/A", greenchair$HomeSize)
greenchair$HomeSize <- sub(pattern = ".*(Israel Williams).*", replacement = "\\N/A", greenchair$HomeSize)
greenchair$HomeSize <- sub(pattern = ".*(1).*", replacement = "\\1 Bedroom", greenchair$HomeSize)
greenchair$HomeSize <- sub(pattern = ".*(2).*", replacement = "2 Bedrooms", greenchair$HomeSize)
greenchair$HomeSize <- sub(pattern = ".*(3).*", replacement = "3 Bedrooms", greenchair$HomeSize)
greenchair$HomeSize <- sub(pattern = ".*(4).*", replacement = "4 Bedrooms", greenchair$HomeSize)
greenchair$HomeSize <- sub(pattern = ".*(5).*", replacement = "5 Bedrooms", greenchair$HomeSize)
greenchair$HomeSize <- sub(pattern = ".*(6).*", replacement = "6 Bedrooms", greenchair$HomeSize)
greenchair$HomeSize <- sub(pattern = ".*(Studio).*", replacement = "\\1", greenchair$HomeSize)
greenchair$HomeSize <- sub(pattern = "bedrooms extra large", replacement = "Bedrooms extra large", greenchair$HomeSize)
greenchair$HomeSize[which(greenchair$HomeSize == "unkown" | greenchair$HomeSize == "Unknown" 
                          | greenchair$HomeSize == "Unkown")] <- "N/A"
greenchair$HomeSize[which(greenchair$HomeSize == "2 bathrooms")] <- "2 Bedrooms"
greenchair$HomeSize[is.na(greenchair$HomeSize)] <- "N/A"
greenchair$HomeSize <- as.factor(greenchair$HomeSize)

## Column DS_Variable_"FurnishingFeePayment"
greenchair$FurnishingFeePayment <- sub(pattern = ".*(agency).*", replacement = "Agency", 
                                       greenchair$FurnishingFeePayment)
greenchair$FurnishingFeePayment <- sub(pattern = ".*(Scholarship).*",
                                       replacement = "Scholarship", greenchair$FurnishingFeePayment)
greenchair$FurnishingFeePayment[which(greenchair$FurnishingFeePayment == "$150 + tax = $161.63 (51-75 points)"
                                      | greenchair$FurnishingFeePayment == "$200 + tax = $215.50 (76-100 points)")] <- "N/A"
greenchair$FurnishingFeePayment <- sub(pattern = ".*(Catholic Charities).*", 
                                       replacement = "Catholic Charities", greenchair$FurnishingFeePayment)
greenchair$FurnishingFeePayment <- sub(pattern = ".*(House Wake!).*", 
                                       replacement = "House Wake!", greenchair$FurnishingFeePayment)
greenchair$FurnishingFeePayment <- sub(pattern = ".*(Triangle Red Cross).*", 
                                       replacement = "Triangle Red Cross", greenchair$FurnishingFeePayment)
greenchair$FurnishingFeePayment <- sub(pattern = "Participant|Particpant", 
                                       replacement = "Participant", greenchair$FurnishingFeePayment)
greenchair$FurnishingFeePayment <- sub(pattern = "Referring Agency", 
                                       replacement = "Agency", greenchair$FurnishingFeePayment)
greenchair$FurnishingFeePayment <- sub(pattern = ".*(Client).*",
                                       replacement = "Participant", greenchair$FurnishingFeePayment)
greenchair$FurnishingFeePayment[is.na(greenchair$FurnishingFeePayment)] <- "N/A"
greenchair$FurnishingFeePayment <- as.factor(greenchair$FurnishingFeePayment)

## Column DT_Variable_"COVID-19"
greenchair$'COVID-19'[which(greenchair$'COVID-19' == "COIVD" | greenchair$'COVID-19' == "COViD" 
                            | greenchair$'COVID-19' == "COVID-19"
                            | greenchair$'COVID-19' == "COVID`")]<- "COVID"
greenchair$'COVID-19'[is.na(greenchair$'COVID-19')] <- "N/A"
greenchair$'COVID-19' <- as.factor(greenchair$'COVID-19')
greenchair$'COVID-19' <- as.factor(greenchair$'COVID-19')

## Column DU_Variable_"Cribs"
greenchair$Cribs[is.na(greenchair$Cribs)] <- NA
summary(greenchair$Cribs)

## Column DV_Variable_"TwinBeds"
greenchair$TwinBeds[is.na(greenchair$TwinBeds)] <- NA
summary(greenchair$TwinBeds)

## Column DW_Variable_"NoteInPocket"
greenchair$NoteInPocket[which(greenchair$NoteInPocket == "no")] <- "No"
greenchair$NoteInPocket[is.na(greenchair$NoteInPocket)] <- "N/A"
greenchair$NoteInPocket <- as.factor(greenchair$NoteInPocket)

## Column DX_Variable_"ChildSleeping"
greenchair$ChildSleeping[is.na(greenchair$ChildSleeping)] <- "N/A"
greenchair$ChildSleeping[which(greenchair$ChildSleeping == "sleeping on floor"
                               | greenchair$ChildSleeping == "Sleeping on floor")] <- "Sleeping on the floor"
greenchair$ChildSleeping <- as.factor(greenchair$ChildSleeping)





# Explore the Income status of the city based on the different years
## Subset for the different years
table(greenchair$Timestamp)
greenchair2010 <- subset(greenchair,  Timestamp>= "2010-01-01" & Timestamp <= "2010-12-31")
greenchair2011 <- subset(greenchair,  Timestamp>= "2011-01-01" & Timestamp <= "2011-12-31")
greenchair2012 <- subset(greenchair,  Timestamp>= "2012-01-01" & Timestamp <= "2012-12-31")
greenchair2013 <- subset(greenchair,  Timestamp>= "2013-01-01" & Timestamp <= "2013-12-31")
greenchair2014 <- subset(greenchair,  Timestamp>= "2014-01-01" & Timestamp <= "2014-12-31")
greenchair2015 <- subset(greenchair,  Timestamp>= "2015-01-01" & Timestamp <= "2015-12-31")
greenchair2016 <- subset(greenchair,  Timestamp>= "2016-01-01" & Timestamp <= "2016-12-31")
greenchair2017 <- subset(greenchair,  Timestamp>= "2017-01-01" & Timestamp <= "2017-12-31")
greenchair2018 <- subset(greenchair,  Timestamp>= "2018-01-01" & Timestamp <= "2018-12-31")
greenchair2019 <- subset(greenchair,  Timestamp>= "2019-01-01" & Timestamp <= "2019-12-31")
greenchair2020 <- subset(greenchair,  Timestamp>= "2020-01-01" & Timestamp <= "2020-12-31")
greenchair2021 <- subset(greenchair,  Timestamp>= "2021-01-01" & Timestamp <= "2021-12-31")


## 2010
Homeincome <- c()

for (i in 1:nrow(greenchair2010)){
  if (is.na(greenchair2010[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2010[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2010[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2010[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2010[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2010[i,"AnnualIncomeAmount"] <= 16350)
    {greenchair2010[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2010[i,"AnnualIncomeAmount"] > 32640)
    {greenchair2010[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2010[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2010[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2010[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2010[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2010[i,"AnnualIncomeAmount"] <= 18650){
      greenchair2010[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2010[i,"AnnualIncomeAmount"]> 37320)
    {greenchair2010[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2010[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2010[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2010[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2010[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2010[i,"AnnualIncomeAmount"]<= 21000)
    {greenchair2010[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2010[i,"AnnualIncomeAmount"]> 42000)
    {greenchair2010[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2010[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2010[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2010[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2010[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2010[i,"AnnualIncomeAmount"]<= 23300)
    {greenchair2010[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2010[i,"AnnualIncomeAmount"]> 46620){
      greenchair2010[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2010[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2010[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2010[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2010[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2010[i,"AnnualIncomeAmount"]<= 25200)
    {greenchair2010[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2010[i,"AnnualIncomeAmount"]> 50400){
      greenchair2010[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2010[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2010[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2010[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2010[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2010[i,"AnnualIncomeAmount"]<= 27050)
    {greenchair2010[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2010[i,"AnnualIncomeAmount"]> 54120){
      greenchair2010[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2010[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2010[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2010[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2010[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2010[i,"AnnualIncomeAmount"]<= 28900)
    {greenchair2010[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2010[i,"AnnualIncomeAmount"]> 57840){
      greenchair2010[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2010[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2010[i,"TotalHHNumber"] == 8 | greenchair2010[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2010[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2010[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2010[i,"AnnualIncomeAmount"]<= 30800)
    {greenchair2010[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2010[i,"AnnualIncomeAmount"]> 61560){
      greenchair2010[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2010[i,"Homeincome"] <- "Low Income"
    }
  }
}


## 2011
Homeincome <- c()

for (i in 1:nrow(greenchair2011)){
  if (is.na(greenchair2011[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2011[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2011[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2011[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2011[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2011[i,"AnnualIncomeAmount"] <= 16600)
    {greenchair2011[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2011[i,"AnnualIncomeAmount"] > 33120)
    {greenchair2011[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2011[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2011[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2011[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2011[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2011[i,"AnnualIncomeAmount"] <= 18950){
      greenchair2011[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2011[i,"AnnualIncomeAmount"]> 37860)
    {greenchair2011[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2011[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2011[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2011[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2011[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2011[i,"AnnualIncomeAmount"]<= 21300)
    {greenchair2011[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2011[i,"AnnualIncomeAmount"]> 42600)
    {greenchair2011[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2011[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2011[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2011[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2011[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2011[i,"AnnualIncomeAmount"]<= 23650)
    {greenchair2011[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2011[i,"AnnualIncomeAmount"]> 47280){
      greenchair2011[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2011[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2011[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2011[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2011[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2011[i,"AnnualIncomeAmount"]<= 25550)
    {greenchair2011[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2011[i,"AnnualIncomeAmount"]> 51120){
      greenchair2011[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2011[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2011[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2011[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2011[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2011[i,"AnnualIncomeAmount"]<= 27450)
    {greenchair2011[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2011[i,"AnnualIncomeAmount"]> 54900){
      greenchair2011[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2011[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2011[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2011[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2011[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2011[i,"AnnualIncomeAmount"]<= 29350)
    {greenchair2011[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2011[i,"AnnualIncomeAmount"]> 58680){
      greenchair2011[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2011[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2011[i,"TotalHHNumber"] == 8 | greenchair2011[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2011[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2011[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2011[i,"AnnualIncomeAmount"]<= 31250)
    {greenchair2011[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2011[i,"AnnualIncomeAmount"]> 62460){
      greenchair2011[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2011[i,"Homeincome"] <- "Low Income"
    }
  }
}


## 2012
Homeincome <- c()

for (i in 1:nrow(greenchair2012)){
  if (is.na(greenchair2012[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2012[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2012[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2012[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2012[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2012[i,"AnnualIncomeAmount"] <= 16800)
    {greenchair2012[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2012[i,"AnnualIncomeAmount"] > 33600)
    {greenchair2012[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2012[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2012[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2012[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2012[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2012[i,"AnnualIncomeAmount"] <= 19200){
      greenchair2012[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2012[i,"AnnualIncomeAmount"]> 38400)
    {greenchair2012[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2012[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2012[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2012[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2012[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2012[i,"AnnualIncomeAmount"]<= 21600)
    {greenchair2012[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2012[i,"AnnualIncomeAmount"]> 43200)
    {greenchair2012[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2012[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2012[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2012[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2012[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2012[i,"AnnualIncomeAmount"]<= 23950)
    {greenchair2012[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2012[i,"AnnualIncomeAmount"]> 47940){
      greenchair2012[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2012[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2012[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2012[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2012[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2012[i,"AnnualIncomeAmount"]<= 25900)
    {greenchair2012[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2012[i,"AnnualIncomeAmount"]> 51780){
      greenchair2012[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2012[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2012[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2012[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2012[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2012[i,"AnnualIncomeAmount"]<= 27800)
    {greenchair2012[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2012[i,"AnnualIncomeAmount"]> 55620){
      greenchair2012[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2012[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2012[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2012[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2012[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2012[i,"AnnualIncomeAmount"]<= 29700)
    {greenchair2012[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2012[i,"AnnualIncomeAmount"]> 59460){
      greenchair2012[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2012[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2012[i,"TotalHHNumber"] == 8 | greenchair2012[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2012[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2012[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2012[i,"AnnualIncomeAmount"]<= 31650)
    {greenchair2012[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2012[i,"AnnualIncomeAmount"]> 63300){
      greenchair2012[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2012[i,"Homeincome"] <- "Low Income"
    }
  }
}


## 2013
Homeincome <- c()

for (i in 1:nrow(greenchair2013)){
  if (is.na(greenchair2013[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2013[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2013[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2013[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2013[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2013[i,"AnnualIncomeAmount"] <= 16000)
    {greenchair2013[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2013[i,"AnnualIncomeAmount"] > 31920)
    {greenchair2013[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2013[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2013[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2013[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2013[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2013[i,"AnnualIncomeAmount"] <= 18250){
      greenchair2013[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2013[i,"AnnualIncomeAmount"]> 36480)
    {greenchair2013[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2013[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2013[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2013[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2013[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2013[i,"AnnualIncomeAmount"]<= 20550)
    {greenchair2013[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2013[i,"AnnualIncomeAmount"]> 41040)
    {greenchair2013[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2013[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2013[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2013[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2013[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2013[i,"AnnualIncomeAmount"]<= 22800)
    {greenchair2013[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2013[i,"AnnualIncomeAmount"]> 45600){
      greenchair2013[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2013[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2013[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2013[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2013[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2013[i,"AnnualIncomeAmount"]<= 24650)
    {greenchair2013[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2013[i,"AnnualIncomeAmount"]> 49260){
      greenchair2013[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2013[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2013[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2013[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2013[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2013[i,"AnnualIncomeAmount"]<= 26450)
    {greenchair2013[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2013[i,"AnnualIncomeAmount"]> 52920){
      greenchair2013[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2013[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2013[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2013[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2013[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2013[i,"AnnualIncomeAmount"]<= 28300)
    {greenchair2013[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2013[i,"AnnualIncomeAmount"]> 56580){
      greenchair2013[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2013[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2013[i,"TotalHHNumber"] == 8 | greenchair2013[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2013[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2013[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2013[i,"AnnualIncomeAmount"]<= 30100)
    {greenchair2013[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2013[i,"AnnualIncomeAmount"]> 60240){
      greenchair2013[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2013[i,"Homeincome"] <- "Low Income"
    }
  }
}

## 2014
Homeincome <- c()

for (i in 1:nrow(greenchair2014)){
  if (is.na(greenchair2014[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2014[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2014[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2014[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2014[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2014[i,"AnnualIncomeAmount"] <= 15950)
    {greenchair2014[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2014[i,"AnnualIncomeAmount"] > 31860)
    {greenchair2014[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2014[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2014[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2014[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2014[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2014[i,"AnnualIncomeAmount"] <= 18200){
      greenchair2014[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2014[i,"AnnualIncomeAmount"]> 36420)
    {greenchair2014[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2014[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2014[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2014[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2014[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2014[i,"AnnualIncomeAmount"]<= 20500)
    {greenchair2014[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2014[i,"AnnualIncomeAmount"]> 40980)
    {greenchair2014[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2014[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2014[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2014[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2014[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2014[i,"AnnualIncomeAmount"]<= 22750)
    {greenchair2014[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2014[i,"AnnualIncomeAmount"]> 45480){
      greenchair2014[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2014[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2014[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2014[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2014[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2014[i,"AnnualIncomeAmount"]<= 24600)
    {greenchair2014[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2014[i,"AnnualIncomeAmount"]> 49140){
      greenchair2014[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2014[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2014[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2014[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2014[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2014[i,"AnnualIncomeAmount"]<= 26400)
    {greenchair2014[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2014[i,"AnnualIncomeAmount"]> 52800){
      greenchair2014[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2014[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2014[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2014[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2014[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2014[i,"AnnualIncomeAmount"]<= 28250)
    {greenchair2014[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2014[i,"AnnualIncomeAmount"]> 56400){
      greenchair2014[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2014[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2014[i,"TotalHHNumber"] == 8 | greenchair2014[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2014[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2014[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2014[i,"AnnualIncomeAmount"]<= 30050)
    {greenchair2014[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2014[i,"AnnualIncomeAmount"]> 60060){
      greenchair2014[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2014[i,"Homeincome"] <- "Low Income"
    }
  }
}


## 2015
Homeincome <- c()

for (i in 1:nrow(greenchair2015)){
  if (is.na(greenchair2015[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2015[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2015[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2015[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2015[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2015[i,"AnnualIncomeAmount"] <= 16600)
    {greenchair2015[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2015[i,"AnnualIncomeAmount"] > 33120)
    {greenchair2015[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2015[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2015[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2015[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2015[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2015[i,"AnnualIncomeAmount"] <= 18950){
      greenchair2015[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2015[i,"AnnualIncomeAmount"]> 37860)
    {greenchair2015[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2015[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2015[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2015[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2015[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2015[i,"AnnualIncomeAmount"]<= 21300)
    {greenchair2015[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2015[i,"AnnualIncomeAmount"]> 42600)
    {greenchair2015[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2015[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2015[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2015[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2015[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2015[i,"AnnualIncomeAmount"]<= 23650)
    {greenchair2015[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2015[i,"AnnualIncomeAmount"]> 47280){
      greenchair2015[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2015[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2015[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2015[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2015[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2015[i,"AnnualIncomeAmount"]<= 25550)
    {greenchair2015[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2015[i,"AnnualIncomeAmount"]> 51120){
      greenchair2015[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2015[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2015[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2015[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2015[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2015[i,"AnnualIncomeAmount"]<= 27450)
    {greenchair2015[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2015[i,"AnnualIncomeAmount"]> 54900){
      greenchair2015[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2015[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2015[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2015[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2015[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2015[i,"AnnualIncomeAmount"]<= 29350)
    {greenchair2015[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2015[i,"AnnualIncomeAmount"]> 58680){
      greenchair2015[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2015[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2015[i,"TotalHHNumber"] == 8 | greenchair2015[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2015[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2015[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2015[i,"AnnualIncomeAmount"]<= 31250)
    {greenchair2015[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2015[i,"AnnualIncomeAmount"]> 62460){
      greenchair2015[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2015[i,"Homeincome"] <- "Low Income"
    }
  }
}


## 2016
Homeincome <- c()

for (i in 1:nrow(greenchair2016)){
  if (is.na(greenchair2016[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2016[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2016[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2016[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2016[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2016[i,"AnnualIncomeAmount"] <= 16100)
    {greenchair2016[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2016[i,"AnnualIncomeAmount"] > 32220)
    {greenchair2016[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2016[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2016[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2016[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2016[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2016[i,"AnnualIncomeAmount"] <= 18400){
      greenchair2016[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2016[i,"AnnualIncomeAmount"]> 36780)
    {greenchair2016[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2016[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2016[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2016[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2016[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2016[i,"AnnualIncomeAmount"]<= 20700)
    {greenchair2016[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2016[i,"AnnualIncomeAmount"]> 41400)
    {greenchair2016[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2016[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2016[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2016[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2016[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2016[i,"AnnualIncomeAmount"]<= 23000)
    {greenchair2016[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2016[i,"AnnualIncomeAmount"]> 45960){
      greenchair2016[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2016[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2016[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2016[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2016[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2016[i,"AnnualIncomeAmount"]<= 24850)
    {greenchair2016[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2016[i,"AnnualIncomeAmount"]> 49680){
      greenchair2016[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2016[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2016[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2016[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2016[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2016[i,"AnnualIncomeAmount"]<= 26700)
    {greenchair2016[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2016[i,"AnnualIncomeAmount"]> 53340){
      greenchair2016[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2016[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2016[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2016[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2016[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2016[i,"AnnualIncomeAmount"]<= 28550)
    {greenchair2016[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2016[i,"AnnualIncomeAmount"]> 57000){
      greenchair2016[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2016[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2016[i,"TotalHHNumber"] == 8 | greenchair2016[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2016[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2016[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2016[i,"AnnualIncomeAmount"]<= 30400)
    {greenchair2016[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2016[i,"AnnualIncomeAmount"]> 60720){
      greenchair2016[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2016[i,"Homeincome"] <- "Low Income"
    }
  }
}


## 2017
Homeincome <- c()

for (i in 1:nrow(greenchair2017)){
  if (is.na(greenchair2017[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2017[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2017[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2017[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2017[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2017[i,"AnnualIncomeAmount"] <= 16850)
    {greenchair2017[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2017[i,"AnnualIncomeAmount"] > 33720)
    {greenchair2017[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2017[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2017[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2017[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2017[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2017[i,"AnnualIncomeAmount"] <= 19250){
      greenchair2017[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2017[i,"AnnualIncomeAmount"]> 38520)
    {greenchair2017[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2017[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2017[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2017[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2017[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2017[i,"AnnualIncomeAmount"]<= 21650)
    {greenchair2017[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2017[i,"AnnualIncomeAmount"]> 43320)
    {greenchair2017[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2017[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2017[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2017[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2017[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2017[i,"AnnualIncomeAmount"]<= 24050)
    {greenchair2017[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2017[i,"AnnualIncomeAmount"]> 48120){
      greenchair2017[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2017[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2017[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2017[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2017[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2017[i,"AnnualIncomeAmount"]<= 26000)
    {greenchair2017[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2017[i,"AnnualIncomeAmount"]> 52020){
      greenchair2017[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2017[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2017[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2017[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2017[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2017[i,"AnnualIncomeAmount"]<= 27900)
    {greenchair2017[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2017[i,"AnnualIncomeAmount"]> 55860){
      greenchair2017[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2017[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2017[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2017[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2017[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2017[i,"AnnualIncomeAmount"]<= 29850)
    {greenchair2017[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2017[i,"AnnualIncomeAmount"]> 59700){
      greenchair2017[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2017[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2017[i,"TotalHHNumber"] == 8 | greenchair2017[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2017[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2017[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2017[i,"AnnualIncomeAmount"]<= 31750)
    {greenchair2017[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2017[i,"AnnualIncomeAmount"]> 63540){
      greenchair2017[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2017[i,"Homeincome"] <- "Low Income"
    }
  }
}


## 2018
Homeincome <- c()

for (i in 1:nrow(greenchair2018)){
  if (is.na(greenchair2018[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2018[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2018[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2018[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2018[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2018[i,"AnnualIncomeAmount"] <= 17750)
    {greenchair2018[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2018[i,"AnnualIncomeAmount"] > 35460)
    {greenchair2018[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2018[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2018[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2018[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2018[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2018[i,"AnnualIncomeAmount"] <= 20250){
      greenchair2018[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2018[i,"AnnualIncomeAmount"]> 40500)
    {greenchair2018[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2018[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2018[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2018[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2018[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2018[i,"AnnualIncomeAmount"]<= 22800)
    {greenchair2018[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2018[i,"AnnualIncomeAmount"]> 45540)
    {greenchair2018[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2018[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2018[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2018[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2018[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2018[i,"AnnualIncomeAmount"]<= 25300)
    {greenchair2018[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2018[i,"AnnualIncomeAmount"]> 50580){
      greenchair2018[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2018[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2018[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2018[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2018[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2018[i,"AnnualIncomeAmount"]<= 27350)
    {greenchair2018[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2018[i,"AnnualIncomeAmount"]> 54660){
      greenchair2018[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2018[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2018[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2018[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2018[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2018[i,"AnnualIncomeAmount"]<= 29350)
    {greenchair2018[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2018[i,"AnnualIncomeAmount"]> 58680){
      greenchair2018[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2018[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2018[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2018[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2018[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2018[i,"AnnualIncomeAmount"]<= 31400)
    {greenchair2018[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2018[i,"AnnualIncomeAmount"]> 62760){
      greenchair2018[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2018[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2018[i,"TotalHHNumber"] == 8 | greenchair2018[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2018[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2018[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2018[i,"AnnualIncomeAmount"]<= 33400)
    {greenchair2018[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2018[i,"AnnualIncomeAmount"]> 66780){
      greenchair2018[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2018[i,"Homeincome"] <- "Low Income"
    }
  }
}


## 2019
Homeincome <- c()

for (i in 1:nrow(greenchair2019)){
  if (is.na(greenchair2019[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2019[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2019[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2019[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2019[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2019[i,"AnnualIncomeAmount"] <= 19500)
    {greenchair2019[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2019[i,"AnnualIncomeAmount"] > 38940)
    {greenchair2019[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2019[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2019[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2019[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2019[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2019[i,"AnnualIncomeAmount"] <= 22250){
      greenchair2019[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2019[i,"AnnualIncomeAmount"]> 44520)
    {greenchair2019[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2019[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2019[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2019[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2019[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2019[i,"AnnualIncomeAmount"]<= 25050)
    {greenchair2019[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2019[i,"AnnualIncomeAmount"]> 50100)
    {greenchair2019[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2019[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2019[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2019[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2019[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2019[i,"AnnualIncomeAmount"]<= 27800)
    {greenchair2019[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2019[i,"AnnualIncomeAmount"]> 55620){
      greenchair2019[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2019[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2019[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2019[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2019[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2019[i,"AnnualIncomeAmount"]<= 30050)
    {greenchair2019[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2019[i,"AnnualIncomeAmount"]> 60120){
      greenchair2019[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2019[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2019[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2019[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2019[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2019[i,"AnnualIncomeAmount"]<= 32250)
    {greenchair2019[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2019[i,"AnnualIncomeAmount"]> 64560){
      greenchair2019[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2019[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2019[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2019[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2019[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2019[i,"AnnualIncomeAmount"]<= 34500)
    {greenchair2019[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2019[i,"AnnualIncomeAmount"]> 69000){
      greenchair2019[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2019[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2019[i,"TotalHHNumber"] == 8 | greenchair2019[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2019[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2019[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2019[i,"AnnualIncomeAmount"]<= 36700)
    {greenchair2019[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2019[i,"AnnualIncomeAmount"]> 73440){
      greenchair2019[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2019[i,"Homeincome"] <- "Low Income"
    }
  }
}


## 2020
Homeincome <- c()

for (i in 1:nrow(greenchair2020)){
  if (is.na(greenchair2020[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2020[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2020[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2020[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2020[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2020[i,"AnnualIncomeAmount"] <= 19800)
    {greenchair2020[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2020[i,"AnnualIncomeAmount"] > 39540)
    {greenchair2020[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2020[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2020[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2020[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2020[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2020[i,"AnnualIncomeAmount"] <= 22600){
      greenchair2020[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2020[i,"AnnualIncomeAmount"]> 45180)
    {greenchair2020[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2020[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2020[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2020[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2020[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2020[i,"AnnualIncomeAmount"]<= 25450)
    {greenchair2020[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2020[i,"AnnualIncomeAmount"]> 50820)
    {greenchair2020[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2020[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2020[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2020[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2020[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2020[i,"AnnualIncomeAmount"]<= 28250)
    {greenchair2020[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2020[i,"AnnualIncomeAmount"]> 56460){
      greenchair2020[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2020[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2020[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2020[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2020[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2020[i,"AnnualIncomeAmount"]<= 30550)
    {greenchair2020[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2020[i,"AnnualIncomeAmount"]> 61020){
      greenchair2020[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2020[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2020[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2020[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2020[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2020[i,"AnnualIncomeAmount"]<= 32800)
    {greenchair2020[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2020[i,"AnnualIncomeAmount"]> 65520){
      greenchair2020[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2020[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2020[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2020[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2020[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2020[i,"AnnualIncomeAmount"]<= 35050)
    {greenchair2020[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2020[i,"AnnualIncomeAmount"]> 70020){
      greenchair2020[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2020[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2020[i,"TotalHHNumber"] == 8 | greenchair2020[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2020[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2020[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2020[i,"AnnualIncomeAmount"]<= 37300)
    {greenchair2020[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2020[i,"AnnualIncomeAmount"]> 74580){
      greenchair2020[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2020[i,"Homeincome"] <- "Low Income"
    }
  }
}


## 2021
Homeincome <- c()

for (i in 1:nrow(greenchair2021)){
  if (is.na(greenchair2021[i,"TotalHHNumber"]))
  {TRUE 
    greenchair2021[i,"Homeincome"] <- "Unknown"}
  else if (greenchair2021[i,"TotalHHNumber"] == 1){
    if (is.na(greenchair2021[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2021[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2021[i,"AnnualIncomeAmount"] <= 20100)
    {greenchair2021[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2021[i,"AnnualIncomeAmount"] > 40200)
    {greenchair2021[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2021[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2021[i,"TotalHHNumber"] == 2){
    if (is.na(greenchair2021[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2021[i,"Homeincome"] <- "Unknown"
    }  
    else if (greenchair2021[i,"AnnualIncomeAmount"] <= 23000){
      greenchair2021[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2021[i,"AnnualIncomeAmount"]> 45960)
    {greenchair2021[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2021[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2021[i,"TotalHHNumber"] == 3){
    if (is.na(greenchair2021[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2021[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2021[i,"AnnualIncomeAmount"]<= 25850)
    {greenchair2021[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2021[i,"AnnualIncomeAmount"]> 51720)
    {greenchair2021[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2021[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2021[i,"TotalHHNumber"] == 4){
    if (is.na(greenchair2021[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2021[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2021[i,"AnnualIncomeAmount"]<= 28700)
    {greenchair2021[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2021[i,"AnnualIncomeAmount"]> 57420){
      greenchair2021[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2021[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2021[i,"TotalHHNumber"] == 5){
    if (is.na(greenchair2021[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2021[i,"Homeincome"] <- "Unknown"
    }   
    else if (greenchair2021[i,"AnnualIncomeAmount"]<= 31000)
    {greenchair2021[i,"Homeincome"] <- "Very Low Income"
    }
    else if (greenchair2021[i,"AnnualIncomeAmount"]> 62040){
      greenchair2021[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2021[i,"Homeincome"] <- "Low Income"
    }
    
  }
  else if (greenchair2021[i,"TotalHHNumber"] == 6){
    if (is.na(greenchair2021[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2021[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2021[i,"AnnualIncomeAmount"]<= 33300)
    {greenchair2021[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2021[i,"AnnualIncomeAmount"]> 66660){
      greenchair2021[i,"Homeincome"] <- "Median"
    }
    else
    {greenchair2021[i,"Homeincome"] <- "Low Income"
    }
  }
  else if (greenchair2021[i,"TotalHHNumber"] == 7){
    if (is.na(greenchair2021[i,"AnnualIncomeAmount"]))
    {TRUE 
      greenchair2021[i,"Homeincome"] <- "Unknown"
    }
    else if (greenchair2021[i,"AnnualIncomeAmount"]<= 35600)
    {greenchair2021[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2021[i,"AnnualIncomeAmount"]> 71220){
      greenchair2021[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2021[i,"Homeincome"] <- "Low Income"
    }
  }
  else (greenchair2021[i,"TotalHHNumber"] == 8 | greenchair2021[i,"TotalHHNumber"] == 9)
  {if (is.na(greenchair2021[i,"AnnualIncomeAmount"]))
  {TRUE 
    greenchair2021[i,"Homeincome"] <- "Unknown"}
    else if (greenchair2021[i,"AnnualIncomeAmount"]<= 37900)
    {greenchair2021[i,"Homeincome"] <- "Very Low Income"
    }    
    else if (greenchair2021[i,"AnnualIncomeAmount"]> 75840){
      greenchair2021[i,"Homeincome"] <- "Median"
    }
    else 
    {greenchair2021[i,"Homeincome"] <- "Low Income"
    }
  }
}

## Combine the subsets
greenchair2 <- rbind(greenchair2010,greenchair2011,greenchair2012,greenchair2013,
                     greenchair2014,greenchair2015,greenchair2016,greenchair2017,
                     greenchair2018,greenchair2019,greenchair2020,greenchair2021)
greenchair2$Homeincome <- factor(greenchair2$Homeincome,
                                 levels=c("Very Low Income", "Low Income","Median", "Unknown"))
greenchair2$Year <- substr(greenchair2$Timestamp,1,4)
greenchair2$Year <- as.factor(greenchair2$Year)
greenchair2$TotalHHNumber <- as.factor(greenchair2$TotalHHNumber)
