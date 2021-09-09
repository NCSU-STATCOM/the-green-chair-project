library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)

tgcp_demog <- read_excel("C:/Users/CKA/Downloads/STATCOM_data.xlsx", col_types = "text")

tgcp_demog <- rename(tgcp_demog, Id = `...1`)

#convert the data set from wide format to longer format (by roll up the household Member Information and drop the missing values as needed)
Longer_Format <- pivot_longer(tgcp_demog,col = 15:118,names_to = "HHMemberInfo",values_to = "HHMemberInfoAnswer",values_drop_na = TRUE) 


#unite the client last name and client first name 
C <- unite(Longer_Format,ClientName, ends_with("Name"), sep = " ", na.rm = FALSE)

#unite the HHMemberInfo and InfoAnswer into one col with ":" separate. 
D <- unite(C,HHMemberInfo, starts_with("HHMemberInfo"), sep = ": ", na.rm = FALSE)


# convert the necessary variables from char to numeric values 
E <- D[, c(1,2,5,13,14,20,21)] <- sapply(D[,c(1,2,5,13,14,20,21)], as.numeric)
str(D)

#create the new rows containing number of beds total in the household (adding QueensBed and TwinBeds)
F <- mutate(D,Beds = QueenBeds+TwinBeds)
#delete cols: QueensBeds and TwinsBeds
G <- subset(F, select = -c(TwinBeds,QueenBeds) )


#trying to deal with the ClientGender and Client Age issues since ClientAge contains some gender info that are not stated in the ClientGender col 

H <- G %>%
  mutate(ClientGenderFill = case_when(
    endsWith(ClientAge, "Male") ~ "Male",
    endsWith(ClientAge, "Female") ~ "Female"
    
  ))

View(H)





