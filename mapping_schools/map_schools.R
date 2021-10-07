library(rgdal)
library(raster)
library(tidyverse)
library(readxl)

##################################################
#### 1. Read in shape file of all the schools in NC
shp_df <- read.csv("NC_schools.csv")
shp <- shp_df[,-1]
coordinates(shp) <- ~ coords.x1 + coords.x2
plot(shp)

##################################################
### 2. Read in STATCOM_data.xlsx
statcom_data <- read_excel("C:/Users/user/Desktop/STATCOM_data.xlsx", col_types = "text")

# Select columns involving which schools the individuals go to: 9 columns in total
school_col_names <- tidyselect::vars_select(colnames(statcom_data), contains('School', ignore.case = TRUE)) 
school_col_df <- statcom_data %>% dplyr::select(school_col_names, "ClientZipCode")


##################################################
### 3. Tidy up school_col_df
# remove columns containing only NA's: HHMember9School
school_col_df <- school_col_df %>% select_if(~!all(is.na(.))) 

# remove rows containing only NA's
school_col_df <- school_col_df %>% filter(if_any(!"ClientZipCode", ~ !is.na(.)))

# remove all notes in parentheses (ex. school is virtual)
school_col_df <- mutate_all(school_col_df, funs(str_replace(., " \\s*\\([^\\)]+\\)", "")))

# Change any annoying values to NA
rm_string <- c("n/a", "N/a", "N/A", "n/A", "na", "N?A",
               "None","Unknown","no","Not", "No",
               "0","1","2","3","4","5","6","7","8","9",
               "adult", "Parent", "Adult", "parent",
               "Starts school next year",
               "with family will return soon",
               "Checking with CM",
               "Calling for school info",
               "unsure",
               "was given", "sibling", 
               "home", "Home", 
               "day care", "Daycare", "daycare", "Day care", "Child Care",
               "Childcare Network",
               "Kinder", "kinder", "Pre", "pre",
               "New born", "infant",
               "below", "Online", "Virtual", "online", "virtual",
               "public school", "Public School", "Berkeley"
               )
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., str_detect(.,paste(rm_string, collapse = "|")), NA)))

# remove values with no specific school names
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "Elementary School", NA)))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "Elementary", NA)))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "Middle School", NA)))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "High School", NA)))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "High school", NA)))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "College", NA)))


# change acronyms ex. ES -> Elementary  etc.
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Elemantary School$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Elementary School$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "elementary$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Elem$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "ELem$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Elem School$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Elementay$", "Elementary")))

school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Middle School$", "Middle")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Middle school$", "Middle")))

school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "High School$", "High")))

school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "ES$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "EM$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "MS$", "Middle")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "HS$", "High")))

school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Elem.$", "Elementary")))

##### Wide to long
school_col_long <- gather(school_col_df, family_member ,school_names, c(school_col_names[1:8]))
school_col_long <- school_col_long %>% na.omit()
school_col_long <- school_col_long %>% dplyr::select(!"family_member")


##### Convert First letter to uppercase
school_col_long$school_names <- str_to_title(school_col_long$school_names) 

########################################################
##### Change specific school names (if they're obviously mistyped)
##### Note. If we don't know if the schools are elementary/middle/high, need to look at
##### original statcom.xlsx to determine individual's age. 
SchoolName <- shp_df$SchoolName

school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Baileywick Elementary$", "Baileywick Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Bailey Wick$", "Baileywick Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Bailey Wix$", "Baileywick Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Baileywick$", "Baileywick Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Bailwick$", "Baileywick Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("BaileyWick$", "Baileywick Road Elementary")

school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Barewell Elementary", "Barwell Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Barewell Road Elementary", "Barwell Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Barwell Elementary", "Barwell Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Barwell$", "Barwell Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Bowwell road Elementary", "Barwell Road Elementary")


school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Baucom$", "Baucom Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Beaverdam Elementary", "Beaver Dam Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Brentwood Center", "Brentwood Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Bridges Program", "Bridges Charter School")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Brier Creek$", "Brier Creek Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Brier Creek High", "Brier Creek Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Brogden", "Brogden Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Broughton", "Needham Broughton High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Bunn", "Bunn Middle")

school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Carol Middle", "Carroll Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Carrol Middle", "Carroll Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Carroll Magnet Middle", "Carroll Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Cary$", "Cary High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Centennial$", "Centennial Campus Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Centennial Magnet Middle", "Centennial Campus Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Centennial Middle$", "Centennial Campus Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Centennila Middle$", "Centennial Campus Middle")



View(unique(school_col_long[order(school_col_long$school_names),2]))
SchoolName[SchoolName %>% str_detect("Wake")]

school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "zebulon es", "Zebulon Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "HeritageElementary", "Heritage Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Dillar Drive Elementary", "Dillard Drive Elementary")))









##################################################
### 5. Get unique school names & Client Zip codes
school_names <- school_col_df[!duplicated(school_col_df$school_names), ]


##################################################
#### 6. Match the school_names with the school names in the shape file

# Let's do a crude match of the two datasets to see which school names to change in 
# school_names
# NOTE. some schools have the same names, but are different schools. 


### Examine weird names & change. 
View(school_names[order(school_names$school_names),])
SchoolName[SchoolName %>% str_detect("Bail")]



for (i in c(1:length(school_names))) {
  name_shp <- str_subset(SchoolName, school_names[i])
  print(name_shp)
}



