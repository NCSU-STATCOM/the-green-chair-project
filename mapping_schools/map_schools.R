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
#school_col_df <- school_col_df %>% filter(if_any(everything(), ~ !is.na(.)))


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
               "day care", "Daycare", "daycare", "Day care",
               "Kinder", "kinder", "Pre", "pre",
               "New born", "infant",
               "below"
               )
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., str_detect(.,paste(rm_string, collapse = "|")), NA)))

# remove values with no specific school names
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "Elementary School", NA)))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "Elementary", NA)))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "Middle School", NA)))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "High School", NA)))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., . == "High school", NA)))

# change ES -> Elementary School
View(school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(replace(., str_detect(.,"ES"), "Elementary School"))))


##################################################
### 4. Get school names & Client Zip codes

school_col_df <- gather(school_col_df, family_member ,school_names, c(school_col_names[1:7]))
school_col_df <- school_col_df %>% na.omit()
school_col_df <- school_col_df %>% dplyr::select(!"family_member")

##################################################
### 5. Get unique school names & Client Zip codes
school_names <- school_col_df[!duplicated(school_col_df$school_names), ]



##################################################
#### 6. Match the school_names with the school names in the shape file

# Let's do a crude match of the two datasets to see which school names to change in 
# school_names
# NOTE. some schools have the same names, but are different schools. 

SchoolName <- shp_df$SchoolName
for (i in c(1:length(school_names))) {
  name_shp <- str_subset(SchoolName, school_names[i])
  print(name_shp)
}



