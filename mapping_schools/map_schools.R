library(rgdal)
library(tidyverse)
library(readxl)

#### 1. Read in shape file of all the public schools in NC
shp <- readOGR(dsn = "Public_Schools.shp", stringsAsFactors = F)
SCHOOL_NAM <- shp$SCHOOL_NAM
SCHOOL_NAM <- as.data.frame(SCHOOL_NAM)
SCHOOL_NAM %>% contains("Child")
str_detect(SCHOOL_NAM, "Child")

### 2. Read in STATCOM_data.xlsx
statcom_data <- read_excel("STATCOM_data.xlsx", col_types = "text")

# Select columns involving which schools the individuals go to: 9 columns in total
school_col_names <- tidyselect::vars_select(colnames(statcom_data), contains('School', ignore.case = TRUE)) 
school_col_df <- statcom_data %>% select(school_col_names)

### 3. Tidy up school_col_df
# remove rows containing only NA's
school_col_df <- school_col_df %>% filter(if_any(everything(), ~ !is.na(.)))

# remove columns containing only NA's: HHMember9School
school_col_df <- school_col_df %>% select_if(~!all(is.na(.))) 

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
school_col_df <- mutate_all(school_col_df, funs(replace(., str_detect(.,paste(rm_string, collapse = "|")), NA)))
school_col_df <- mutate_all(school_col_df, funs(replace(., . == "Elementary School", NA)))
school_col_df <- mutate_all(school_col_df, funs(replace(., . == "Elementary", NA)))
school_col_df <- mutate_all(school_col_df, funs(replace(., . == "Middle School", NA)))
school_col_df <- mutate_all(school_col_df, funs(replace(., . == "High School", NA)))
school_col_df <- mutate_all(school_col_df, funs(replace(., . == "High school", NA)))


school_names <- unique(as.vector(as.matrix(school_col_df)))
school_names <- as.data.frame(school_names)
