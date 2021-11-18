library(rgdal)
library(raster)
library(tidyverse)
library(readxl)
library(viridis)
library(rgeos)
library(sf)

##################################################
#### 1. Read in shape file of all the schools in NC
#shp <- read_csv("NC_schools.csv")
public_shp_df <- st_read(dsn = "D:/NC_schools_shapefile/Public_Schools.shp", stringsAsFactors = F)
non_public_shp_df <- st_read(dsn = "D:/NC_schools_shapefile/Non-Public_Schools.shp", stringsAsFactors = F)
uni_shp_df <- st_read(dsn = "D:/NC_schools_shapefile/Colleges_and_Universities.shp", stringsAsFactors = F)

### 1-2. Rename & remove columns in the three shape files
### Eyeballing the three files, we select common variables in all three files
public_shp_df <- public_shp_df %>% dplyr::select(c("SCHOOL_NAM", "PHYS_ADDR", "PHYS_CITY", "PHYS_ZIP", "COUNTY", "geometry"))
non_public_shp_df <- non_public_shp_df %>% dplyr::select(c("SchoolName", "Address", "City", "Zipcode", "County", "geometry"))
uni_shp_df <- uni_shp_df %>% dplyr::select(c("NAME", "ADDRESS", "CITY", "ZIP", "COUNTY", "geometry"))
colnames(public_shp_df) <- colnames(uni_shp_df) <- c("SchoolName", "Address", "City", "Zipcode", "County", "geometry")

# merge the three shape files into one
shp <- union_all(public_shp_df, non_public_shp_df)

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
               "Kinder", "kinder", "Pre", "pre", "Kid",
               "Head Start", "Headstart", "Head start", "Program",
               "Center",
               "New born", "infant",
               "below", "Online", "Virtual", "online", "virtual",
               "public school", "Public School", "Berkeley", "WCPSS"
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
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Highschool$", "High"))) 

school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "ES$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "EM$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "MS$", "Middle")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "HS$", "High")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Elem.$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Eleementary$", "Elementary")))
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Elementar$", "Elementary")))  
school_col_df <- school_col_df %>% mutate_at(vars(-("ClientZipCode")), funs(str_replace_all(., "Elementry$", "Elementary")))  

##### Wide to long
school_col_long <- gather(school_col_df, family_member ,school_names, c(school_col_names[1:8]))
school_col_long <- school_col_long %>% na.omit()
school_col_long <- school_col_long %>% dplyr::select(!"family_member")

##### Convert First letter to uppercase
school_col_long$school_names <- str_to_title(school_col_long$school_names) 

########################################################
##### Remove unidentifiable school names
unident_names <- c("Curtis School", "Lapetite", "Little Stepping Stones",
                   "Wake County", "Wake")
school_col_long <- school_col_long %>% filter(!school_names %in% unident_names)

########################################################
##### Change specific school names (if they're obviously mistyped)
##### Note. If we don't know if the schools are elementary/middle/high, need to look at
##### original statcom.xlsx to determine individual's age. 
SchoolName <- shp$SchoolName

### B
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

### C
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Carol Middle", "Carroll Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Carrol Middle", "Carroll Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Carroll Magnet Middle", "Carroll Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Cary$", "Cary High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Centennial$", "Centennial Campus Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Centennial Magnet Middle", "Centennial Campus Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Centennial Middle$", "Centennial Campus Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Centennila Middle$", "Centennial Campus Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Centralized Wake High$", "Central Wake High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Charles Bugg$", "Charles B Aycock High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Clayton$", "East Clayton Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Clevland High$", "Cleveland High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Comb Elementary$", "Combs Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Conn$", "Conn Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Conn Magnet Elementary$", "Conn Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Corinth Holder$", "Corinth Holders High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Creech Elementary$", "Creech Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Crosby Garfield$", "Crosby Garfield Center")

### D
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Daniels Magnet Middle$", "Daniels Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Davis Drive$", "Davis Drive Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Dillard Drive Magnet Middle$", "Dillard Drive Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Dillard$", "Dillard Drive Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Dillard Drive$", "Dillard Drive Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Dillard Elementary$", "Dillard Drive Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Dillard Dr Elem School$", "Dillard Drive Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Dillard Dr$", "Dillard Drive Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Durant Rd Elementary$", "Durant Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Durant Rd$", "Durant Road Elementary")

### E
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^East Garner Magnet Middle$", "East Garner Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^East Millbrook$", "East Millbrook Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Enloe$", "William G Enloe High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Enloe High$", "William G Enloe High")

### F
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Forestville$", "Forestville Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Forestville Elementary$", "Forestville Road Elementary")

### G
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Green Road Elementary$", "Green Elementary")

### H
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Heritageelementary$", "Heritage Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Hertiage Creek Elementary$", "Heritage Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Hertiage", "Heritage")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Hodge Elementary$", "Hodge Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Holly Grail Middle$", "Holly Grove Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Hunter$", "Hunter Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Jefferys Grove$", "Jeffreys Grove Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Jeffreys Grove$", "Jeffreys Grove Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Jones Dairy$", "Jones Dairy Elementary")

### K
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Knightdale High School$", "Knightdale High")

### L
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Lake Myra$", "Lake Myra Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Leedsville Elementary$", "Leesville Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Leesville Elementary$", "Leesville Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Leesville$", "Leesville Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Leesville Rd Elementary$", "Leesville Road Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Leesville Middle$", "Leesville Road Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Leesville Rd Middle$", "Leesville Road Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Leesville High$", "Leesville Road High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Leevilles High$", "Leesville Road High")

school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Lochart Elementary$", "Lockhart Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Lockheart Elementary$", "Lockhart Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Longleaf School Of The Arts$", "Longleaf Academy")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Lufkin Middle$", "Lufkin Road Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Lukfinn Middle$", "Lufkin Road Middle")

### M
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Martin$", "Martin Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Mary Phillips$", "Mary Phillips High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Middle Creek$", "Middle Creek High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Milbrook Elementary$", "Millbrook Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Millbrook$", "Millbrook Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Millbook High$", "Millbrook High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Myra Elementary$", "Lake Myra Elementary")

### N
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Needham Broughton High High$", "Needham Broughton High")

### O
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Oake", "Oak")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Oberlin$", "Oberlin Middle")

### P
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Panther Creek$", "Panther Creek High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Partnership$", "Partnership Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Pave$", "Pave Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Powell's", "Powell")

### R
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Ready", "Reedy")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Reedy Creek$", "Reedy Creek Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Reedy Creek Magnet Middle$", "Reedy Creek Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^River Bend$", "River Bend Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Riverbend", "River Bend")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Riverside", "Riverside High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Rogers Lane$", "Rogers Lane Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Roger Lane Elementary$", "Rogers Lane Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Root$", "Root Elementary")

### S 	
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Salem$", "Salem Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Sanderson$", "Sanderson High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Score$", "Score Academy")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Se Raleigh Elementary$", "Southeast Raleigh Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Ser Elementary$", "Southeast Raleigh Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Southeast High$", "Southeast Raleigh High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Stough$", "Stough Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Southeast High School$", "Southeast High")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Southeast High School$", "Southeast High")

### T
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Turner Creek$", "Turner Creek Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Torchlight$", "Torchlight Academy")

### U 
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Underwood$", "Underwood Elementary")

### W
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Willow Springs$", "Willow Springs Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Wildwood Forest$", "Wildwood Forest Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Westview Elementary", "West View Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("West Millbrooke Middle", "West Millbrook Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^West Milbrook$", "West Millbrook Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^West Lake$", "West Lake Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Wendell$", "Wendell Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Wednell Middle$", "Wendell Middle")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Weatherstone$", "Weatherstone Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Weatherspoon Elementary$", "Weatherstone Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Walnut Creek$", "Walnut Creek ES")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Walnut Creek Elementary$", "Walnut Creek ES")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Wakeland", "Wakelon")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Wakelabd", "Wakelon")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Wakelon$", "Wakelon Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Wakefield$", "Wakefield Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Wake Tech Early College$", "Wake Early College of Health and Science")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Wake Early College$", "Wake Early College of Health and Science")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Wake Forest$", "Wake Forest Elementary")

### Z
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("zebulon es", "Zebulon Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("Zebulon Es", "Zebulon Elementary")
school_col_long$school_names <- school_col_long$school_names %>% str_replace_all("^Zebulon$", "Zebulon Middle")

#View(unique(school_col_long[order(school_col_long$school_names),2]))
#SchoolName[SchoolName %>% str_detect("score")]


#################################################
# 4. Merge the shape file with the school_col dataset. 
# Note. There are wrong rows in the shape file (same school name, NOT near WAKE COUNTY).
#       Delete these schools (one school exactly) since they only cause confusion!
##################################################
shp <- shp %>% filter(!Address == "987 Carver Sch Rd")

colnames(school_col_long) <- c("Zipcode","SchoolName")
school_col_long$Zipcode_first <- str_sub(school_col_long$Zipcode, 1,3)
shp$Zipcode <- as.character(shp$Zipcode)
shp$Zipcode_first <- str_sub(shp$Zipcode, 1,3)
school_col_merge <- left_join(school_col_long, shp, by = c("SchoolName", "Zipcode_first"))

##################################################
# 5. Some schools are not included in the shape file. 
# Manually add their coordinates.

sum(school_col_merge$County %>% is.na()) # Note. Total of 296 schools are missing coordinates. 

###################################################
# Let's plot what we have for now. 

merge_shp <- school_col_merge %>% drop_na()
counties_shp <- st_read("D:/NC_schools_shapefile/ZIP_Code_Tabulation_Areas.shp")
plot(counties_shp)
counties_centroids <- counties_shp %>% select(c("GEOID10", "geometry"))
plot(counties_centroids)


ggplot() + 
  geom_sf(data = counties_centroids, aes(geometry = geometry)) 





