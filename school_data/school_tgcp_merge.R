library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
#read excel document 
statcom_data <- read_excel("STATCOM_data.xlsx", col_types = "text")

source("school_data/clean_school_names.R")

school_col_names <- tidyselect::vars_select(colnames(statcom_data), contains('School', ignore.case = TRUE)) 

for(col_name in school_col_names){
  statcom_data[[col_name]] = clean_school_names(statcom_data %>% select(col_name, ClientZipCode))$school_names
}

school_cols = unlist(statcom_data %>% select(school_col_names))

count = table(school_cols, useNA="no")

school_count_df <- data.frame(school_name = names(count),
                     tgcp_count = as.integer(count))

lunch_data = read.csv("school_data/data/lunches_2020_2021.csv")

lunch_data = lunch_data %>% select(SCHOOL.NAME, FREE.and.REDUCED, FR.PERCENTAGE)

school_lunch = merge(school_count_df, lunch_data, by.x = "school_name", by.y = "SCHOOL.NAME")
school_lunch$FREE.and.REDUCED = as.numeric(school_lunch$FREE.and.REDUCED)
school_lunch$FR.PERCENTAGE = as.numeric(sub("%", "", school_lunch$FR.PERCENTAGE))


ggplot(school_lunch, aes(x=tgcp_count, y=FREE.and.REDUCED)) + 
  geom_point() + 
  geom_text(
    label=school_lunch$school_name,
    size = 3,
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = TRUE
  ) 


ggplot(school_lunch, aes(x=tgcp_count, y=FR.PERCENTAGE)) + 
  geom_point() + 
  geom_text(
    label=school_lunch$school_name,
    size = 3,
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = TRUE
  ) 


# separate by elementary, middle, and high school
