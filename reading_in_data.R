
library(readxl)
library(tidyverse)



tgcp_demog <- read_excel("STATCOM_data.xlsx", col_types = "text")

tgcp_demog <- rename(tgcp_demog, Id = `...1`)



# viewing the data

View(tail(tgcp_demog[, c(1:40, 119:128)], n = 100))



# Exploration of tgcp_demog data

# Proportion of missing values in each column
apply(tgcp_demog, 2, function(x) mean(is.na(x)))
# Question: at which time(s) do each column generally start having not-NA values? 

# About 22% of clients referred via Wake County Public School System
mean(tgcp_demog$Agency == "WCPSS", na.rm = T) 


