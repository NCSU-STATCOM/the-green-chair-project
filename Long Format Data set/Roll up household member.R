library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)

tgcp_demog <- read_excel("C:/Users/CKA/Downloads/STATCOM_data.xlsx", col_types = "text")

tgcp_demog <- rename(tgcp_demog, Id = `...1`)


Longer_Format <- pivot_longer(tgcp_demog,col = 20:117,names_to = "HHMemberInfo",values_to = "HHMemberInfoAnswer",values_drop_na = TRUE)




library(reshape2)
#New <-melt(tgcp_demog,na.rm = TRUE, id.vars = c(names(tgcp_demog)[-20:-117]),measure.vars = c(names(tgcp_demog)[20:117]),names_pattern = "(.)(.)")
#names(longer)[31] <- "HHMInfoAnswer"
#names(longer)[32] <- "HHMInfo"
#another way seems incorrect,,, 
