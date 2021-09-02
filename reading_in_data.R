
library(readxl)
library(tidyverse)



tgcp_demog <- read_excel("STATCOM_data.xlsx", col_types = "text")

tgcp_demog <- rename(tgcp_demog, Id = `...1`)



View(tail(tgcp_demog[, c(1:40, 119:128)], n = 100))


