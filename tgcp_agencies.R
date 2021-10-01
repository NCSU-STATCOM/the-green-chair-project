## Common Agencies

# Setting up
library(readxl)
library(writexl)
library(tidyverse)

# Read in raw data
tgcp_demog <- read_excel("STATCOM_data.xlsx", col_types = "text")
tgcp_demog <- rename(tgcp_demog, Id = `...1`)

# Viewing the data
#View(tail(tgcp_demog[, c(1:40, 119:128)], n = 100))

# List of Agencies -- needs to be cleaned
#ggplot(data = tgcp_demog, aes(x = Agency)) + geom_histogram(stat = "count")
agency_table <- as.data.frame(table(tgcp_demog$Agency))
colnames(agency_table) <- c("Agency","Freq")
View(agency_table)

# Write to spreadsheet for data cleaning
# write_xlsx(agency_table, 
          # "C:\\Users\\shakt\\OneDrive\\Documents\\NC State\\TGCP\\the green chair project\\AgencyNames.xlsx")

# Reading in new spreadsheet with duplicate rows for cleaned names
agency_clean <- read_excel(paste0(getwd(), "/tgcp-agencies/AgencyNames.xlsx"), range = cell_cols("B:D"))
View(agency_clean)

# Aggregating duplicate rows to update frequency count
agency_clean2 <- aggregate(Freq~Agency_Clean_Short, 
                           data = agency_clean, FUN=sum)

## Visualizing the agency data
# Not very useful plot (too many variables)
#ggplot(data = agency_clean, aes(x = Agency_Clean_Short)) + geom_bar()

# Top 10 Agencies only
agency_clean2 %>% arrange(desc(Freq)) %>%
  slice(1:10) %>%
  mutate(agency_rank = fct_reorder(Agency_Clean_Short, Freq),
         highlight = 
           ifelse(Agency_Clean_Short %in% 
                              Agency_Clean_Short[which.max(Freq)],T,F)) %>%
  ggplot(., aes(x=factor(agency_rank),y=Freq)) + 
  geom_bar(stat = 'identity', aes(fill=highlight)) + 
  scale_fill_manual(values = c("grey50","#6BBF4E")) +
  coord_flip() +
  theme(axis.text.y = element_text(size = 7, angle = 45), 
        legend.position = "none") +
  labs(title = "Top 10 Agencies: WCPSS with nearly 1200 referrals",
       x = "", y = "")
  #theme(axis.text.x = element_text(size = 3, angle = 45))

