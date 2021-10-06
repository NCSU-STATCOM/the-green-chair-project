library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)

tgcp_demog <- read_excel("C:/Users/CKA/Downloads/STATCOM_data.xlsx", col_types = "text")

tgcp_demog <- rename(tgcp_demog, Id = `...1`)


table(tgcp_demog$TotalHHNumber,

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
#looking for a way to merge two cols within a dataset and remove duplicates while maintaining the both null values. 

View(H)

table(H$Race,H$ClientAge)
P <-H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: '))

P <- H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: 2.0'))
Y <- H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: 3.0'))
B <-H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: 4.0'))
K <- H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: 5.0'))
U <- H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: 6.0'))
Q <-H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: 7.0'))
I <- H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: 8.0'))
L <- H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: 9.0'))
R <-H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: 10.0'))
Z <- H %>% group_by(ClientName) %>% filter(HHMemberInfo == c('TotalHHNumber: 1.0'))

A<- rbind(P,Y)
B <- rbind(B,K)
C <- rbind(U,Q)
D <- rbind(I,L)
E <- rbind(R,Z)
F <- rbind(A,B)
G <- rbind(C,D)
J <- rbind(F,G)
S <- rbind(J,E)

g <- ggplot(S, aes(x = HHMemberInfo,y = Beds))
g + geom_point(aes(color = as.factor(Beds), shape = as.factor(HHMemberInfo)),
               size = 2 )+  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

plot1 <- ggplot(S, aes(x = HHMemberInfo, fill = as.factor(Beds))) + geom_bar(position = "stack")+
  scale_fill_discrete(name = "Beds") +
  labs (title = "Count:House Hold Total Numbers, Stack: # of Beds(Remove NA)")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



plot2 <- ggplot(subset(S,!is.na(Beds)), aes(x = HHMemberInfo, fill = as.factor(Beds))) + 
  geom_bar(position = "stack")+
  scale_fill_discrete(name = "Beds") +
  labs (title = "Count:House Hold Total Numbers, Stack: # of Beds(Remove NA)")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))












#some outside sources. 
library(readr)
F <- read_excel("C:/Users/CKA/Desktop/Green Chair Project/HOME_IncomeLmts_Natl_2021.xlsx")
Z <- F %>% filter(areaname == "Raleigh, NC MSA") %>% select(num_range(c("Lim30_21p"),1:8))
E<-Z[1,]
length(E)
z = 0
for(i in 1:length(E)){
  z = z + E[[i]]
  print(z)
}
D <- F %>% filter(areaname == "Raleigh, NC MSA") %>% select(num_range(c("Lim60_21p"),1:8))
R<-D[1,]
























