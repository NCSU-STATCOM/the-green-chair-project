##SVI Basic Correlation by Number of Clients in ZipCode

library(dplyr)
library(corrplot)
library(GGally)

##Loading in data
GC_SVI <- readRDS("cleaned_STATCOM_data_SVI.rds")


##Aggregate SV indices by ClientZipCode...Using Mean (Can Change Function)
mean_SVI<-aggregate(GC_SVI[104:ncol(GC_SVI)], by=GC_SVI["ClientZipCode"],mean)

##Gather counts of Those in Each ZipCode
counts<-GC_SVI%>%count(ClientZipCode)
##Counting Number of Median, Low and Very Income in Each ZipCode
counts_income<-GC_SVI%>%group_by(ClientZipCode)%>%count(Homeincome)
counts_income<-counts_income[!(counts_income$Homeincome=="Unknown"|is.na(counts_income$ClientZipCode)),]
for (col in unique(counts_income$Homeincome)){
  inc_zips<-counts_income$ClientZipCode[counts_income$Homeincome==col]
  inc_counts<-counts_income$n[counts_income$Homeincome==col]
  col_frame<-data.frame(ClientZipCode=inc_zips, n=inc_counts)
  total_zips<-counts$ClientZipCode[!is.na(counts$ClientZipCode)]
  for (zip in total_zips){
    if (!(zip%in%inc_zips)){
      col_frame<-rbind(col_frame, data.frame(ClientZipCode=zip, n=0))
    }
  }
  col_frame<-col_frame[order(match(col_frame$ClientZipCode,total_zips)),]
  mean_SVI[paste("n",col)]<-col_frame$n
}

mean_SVI["n"]<-counts[1:nrow(counts)-1,2]

to_corr<-mean_SVI[,!(colnames(mean_SVI)%in%(c("ClientZipCode"))|startsWith(colnames(mean_SVI),"n"))]


corrs_n<-cor(mean_SVI$n[complete.cases(to_corr)],to_corr[complete.cases(to_corr),])
corrs_med<-cor(mean_SVI[complete.cases(to_corr), "n Median"],to_corr[complete.cases(to_corr),])
corrs_lowinc<-cor(mean_SVI[complete.cases(to_corr), "n Low Income"],to_corr[complete.cases(to_corr), ])
corrs_verylowinc<-cor(mean_SVI[complete.cases(to_corr), "n Very Low Income"],to_corr[complete.cases(to_corr), ])

##Taking out those SVI with absolute correlation less than 0.2
of_interest_n<-colnames(to_corr)[abs(corrs_n)>=0.2]
of_interest_med<-colnames(to_corr)[abs(corrs_med)>=0.2]
of_interest_lowinc<-colnames(to_corr)[abs(corrs_lowinc)>=0.2]
of_interest_verylowinc<-colnames(to_corr)[abs(corrs_verylowinc)>=0.2]

# can shorten names later for better plotting
##Correlation Matrix Plots
corrplot(cor(as.matrix(cbind(mean_SVI$n[complete.cases(to_corr)],to_corr[complete.cases(to_corr), of_interest_n]))), 
         title="SVI Correlation with Number of People in ZipCode")
corrplot(cor(as.matrix(cbind(mean_SVI[complete.cases(to_corr), "n Median"],to_corr[complete.cases(to_corr), of_interest_med]))), 
         title="SVI Correlation with Number of People in ZipCode Below Median Income")
corrplot(cor(as.matrix(cbind(mean_SVI[complete.cases(to_corr), "n Low Income"],to_corr[complete.cases(to_corr), of_interest_lowinc]))),
         title="SVI Correlation with Number of People in ZipCode Below Low Income")
corrplot(cor(as.matrix(cbind(mean_SVI[complete.cases(to_corr), "n Very Low Income"],to_corr[complete.cases(to_corr), of_interest_verylowinc]))),
         title="SVI Correlation with Number of People in ZipCode Below Very Low Income")

# taking a look at the numbers
cor(as.matrix(cbind(mean_SVI$n[complete.cases(to_corr)],to_corr[complete.cases(to_corr), of_interest_n])))[1,]
cor(as.matrix(cbind(mean_SVI$`n Median`[complete.cases(to_corr)],to_corr[complete.cases(to_corr), of_interest_med])))[1,]
cor(as.matrix(cbind(mean_SVI$`n Low Income`[complete.cases(to_corr)],to_corr[complete.cases(to_corr), of_interest_lowinc])))[1,]
cor(as.matrix(cbind(mean_SVI$`n Very Low Income`[complete.cases(to_corr)],to_corr[complete.cases(to_corr), of_interest_verylowinc])))[1,]

# using GGally package to see significances 

ggpairs(data.frame(cbind(mean_SVI$n[complete.cases(to_corr)], to_corr[complete.cases(to_corr),c("EP_AGE65", "EP_MINRTY", "EP_MUNIT", "EP_MOBILE", "EP_CROWD")])), title = "Correlogram of the Metrics")

# # PCA of the variables of interest
# 
# res_pca <- prcomp(to_corr[c("EP_AGE65", "EP_MINRTY", "EP_MUNIT", "EP_MOBILE", "EP_CROWD")])
# 
# first_pc <- res_pca$x[, 1]
# 
# # including the first PC summary into the correllogram
# 
# ggpairs(data.frame(cbind(mean_SVI$n, to_corr[c("EP_AGE65", "EP_MINRTY", "EP_MUNIT", "EP_MOBILE", "EP_CROWD")], first_pc)), title = "Correlogram of the Metrics")
# 
# # The first PC doesn't correlate as well.
