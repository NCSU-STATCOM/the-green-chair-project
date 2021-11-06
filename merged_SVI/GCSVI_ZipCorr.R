##SVI Basic Correlation by Number of Clients in ZipCode

library(dplyr)
library(corrplot)

##Loading in data
data_dir<-"" ##Set local directory

GC_SVI_file<-"merged_CDC_GC_clean.csv"

GC_SVI<-read.csv(paste0(data_dir,GC_SVI_file))


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


corrs_n<-cor(mean_SVI$n,to_corr)
corrs_med<-cor(mean_SVI["n Median"],to_corr)
corrs_lowinc<-cor(mean_SVI["n Low Income"],to_corr)
corrs_verylowinc<-cor(mean_SVI["n Very Low Income"],to_corr)

##Taking out those SVI with absolute correlation less than 0.2
of_interest_n<-colnames(to_corr)[abs(corrs_n)>=0.2]
of_interest_med<-colnames(to_corr)[abs(corrs_med)>=0.2]
of_interest_lowinc<-colnames(to_corr)[abs(corrs_lowinc)>=0.2]
of_interest_verylowinc<-colnames(to_corr)[abs(corrs_verylowinc)>=0.2]

##Correlation Matrix Plots
corrplot(cor(as.matrix(cbind(mean_SVI$n,to_corr[of_interest_n]))), 
         title="SVI Correlation with Number of People in ZipCode")
corrplot(cor(as.matrix(cbind(mean_SVI["n Median"],to_corr[of_interest_med]))), 
         title="SVI Correlation with Number of People in ZipCode Below Median Income")
corrplot(cor(as.matrix(cbind(mean_SVI["n Low Income"],to_corr[of_interest_lowinc])),
         title="SVI Correlation with Number of People in ZipCode Below Low Income"))
corrplot(cor(as.matrix(cbind(mean_SVI["n Very Low Income"],to_corr[of_interest_verylowinc])),
         title="SVI Correlation with Number of People in ZipCode Below Very Low Income"))