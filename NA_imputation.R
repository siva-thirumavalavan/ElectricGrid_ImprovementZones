rm(list = ls())
options(warn=-1)


setwd("D:/MSBA/Trimester 3/MIS41170 - Capstone Project/Datasets/RestructuredData")
# install.packages('mice')
# library(mice)
library(dplyr)

###reading the data 
df <- read.csv('RestructuredData.csv',stringsAsFactors = F,na.strings = '')
countyCensus <- read.csv('Ireland_CountyCensus.csv',stringsAsFactors = F)

sum(is.na(df$BERrating))/nrow(df)
(nrow(df)-nrow(df_nonExempt))/nrow(df)
###removing exempt households i.e. households with an approved scheme but no ratings
df_nonExempt <- df[(!(df$approvedClaims>0 & is.na(df$BERrating))&!is.na(df$County26)),c(1:21)]
df_nonExempt$County26[df_nonExempt$County26=='DUBLIN 15'] <- 'DUBLIN'
countyCensus$County26 <- toupper(countyCensus$County26)
df_nonExempt <- merge(df_nonExempt,countyCensus,by = 'County26',all.x = T)
# write.csv(df_nonExempt[!is.na(df_nonExempt$BERrating),],'restructuredData_nonImputed.csv',row.names = F)

for (countyIter in unique(df_nonExempt$County26)) {
  df_filtered <- df_nonExempt[df_nonExempt$County26 == countyIter,]
  BERdistribution <- data.frame(round(table(df_filtered$BERrating)/sum(table(df_filtered$BERrating))*nrow(df_filtered[is.na(df_filtered$BERrating),])))
  fillList <- c()
  BERdistribution$Var1 <- as.character(BERdistribution$Var1)
  for (i in 1:nrow(BERdistribution)){
    fillList <- c(fillList,rep(BERdistribution$Var1[i],BERdistribution$Freq[i]))
  }
  df_nonExempt[df_nonExempt$County26 == countyIter,'BERrating'][is.na(  df_nonExempt[df_nonExempt$County26 == countyIter,'BERrating'])] = sample(fillList)
}

df_nonExempt$BERpublic <-  df_nonExempt$BERrating

for (BERIter in unique(df_nonExempt$BERrating)) {
  df_filtered <- df_nonExempt[df_nonExempt$BERrating == BERIter,]
  HSfueldistribution <- data.frame(round(table(df_filtered$HS_fuel)/sum(table(df_filtered$HS_fuel))*nrow(df_filtered[is.na(df_filtered$HS_fuel),])))
  fillList <- c()
  HSfueldistribution$Var1 <- as.character(HSfueldistribution$Var1)
  for (i in 1:nrow(HSfueldistribution)){
    fillList <- c(fillList,rep(HSfueldistribution$Var1[i],HSfueldistribution$Freq[i]))
  }
  df_nonExempt[df_nonExempt$BERrating == BERIter,'HS_fuel'][is.na(  df_nonExempt[df_nonExempt$BERrating == BERIter,'HS_fuel'])] = sample(fillList)
  
}

sum(is.na(df_nonExempt$HS_fuel))

write.csv(df_nonExempt,'restructedData_imputed.csv',row.names = F)
