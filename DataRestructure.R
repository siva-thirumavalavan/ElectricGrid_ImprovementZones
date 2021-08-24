rm(list = ls())

#set the working directory
setwd("D:/MSBA/Trimester 3/MIS41170 - Capstone Project/Datasets/retrofits")

#load the libraries
library(dplyr)

#read the data
retrofits_raw <- read.csv('All_retrofits_Anonymized_03.03.21.csv',stringsAsFactors = F)
paste0('Has ',round(1- (nrow(unique(retrofits_raw))/nrow(retrofits_raw)),2),'% missing values')
retrofits_raw <- unique(retrofits_raw)
names(retrofits_raw)


countyVariables <- read.csv('Countywise.csv',stringsAsFactors = F)
countyVariables$County26 <- toupper(countyVariables$County26)

#fixing the date column
retrofits_raw$Date <- as.Date(retrofits_raw$Date,format = "%Y-%m-%d")

### for each house ID, we consider the first application date, the final application date, total applications, no of applications approved,rejected and pending, total no of appliances
claimsData <- retrofits_raw %>% 
              group_by(UUID,Scheme,Measures) %>% 
              summarise('min_date'=min(Date),
                'max_date'=max(Date),
                'total_submissions'=length(UUID),
                'rejectedClaims'=sum(Status=='Cancelled',na.rm = T),
                'approvedClaims'=sum(Status=='Completed',na.rm = T),
                'pendingClaims'=sum(Status=='Ongoing',na.rm = T),
                'measureCount'=length(unlist(strsplit(Measures,","))))

claimsData_rolled <- claimsData %>% 
                     group_by(UUID) %>%
                     summarise('schemeCount' = length(unique(Scheme)),
                               'totalMeasures' = sum(measureCount),
                               'schemeConcat' = paste0(unique(Scheme),collapse = ","),
                               'min_date'=min(min_date),
                               'max_date'=max(max_date),
                               'total_submissions'=sum(total_submissions),
                               'rejectedClaims'=sum(rejectedClaims),
                               'approvedClaims'=sum(approvedClaims),
                               'pendingClaims'=sum(pendingClaims),
                               'houseVintage'=length(seq(from=as.Date(min_date),to=as.Date(max_date),by='month'))                               )
                 

### for each house ID, we assign the geographical codes with least probabilistic error
rawGeographyData <- unique(retrofits_raw[,c(1:5,40,41)])
geographyData <- rawGeographyData %>% group_by(UUID) %>%
  summarise('cso_small_area'=ifelse(n()>1,cso_small_area[which.min(prob_smarea_error_0corr)],cso_small_area),
            'geo_small_area'=ifelse(n()>1,geo_small_area[which.min(prob_smarea_error_0corr)],geo_small_area),
            'County26'=ifelse(n()>1,County26[which.min(prob_smarea_error_0corr)],County26),
            'County52'=ifelse(n()>1,County52[which.min(prob_smarea_error_0corr)],County52))


#we consider the latest rating of the house for an approved scheme
ratingsData <- unique(retrofits_raw[retrofits_raw$BERrating!='',c(1,9,34:39)])
ratingsData_rolled <- ratingsData %>% group_by(UUID) %>%
                            summarise('BERrating'=BERrating[which(Date==max(Date))],
                                      'BERpublic'=BERrating[which(Date==max(Date))],
                                      'CentralHeating'=CentralHeating[which(Date==max(Date))],
                                      'HS_fuel'=HS_fuel[which(Date==max(Date))],
                                      'HS_eff'=HS_eff[which(Date==max(Date))],
                                      'HLC'=HLC[which(Date==max(Date))]
                            )

### merging the three sets
restructuredData_1 <- merge(claimsData_rolled,geographyData,by='UUID',all.x=T)
restructuredData_2 <- merge(restructuredData_1,ratingsData_rolled,by='UUID',all.x=T)
restructuredData_3 <- merge(restructuredData_2,countyVariables,by='County26',all.x=T)

write.csv(restructuredData_3,'restructuredData.csv',row.names = F,na = "")
















