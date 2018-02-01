library(data.table)


#Specify columns to read from the data after reading the metadata
col2Read <- c(10,11,12,19,21,22,23,60,64,108,109,137:144,147,168,171,175,176,183,185,194,198,199,200,202,203,204,206,207,208,210,212,214,215,216,219,223,227,232)

#Select month from each quarter of year
month1 <- fread("E:/e687/out-201402.csv", nrows = 1170062, stringsAsFactors = F)[,..col2Read]
month2 <- fread("E:/e687/out-201405.csv", nrows = 1373582, stringsAsFactors = F)[,..col2Read]
month3 <- fread("E:/e687/out-201408.csv", nrows = 1387446, stringsAsFactors = F)[,..col2Read]
month4 <- fread("E:/e687/out-201411.csv", nrows = 1303803, stringsAsFactors = F)[,..col2Read]

month1Mod <- month1  
month2Mod <- month2  
month3Mod <- month3
month4Mod <- month4  

#Function to remove values where NPS_Type is NA/Blank or Likelihood_Recommend_H is NA/Blank
removeNAandEmpty <- function(monthData){
  monthData <- monthData[!(is.na(monthData$Likelihood_Recommend_H) | (monthData$Likelihood_Recommend_H == "")),]
  monthData <- monthData[!(is.na(monthData$NPS_Type) | (monthData$NPS_Type == "")),]  
  return(monthData)
}

#Pass all the files through the function to remove initial values 
month1Mod <- removeNAandEmpty(month1Mod)
month2Mod <- removeNAandEmpty(month2Mod)
month3Mod <- removeNAandEmpty(month3Mod)
month4Mod <- removeNAandEmpty(month4Mod)


#Combine the clean data of four months into one file
combinedData <- rbind(month1Mod, month2Mod, month3Mod,month4Mod)

#Save this data to a CSV file for future use
write.csv(combinedData, "E:/e687/combinedDataSetNew.csv")


#Create a new varibale to save original un-modified data
combinedDataMod <- combinedData

#Working only on data from United States
combinedDataMod <- combinedDataMod[combinedDataMod$Country_PL=="United States",]

#Convert numerical variables to categorical variables, adding a new column

combinedDataMod$OSatisfactionType <- ""
combinedDataMod[which(combinedDataMod$Overall_Sat_H<=6), 46]<-"LOW"
combinedDataMod[which(combinedDataMod$Overall_Sat_H>=9), 46]<-"HIGH"
combinedDataMod[which(combinedDataMod$Overall_Sat_H==7), 46]<-"MED"
combinedDataMod[which(combinedDataMod$Overall_Sat_H==8), 46]<-"MED"


combinedDataMod$guesRoomSatisfactionType <- ""
combinedDataMod[which(combinedDataMod$Guest_Room_H<=6), 47]<-"LOW"
combinedDataMod[which(combinedDataMod$Guest_Room_H>=9), 47]<-"HIGH"
combinedDataMod[which(combinedDataMod$Guest_Room_H==7), 47]<-"MED"
combinedDataMod[which(combinedDataMod$Guest_Room_H==8), 47]<-"MED"

combinedDataMod$tranquilityType <- ""
combinedDataMod[which(combinedDataMod$Tranquility_H<=6), 48]<-"Low"
combinedDataMod[which(combinedDataMod$Tranquility_H>=9), 48]<-"High"
combinedDataMod[which(combinedDataMod$Tranquility_H==7), 48]<-"Med"
combinedDataMod[which(combinedDataMod$Tranquility_H==8), 48]<-"Med"



combinedDataMod$customerSatisfactionType <- ""
combinedDataMod[which(combinedDataMod$Customer_SVC_H<=6), 49]<-"Low"
combinedDataMod[which(combinedDataMod$Customer_SVC_H>=9), 49]<-"High"
combinedDataMod[which(combinedDataMod$Customer_SVC_H==7), 49]<-"Med"
combinedDataMod[which(combinedDataMod$Customer_SVC_H==8), 49]<-"Med"


combinedDataMod$staffCareType <- ""
combinedDataMod[which(combinedDataMod$Staff_Cared_H<=6), 50]<-"Low"
combinedDataMod[which(combinedDataMod$Staff_Cared_H>=9), 50]<-"High"
combinedDataMod[which(combinedDataMod$Staff_Cared_H==7), 50]<-"Med"
combinedDataMod[which(combinedDataMod$Staff_Cared_H==8), 50]<-"Med"


combinedDataMod$InteretSatType <- ""
combinedDataMod[which(combinedDataMod$Internet_Sat_H<=6), 51]<-"Low"
combinedDataMod[which(combinedDataMod$Internet_Sat_H>=9), 51]<-"High"
combinedDataMod[which(combinedDataMod$Internet_Sat_H==7), 51]<-"Med"
combinedDataMod[which(combinedDataMod$Internet_Sat_H==8), 51]<-"Med"

combinedDataMod$HotelConditionType <- ""
combinedDataMod[which(combinedDataMod$Condition_Hotel_H<=6), 52]<-"Low"
combinedDataMod[which(combinedDataMod$Condition_Hotel_H>=9), 52]<-"High"
combinedDataMod[which(combinedDataMod$Condition_Hotel_H==7), 52]<-"Med"
combinedDataMod[which(combinedDataMod$Condition_Hotel_H==8), 52]<-"Med"


combinedDataMod$FoodBevExpType <- ""
combinedDataMod[which(combinedDataMod$`F&B_Overall_Experience_H`<=6), 53]<-"Low"
combinedDataMod[which(combinedDataMod$`F&B_Overall_Experience_H`>=9), 53]<-"High"
combinedDataMod[which(combinedDataMod$`F&B_Overall_Experience_H`==7), 53]<-"Med"
combinedDataMod[which(combinedDataMod$`F&B_Overall_Experience_H`==8), 53]<-"Med"


#Create function to remove column(taken as a paramter) with empty or NA values from a dataset (taken as a parameter )
checkAndRemoveEmptyandNAvalues <- function(data2Check, column2Check){
  data2Check <- data2Check[!(is.na(column2Check) | (column2Check == "")),]
  return(data2Check)
}

