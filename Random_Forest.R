
install.packages("magrittr")
library(magrittr)
library(dplyr)
library(randomForest)
library(ggthemes)

#See all types of purpose of visit
unique(combinedDataMod$POV_CODE_C)
combinedDataModT <- combinedDataMod[(combinedDataMod$POV_CODE_C=="BUSINESS") | (combinedDataMod$POV_CODE_C=="LEISURE"),]

#Visualize data for both Business and Leisure Data
BusiLeisure <- ggplot(combinedDataModT, aes(x=as.factor(combinedDataModT$POV_CODE_C))) + geom_bar(aes( fill=factor(combinedDataModT$NPS_Type)))
BusiLeisure


BusinessSubset <- subset(combinedDataModT, combinedDataModT$POV_CODE_C == "BUSINESS")
LeisureSubset <- subset(combinedDataModT, combinedDataModT$POV_CODE_C == "LEISURE")


#Business Subset

groupInddataSet <- BusinessSubset

groupCleaned <- checkAndRemoveEmptyandNAvalues(groupInddataSet, groupInddataSet$`Dry-Cleaning_PL`)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$`All Suites_PL`)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Casino_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Elevators_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Laundry_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$LENGTH_OF_STAY_CATEGORY_R)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$`Brand Initial_PL`)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Type_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Relationship_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$OSatisfactionType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$guesRoomSatisfactionType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$tranquilityType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$customerSatisfactionType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$staffCareType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$InteretSatType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$`Fitness Center_PL`)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$`Business Center_PL`)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Golf_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$HotelConditionType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$FoodBevExpType)



groupCleaned$`Dry-Cleaning_PL` <- as.factor(groupCleaned$`Dry-Cleaning_PL`)
groupCleaned$`All Suites_PL` <- as.factor(groupCleaned$`All Suites_PL`)
groupCleaned$Casino_PL <- as.factor(groupCleaned$Casino_PL)
groupCleaned$Elevators_PL <- as.factor(groupCleaned$Elevators_PL)
groupCleaned$Laundry_PL <- as.factor(groupCleaned$Laundry_PL)
groupCleaned$LENGTH_OF_STAY_CATEGORY_R <- as.factor(groupCleaned$LENGTH_OF_STAY_CATEGORY_R)
groupCleaned$`Brand Initial_PL` <- as.factor(groupCleaned$`Brand Initial_PL`)
groupCleaned$Type_PL <- as.factor(groupCleaned$Type_PL)
groupCleaned$Relationship_PL <- as.factor(groupCleaned$Relationship_PL)
groupCleaned$OSatisfactionType <- as.factor(groupCleaned$OSatisfactionType)
groupCleaned$guesRoomSatisfactionType <- as.factor(groupCleaned$guesRoomSatisfactionType)
groupCleaned$tranquilityType <- as.factor(groupCleaned$tranquilityType)
groupCleaned$customerSatisfactionType <- as.factor(groupCleaned$customerSatisfactionType)
groupCleaned$staffCareType <- as.factor(groupCleaned$staffCareType)
groupCleaned$InteretSatType <- as.factor(groupCleaned$InteretSatType)
groupCleaned$`Fitness Center_PL` <- as.factor(groupCleaned$`Fitness Center_PL`)
groupCleaned$Golf_PL <- as.factor(groupCleaned$Golf_PL)
groupCleaned$`Business Center_PL` <- as.factor(groupCleaned$`Business Center_PL`)
groupCleaned$HotelConditionType <- as.factor(groupCleaned$HotelConditionType)
groupCleaned$FoodBevExpType <- as.factor(groupCleaned$FoodBevExpType)

data2Use <- groupCleaned
data2Use <- data2Use[(data2Use$NPS_Type == "Detractor" | data2Use$NPS_Type == "Promoter"),]
data2Use$NPS_Type <- as.factor(data2Use$NPS_Type)

data2Use$DryCl <- data2Use$`Dry-Cleaning_PL`
data2Use$ALL_Suites <- data2Use$`All Suites_PL`
data2Use$BRand <- data2Use$`Brand Initial_PL`
data2Use$Fitness <- data2Use$`Fitness Center_PL`

#Training and Testing Data
randomizedIndex <- (sample(1:dim(data2Use)[1]))
head(randomizedIndex)

indexFortowthird <- floor(2*(dim(data2Use)[1])/3)

trainingData <- data2Use[1:indexFortowthird,]
testingData <- data2Use[(indexFortowthird+1):nrow(data2Use),]

#Model for random forest
rf_Model <- randomForest(factor(NPS_Type) ~ staffCareType +customerSatisfactionType + guesRoomSatisfactionType+
                           tranquilityType  + HotelConditionType +FoodBevExpType ,data=trainingData)
rf_Model
plot(rf_Model, ylim=c(0,0.4))
legend('topright', colnames(rf_Model$err.rate), col=1:3, fill=1:3, cex =  .5)

importance    <- importance(rf_Model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + coord_flip() + theme_few()


rfPredict <- predict(rf_Model, testingData)

length(rfPredict)
nrow(testingData)

testingData$rfpred <- rfPredict

#Function to check accuracy of random forest
functionForRForestAccuracy <- function(dataSet){
  a <- ifelse(dataSet$testingData.NPS_Type == dataSet$testingData.rfpred, 1,0)
  return(length(which((a==1) == TRUE))/nrow(dataSet))
}

comparisonTable <- data.frame(testingData$NPS_Type,testingData$rfpred)
#View(comparisonTable)
table(comparisonTable)
functionForRForestAccuracy(comparisonTable)


#Leisure Subset

groupInddataSet <- LeisureSubset

groupCleaned <- checkAndRemoveEmptyandNAvalues(groupInddataSet, groupInddataSet$`Dry-Cleaning_PL`)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$`All Suites_PL`)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Casino_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Elevators_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Laundry_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$LENGTH_OF_STAY_CATEGORY_R)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$`Brand Initial_PL`)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Type_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Relationship_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$OSatisfactionType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$guesRoomSatisfactionType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$tranquilityType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$customerSatisfactionType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$staffCareType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$InteretSatType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$`Fitness Center_PL`)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$`Business Center_PL`)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$Golf_PL)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$HotelConditionType)
groupCleaned <- checkAndRemoveEmptyandNAvalues(groupCleaned, groupCleaned$FoodBevExpType)



groupCleaned$`Dry-Cleaning_PL` <- as.factor(groupCleaned$`Dry-Cleaning_PL`)
groupCleaned$`All Suites_PL` <- as.factor(groupCleaned$`All Suites_PL`)
groupCleaned$Casino_PL <- as.factor(groupCleaned$Casino_PL)
groupCleaned$Elevators_PL <- as.factor(groupCleaned$Elevators_PL)
groupCleaned$Laundry_PL <- as.factor(groupCleaned$Laundry_PL)
groupCleaned$LENGTH_OF_STAY_CATEGORY_R <- as.factor(groupCleaned$LENGTH_OF_STAY_CATEGORY_R)
groupCleaned$`Brand Initial_PL` <- as.factor(groupCleaned$`Brand Initial_PL`)
groupCleaned$Type_PL <- as.factor(groupCleaned$Type_PL)
groupCleaned$Relationship_PL <- as.factor(groupCleaned$Relationship_PL)
groupCleaned$OSatisfactionType <- as.factor(groupCleaned$OSatisfactionType)
groupCleaned$guesRoomSatisfactionType <- as.factor(groupCleaned$guesRoomSatisfactionType)
groupCleaned$tranquilityType <- as.factor(groupCleaned$tranquilityType)
groupCleaned$customerSatisfactionType <- as.factor(groupCleaned$customerSatisfactionType)
groupCleaned$staffCareType <- as.factor(groupCleaned$staffCareType)
groupCleaned$InteretSatType <- as.factor(groupCleaned$InteretSatType)
groupCleaned$`Fitness Center_PL` <- as.factor(groupCleaned$`Fitness Center_PL`)
groupCleaned$Golf_PL <- as.factor(groupCleaned$Golf_PL)
groupCleaned$`Business Center_PL` <- as.factor(groupCleaned$`Business Center_PL`)
groupCleaned$HotelConditionType <- as.factor(groupCleaned$HotelConditionType)
groupCleaned$FoodBevExpType <- as.factor(groupCleaned$FoodBevExpType)

data2Use <- groupCleaned
data2Use <- data2Use[(data2Use$NPS_Type == "Detractor" | data2Use$NPS_Type == "Promoter"),]
data2Use$NPS_Type <- as.factor(data2Use$NPS_Type)

data2Use$DryCl <- data2Use$`Dry-Cleaning_PL`
data2Use$ALL_Suites <- data2Use$`All Suites_PL`
data2Use$BRand <- data2Use$`Brand Initial_PL`
data2Use$Fitness <- data2Use$`Fitness Center_PL`

randomizedIndex <- (sample(1:dim(data2Use)[1]))
head(randomizedIndex)

indexFortowthird <- floor(2*(dim(data2Use)[1])/3)

trainingData <- data2Use[1:indexFortowthird,]
testingData <- data2Use[(indexFortowthird+1):nrow(data2Use),]


rf_Model <- randomForest(factor(NPS_Type) ~ staffCareType +customerSatisfactionType + guesRoomSatisfactionType+
                           tranquilityType  + HotelConditionType +FoodBevExpType ,data=trainingData)
rf_Model
plot(rf_Model, ylim=c(0,0.4))
legend('topright', colnames(rf_Model$err.rate), col=1:3, fill=1:3, cex =  .5)

importance    <- importance(rf_Model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + coord_flip() + theme_few()


rfPredict <- predict(rf_Model, testingData)

length(rfPredict)
nrow(testingData)

testingData$rfpred <- rfPredict

functionForRForestAccuracy <- function(dataSet){
  a <- ifelse(dataSet$testingData.NPS_Type == dataSet$testingData.rfpred, 1,0)
  return(length(which((a==1) == TRUE))/nrow(dataSet))
}

comparisonTable <- data.frame(testingData$NPS_Type,testingData$rfpred)
#View(comparisonTable)
table(comparisonTable)
functionForRForestAccuracy(comparisonTable)
