#KSVM


groupVsIndBar <- ggplot(combinedDataMod, aes(x=as.factor(combinedDataMod$GROUPS_VS_FIT_R))) + geom_bar(aes( fill=factor(combinedDataMod$NPS_Type))) + ggtitle("Distribution of NPS Type with Type of Traveller") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Type of Traveller") + ylab("Count of Traveller") + labs(fill="NPS Type")
groupVsIndBar

groupSubset <- subset(combinedDataMod, combinedDataMod$GROUPS_VS_FIT_R == "Groups")
indSubset <- subset(combinedDataMod, combinedDataMod$GROUPS_VS_FIT_R == "FIT")

groupInddataSet <- groupSubset

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

data2Use <- groupCleaned
data2Use <- data2Use[(data2Use$NPS_Type == "Detractor" | data2Use$NPS_Type == "Promoter"),]
data2Use$NPS_Type <- as.factor(data2Use$NPS_Type)


randomizedIndex <- (sample(1:dim(data2Use)[1]))
head(randomizedIndex)

indexFortowthird <- floor(2*(dim(data2Use)[1])/3)

trainingData <- data2Use[1:indexFortowthird,]
testingData <- data2Use[(indexFortowthird+1):nrow(data2Use),]

library(kernlab)
ksvm_Model<-ksvm(NPS_Type~`Dry-Cleaning_PL`  + `All Suites_PL` + InteretSatType + tranquilityType + staffCareType +  Elevators_PL + Laundry_PL + `Brand Initial_PL` +  Type_PL   + Relationship_PL  + `Fitness Center_PL`,data=trainingData, kernel = "rbfdot", kpar = "automatic", C = 50, cross = 3, prob.model = TRUE)

ksvm_Model

ksvmPredict <- predict(ksvm_Model, testingData)
length(ksvmPredict)
nrow(testingData)

testingData$predictedValues <- ksvmPredict


functionForKSVMAccuracy <- function(dataSet){
  a <- ifelse(dataSet$testingData.NPS_Type == dataSet$testingData.predictedValues, 1,0)
  return(length(which((a==1) == TRUE))/nrow(dataSet) * 100)
}

comparisonTable <- data.frame(testingData$NPS_Type,testingData$predictedValues)
table(comparisonTable)
functionForKSVMAccuracy(comparisonTable)
colnames(comparisonTable) <- c("Original", "Predicted")
originalPlot <- ggplot(comparisonTable, aes(x=comparisonTable$Original)) + geom_bar(aes( fill=factor(comparisonTable$Original)))
originalPlot
predictedPlot <- ggplot(comparisonTable, aes(x=comparisonTable$Predicted)) + geom_bar(aes( fill=factor(comparisonTable$Predicted)))
predictedPlot
library(gridExtra)
grid.arrange(originalPlot, predictedPlot, ncol=2)

library(naivebayes)
nb_Model <- naiveBayes(NPS_Type~staffCareType + tranquilityType + `Dry-Cleaning_PL`  + `All Suites_PL` + Elevators_PL + Laundry_PL + `Brand Initial_PL` +  Type_PL   + Relationship_PL  + InteretSatType + `Fitness Center_PL`, data=trainingData)
nbPred <- predict(nb_Model, testingData)

length(nbPred)
nrow(testingData)


testingData$predictedValuesNB <- nbPred
comparisonTable <- data.frame(testingData$NPS_Type,testingData$predictedValuesNB)
table(comparisonTable)
functionForNBAccuracy <- function(dataSet){
  a <- ifelse(dataSet$testingData.NPS_Type == dataSet$testingData.predictedValuesNB, 1,0)
  return(length(which((a==1) == TRUE))/nrow(dataSet) * 100)
}
functionForNBAccuracy(comparisonTable)
colnames(comparisonTable) <- c("Original", "Predicted")
originalPlot <- ggplot(comparisonTable, aes(x=comparisonTable$Original)) + geom_bar(aes( fill=factor(comparisonTable$Original)))
originalPlot
predictedPlot <- ggplot(comparisonTable, aes(x=comparisonTable$Predicted)) + geom_bar(aes( fill=factor(comparisonTable$Predicted)))
predictedPlot
library(gridExtra)
grid.arrange(originalPlot, predictedPlot, ncol=2)

#Individual Data
groupInddataSet <- indSubset

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

data2Use <- groupCleaned
data2Use <- data2Use[(data2Use$NPS_Type == "Detractor" | data2Use$NPS_Type == "Promoter"),]
data2Use$NPS_Type <- as.factor(data2Use$NPS_Type)


randomizedIndex <- (sample(1:dim(data2Use)[1]))
head(randomizedIndex)

indexFortowthird <- floor(2*(dim(data2Use)[1])/3)

trainingData <- data2Use[1:indexFortowthird,]
testingData <- data2Use[(indexFortowthird+1):nrow(data2Use),]

library(kernlab)
ksvm_Model<-ksvm(NPS_Type~staffCareType + tranquilityType + `Dry-Cleaning_PL`  + `All Suites_PL` + Elevators_PL + Laundry_PL + `Brand Initial_PL` +  Type_PL   + Relationship_PL  + InteretSatType + `Fitness Center_PL`,data=trainingData, kernel = "rbfdot", kpar = "automatic", C = 50, cross = 3, prob.model = TRUE)
ksvm_Model

ksvmPredict <- predict(ksvm_Model, testingData)
length(ksvmPredict)
nrow(testingData)

#plot(ksvm_Model, testingData, staffCareType ~ tranquilityType)

testingData$predictedValues <- ksvmPredict


functionForKSVMAccuracy <- function(dataSet){
  a <- ifelse(dataSet$testingData.NPS_Type == dataSet$testingData.predictedValues, 1,0)
  return(length(which((a==1) == TRUE))/nrow(dataSet) * 100)
}

comparisonTable <- data.frame(testingData$NPS_Type,testingData$predictedValues)
table(comparisonTable)
functionForKSVMAccuracy(comparisonTable)
colnames(comparisonTable) <- c("Original", "Predicted")
originalPlot <- ggplot(comparisonTable, aes(x=comparisonTable$Original)) + geom_bar(aes( fill=factor(comparisonTable$Original)))
#                , aes(x=comparisonTable$testingData.NPS_Type, y=comparisonTable$testingData.predictedValues)) + geom_col(aes( fill=factor(comparisonTable$testingData.predictedValues)))
originalPlot
predictedPlot <- ggplot(comparisonTable, aes(x=comparisonTable$Predicted)) + geom_bar(aes( fill=factor(comparisonTable$Predicted)))
predictedPlot
library(gridExtra)
grid.arrange(originalPlot, predictedPlot, ncol=2)

library(naivebayes)
nb_Model <- naiveBayes(NPS_Type~staffCareType + tranquilityType + `Dry-Cleaning_PL`  + `All Suites_PL` + Elevators_PL + Laundry_PL + `Brand Initial_PL` +  Type_PL   + Relationship_PL  + InteretSatType + `Fitness Center_PL`, data=trainingData)
nbPred <- predict(nb_Model, testingData)

length(nbPred)
nrow(testingData)


testingData$predictedValuesNB <- nbPred
comparisonTable <- data.frame(testingData$NPS_Type,testingData$predictedValuesNB)
table(comparisonTable)
functionForNBAccuracy <- function(dataSet){
  a <- ifelse(dataSet$testingData.NPS_Type == dataSet$testingData.predictedValuesNB, 1,0)
  return(length(which((a==1) == TRUE))/nrow(dataSet) * 100)
}
functionForNBAccuracy(comparisonTable)
colnames(comparisonTable) <- c("Original", "Predicted")
originalPlot <- ggplot(comparisonTable, aes(x=comparisonTable$Original)) + geom_bar(aes( fill=factor(comparisonTable$Original)))
#                , aes(x=comparisonTable$testingData.NPS_Type, y=comparisonTable$testingData.predictedValues)) + geom_col(aes( fill=factor(comparisonTable$testingData.predictedValues)))
originalPlot
predictedPlot <- ggplot(comparisonTable, aes(x=comparisonTable$Predicted)) + geom_bar(aes( fill=factor(comparisonTable$Predicted)))
predictedPlot
library(gridExtra)
grid.arrange(originalPlot, predictedPlot, ncol=2)


install.packages("caret")
library(caret)

dsf <- varImp(ksvm_Model)
