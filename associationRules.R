#Association rules for Gender - Male & Female Subset

#Load library for visualization
library(ggplot2)

#Find unique values from Gender
unique(combinedDataMod$Gender_H)
combinedDataModT <- combinedDataMod[(combinedDataMod$Gender_H=="Male") | (combinedDataMod$Gender_H=="Female"),]

#Plot bar graph for gender data
FemMalePlot <- ggplot(combinedDataModT, aes(x=as.factor(combinedDataModT$Gender_H))) + geom_bar(aes( fill=factor(combinedDataModT$NPS_Type))) + ggtitle("Distribution of NPS Type with Gender") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Gender of Traveller") + ylab("Count of Traveller") + labs(fill="NPS Type")
FemMalePlot


asd <- combinedDataModT
asd <- checkAndRemoveEmptyandNAvalues(asd, asd$Tranquility_H)
ggplot(asd, aes(Tranquility_H, fill = factor(NPS_Type))) + 
  geom_histogram() + facet_grid(.~Gender_H) + theme_few()
#####

FemaleSubset <- subset(combinedDataModT, combinedDataModT$Gender_H == "Female")
MaleSubset <- subset(combinedDataModT, combinedDataModT$Gender_H == "Male")


#Female Subset
dataAssocitionSet <- FemaleSubset

#Clean columns in Female Subset
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssocitionSet, dataAssocitionSet$`Dry-Cleaning_PL`)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$`All Suites_PL`)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Casino_PL)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Elevators_PL)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Laundry_PL)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$LENGTH_OF_STAY_CATEGORY_R)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$`Brand Initial_PL`)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Type_PL)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Relationship_PL)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$OSatisfactionType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$guesRoomSatisfactionType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$tranquilityType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$customerSatisfactionType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$staffCareType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$InteretSatType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$`Fitness Center_PL`)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$`Business Center_PL`)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Golf_PL)
#dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Conference_PL)







#Store all variables as factors

dataAssociationCleaned$`Dry-Cleaning_PL` <- as.factor(dataAssociationCleaned$`Dry-Cleaning_PL`)
dataAssociationCleaned$`All Suites_PL` <- as.factor(dataAssociationCleaned$`All Suites_PL`)
dataAssociationCleaned$Casino_PL <- as.factor(dataAssociationCleaned$Casino_PL)
dataAssociationCleaned$Elevators_PL <- as.factor(dataAssociationCleaned$Elevators_PL)
dataAssociationCleaned$Laundry_PL <- as.factor(dataAssociationCleaned$Laundry_PL)
dataAssociationCleaned$NPS_Type <- as.factor(dataAssociationCleaned$NPS_Type)
dataAssociationCleaned$LENGTH_OF_STAY_CATEGORY_R <- as.factor(dataAssociationCleaned$LENGTH_OF_STAY_CATEGORY_R)
dataAssociationCleaned$`Brand Initial_PL` <- as.factor(dataAssociationCleaned$`Brand Initial_PL`)
dataAssociationCleaned$Type_PL <- as.factor(dataAssociationCleaned$Type_PL)
dataAssociationCleaned$Relationship_PL <- as.factor(dataAssociationCleaned$Relationship_PL)
dataAssociationCleaned$OSatisfactionType <- as.factor(dataAssociationCleaned$OSatisfactionType)
dataAssociationCleaned$guesRoomSatisfactionType <- as.factor(dataAssociationCleaned$guesRoomSatisfactionType)
dataAssociationCleaned$tranquilityType <- as.factor(dataAssociationCleaned$tranquilityType)
dataAssociationCleaned$customerSatisfactionType <- as.factor(dataAssociationCleaned$customerSatisfactionType)
dataAssociationCleaned$staffCareType <- as.factor(dataAssociationCleaned$staffCareType)
dataAssociationCleaned$InteretSatType <- as.factor(dataAssociationCleaned$InteretSatType)
dataAssociationCleaned$`Fitness Center_PL` <- as.factor(dataAssociationCleaned$`Fitness Center_PL`)
dataAssociationCleaned$Golf_PL <- as.factor(dataAssociationCleaned$Golf_PL)
dataAssociationCleaned$`Business Center_PL` <- as.factor(dataAssociationCleaned$`Business Center_PL`)

#Select columns for Assioication Rules
colForArules <- dataAssociationCleaned[, c("Dry-Cleaning_PL", "All Suites_PL", "Casino_PL", "Elevators_PL", "Laundry_PL", "NPS_Type","LENGTH_OF_STAY_CATEGORY_R",
                                 "Brand Initial_PL","Type_PL","Relationship_PL", "Fitness Center_PL",
                                 "tranquilityType", "staffCareType", "InteretSatType")]

#Load libraries to plot association rules
library(arulesViz)
library(arules)

genderRulesPromoters <- apriori(colForArules, parameter = list(support = 0.6, confidence = 0.86), appearance = list(default  = "lhs", rhs=("NPS_Type=Promoter")))
summary(genderRulesPromoters)
inspect(genderRulesPromoters)

genderRulesPromoters <- sort(genderRulesPromoters, by="lift")
inspect(head(genderRulesPromoters))

plot(genderRulesPromoters)

plot((genderRulesPromoters), method="paracoord", control=list(reorder=TRUE))
plot((genderRulesPromoters), method="graph", control=list(type="items"))


#Male Subset

dataAssocitionSet <- MaleSubset

dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssocitionSet, dataAssocitionSet$`Dry-Cleaning_PL`)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$`All Suites_PL`)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Casino_PL)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Elevators_PL)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Laundry_PL)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$LENGTH_OF_STAY_CATEGORY_R)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$`Brand Initial_PL`)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Type_PL)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Relationship_PL)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$OSatisfactionType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$guesRoomSatisfactionType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$tranquilityType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$customerSatisfactionType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$staffCareType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$InteretSatType)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$`Fitness Center_PL`)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$`Business Center_PL`)
dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Golf_PL)
#dataAssociationCleaned <- checkAndRemoveEmptyandNAvalues(dataAssociationCleaned, dataAssociationCleaned$Conference_PL)







#Store all variables as factors

dataAssociationCleaned$`Dry-Cleaning_PL` <- as.factor(dataAssociationCleaned$`Dry-Cleaning_PL`)
dataAssociationCleaned$`All Suites_PL` <- as.factor(dataAssociationCleaned$`All Suites_PL`)
dataAssociationCleaned$Casino_PL <- as.factor(dataAssociationCleaned$Casino_PL)
dataAssociationCleaned$Elevators_PL <- as.factor(dataAssociationCleaned$Elevators_PL)
dataAssociationCleaned$Laundry_PL <- as.factor(dataAssociationCleaned$Laundry_PL)
dataAssociationCleaned$NPS_Type <- as.factor(dataAssociationCleaned$NPS_Type)
dataAssociationCleaned$LENGTH_OF_STAY_CATEGORY_R <- as.factor(dataAssociationCleaned$LENGTH_OF_STAY_CATEGORY_R)
dataAssociationCleaned$`Brand Initial_PL` <- as.factor(dataAssociationCleaned$`Brand Initial_PL`)
dataAssociationCleaned$Type_PL <- as.factor(dataAssociationCleaned$Type_PL)
dataAssociationCleaned$Relationship_PL <- as.factor(dataAssociationCleaned$Relationship_PL)
dataAssociationCleaned$OSatisfactionType <- as.factor(dataAssociationCleaned$OSatisfactionType)
dataAssociationCleaned$guesRoomSatisfactionType <- as.factor(dataAssociationCleaned$guesRoomSatisfactionType)
dataAssociationCleaned$tranquilityType <- as.factor(dataAssociationCleaned$tranquilityType)
dataAssociationCleaned$customerSatisfactionType <- as.factor(dataAssociationCleaned$customerSatisfactionType)
dataAssociationCleaned$staffCareType <- as.factor(dataAssociationCleaned$staffCareType)
dataAssociationCleaned$InteretSatType <- as.factor(dataAssociationCleaned$InteretSatType)
dataAssociationCleaned$`Fitness Center_PL` <- as.factor(dataAssociationCleaned$`Fitness Center_PL`)
dataAssociationCleaned$Golf_PL <- as.factor(dataAssociationCleaned$Golf_PL)
dataAssociationCleaned$`Business Center_PL` <- as.factor(dataAssociationCleaned$`Business Center_PL`)

#Columns for Male Subset
colForArules <- dataAssociationCleaned[, c("Dry-Cleaning_PL", "All Suites_PL", "Casino_PL", "Elevators_PL", "Laundry_PL", "NPS_Type","LENGTH_OF_STAY_CATEGORY_R",
                                           "Brand Initial_PL","Type_PL","Relationship_PL", "Fitness Center_PL",
                                           "tranquilityType", "staffCareType", "InteretSatType")]


genderRulesPromoters <- apriori(colForArules, parameter = list(support = 0.6, confidence = 0.8), appearance = list(default  = "lhs", rhs=("NPS_Type=Promoter")))
summary(genderRulesPromoters)
inspect(genderRulesPromoters)

genderRulesPromoters <- sort(genderRulesPromoters, by="lift")
inspect(head(genderRulesPromoters))

plot(genderRulesPromoters)

plot((genderRulesPromoters), method="paracoord", control=list(reorder=TRUE))

plot((genderRulesPromoters), method="graph", control=list(type="items"))

