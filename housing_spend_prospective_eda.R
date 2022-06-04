######################################################
# Load the prospective data
######################################################
donationsProspData <- read.csv("DonationsData_prospects6K_studentVersion.csv")
consumerProspData <- read.csv("consumerData_prospects6K_studentVersion.csv")
inHouseProspData <- read.csv("inHouseData_prospects6K_studentVersion.csv")
magazineProspData <- read.csv("magazineData_prospects6K_studentVersion.csv")
politicalProspData <- read.csv("politicalData_prospects6K_studentVersion.csv")

joinProspData <- left_join(donationsProspData, consumerProspData, by = c("tmpID"))
joinProspData <- left_join(joinProspData, inHouseProspData, by = c("tmpID"))
joinProspData <- left_join(joinProspData, magazineProspData, by = c("tmpID"))
joinProspData <- left_join(joinProspData, politicalProspData, by = c("tmpID"))


write.csv(joinProspData , "prosp_housing.csv", row.names = FALSE, quote = FALSE)
summary(joinProspData)
colnames(joinProspData)
colSums(is.na(joinProspData))
colSums(joinProspData== "")
## use same rules from training to keep consistency
# train_col_null is from training 
for (i in train_col_null){
  joinProspData[[i]][is.na(joinProspData[[i]])] <- mean(joinProspData[[i]], na.rm = TRUE)
}
#review 
colSums(is.na(joinProspData))

# drop same columns from training 
joinProspData <- joinProspData[ , !(names(joinProspData) %in% train_col_empty)]

# fixing same values than training 
#  LandValue, EstHomeValue these are numeric values that are string

joinProspData$LandValue<-as.numeric(gsub("\\$","",as.character(joinProspData$LandValue)))
joinProspData$EstHomeValue<-as.numeric(gsub("\\$","",as.character(joinProspData$EstHomeValue)))

# verify convert on numeric correctly
summary(joinProspData$LandValue)
summary(joinProspData$EstHomeValue)

joinProspData$LandValue[is.na(joinProspData$LandValue)] <- mean(joinProspData$LandValue, na.rm = TRUE)
joinProspData$EstHomeValue[is.na(joinProspData$EstHomeValue)] <- mean(joinProspData$EstHomeValue, na.rm = TRUE)

# fix all categorial fixed with the mode same columns from training
for (i in train_col_empty_fix){
  joinProspData[[i]][joinProspData[[i]]==""] <- NA
  joinProspData[[i]][is.na(joinProspData[[i]])]  <- Mode(joinProspData[[i]])
}

######################################################
# Clean testing set and save csv file
######################################################
write.csv(joinProspData , "prosp_housing_clean.csv", row.names = FALSE, quote = FALSE)