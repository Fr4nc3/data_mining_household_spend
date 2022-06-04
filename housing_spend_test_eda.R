######################################################
# Load the test data
######################################################
donationsTestData <- read.csv("DonationsData_testing5K_studentVersion.csv")
consumerTestData <- read.csv("consumerData_testing5K_studentVersion.csv")
inHouseTestData <- read.csv("inHouseData_testing5K_studentVersion.csv")
magazineTestData <- read.csv("magazineData_testing5K_studentVersion.csv")
politicalTestData <- read.csv("politicalData_testing5K_studentVersion.csv")

joinTestData <- left_join(donationsTestData, consumerTestData, by = c("tmpID"))
joinTestData <- left_join(joinTestData, inHouseTestData, by = c("tmpID"))
joinTestData <- left_join(joinTestData, magazineTestData, by = c("tmpID"))
joinTestData <- left_join(joinTestData, politicalTestData, by = c("tmpID"))

write.csv(joinTestData , "test_housing.csv", row.names = FALSE, quote = FALSE)
summary(joinTestData)
colnames(joinTestData)
colSums(is.na(joinTestData))
colSums(joinTestData== "")
## use same rules from training to keep consistency
# train_col_null is from training 
for (i in train_col_null){
  joinTestData[[i]][is.na(joinTestData[[i]])] <- mean(joinTestData[[i]], na.rm = TRUE)
}
#review 
colSums(is.na(joinData))

# drop same columns from training 
joinTestData <- joinTestData[ , !(names(joinTestData) %in% train_col_empty)]

# fixing same values than training 
#  LandValue, EstHomeValue these are numeric values that are string

joinTestData$LandValue<-as.numeric(gsub("\\$","",as.character(joinTestData$LandValue)))
joinTestData$EstHomeValue<-as.numeric(gsub("\\$","",as.character(joinTestData$EstHomeValue)))

# verify convert on numeric correctly
summary(joinTestData$LandValue)
summary(joinTestData$EstHomeValue)

joinTestData$LandValue[is.na(joinTestData$LandValue)] <- mean(joinTestData$LandValue, na.rm = TRUE)
joinTestData$EstHomeValue[is.na(joinTestData$EstHomeValue)] <- mean(joinTestData$EstHomeValue, na.rm = TRUE)

# fix all categorial fixed with the mode same columns from training
for (i in train_col_empty_fix){
  joinTestData[[i]][joinTestData[[i]]==""] <- NA
  joinTestData[[i]][is.na(joinTestData[[i]])]  <- Mode(joinTestData[[i]])
}

######################################################
# Clean testing set and save csv file
######################################################
write.csv(joinTestData , "test_housing_clean.csv", row.names = FALSE, quote = FALSE)




