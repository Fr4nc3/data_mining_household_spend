######################################################
# Francia F. Riesco
# Housing  EDA
######################################################

######################################################
# Libraries that we will use on this process
######################################################
library(dplyr)
library(vtreat)
library(caret)
library(dplyr)
library(rpart.plot) # visualizing
library(MLmetrics)
library(ROSE)
library(ggplot2)
library(ggthemes)
library(randomForest)
library(ggmap)
library(ltm)
library(ggpubr)
library(ModelMetrics)
library(forecast)
library(aod)
library(pROC)

options(scipen = 999)

setwd("~/workstation/fr-p/Github/data_mining_household_spend/studentTables")
######################################################
# categorial values missing will be update with the mode of the field
# numerical values will be update with the mean of the field
# create mode function
######################################################
Mode <- function(x) {
  ux <- na.omit(unique(x))
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
######################################################
# Load the training dataset
######################################################
donationsData <- read.csv("DonationsData_training15K_studentVersion.csv")
consumerData <- read.csv("consumerData_training15K_studentVersion.csv")
inHouseData <- read.csv("inHouseData_training15K_studentVersion.csv")
magazineData <- read.csv("magazineData_training15K_studentVersion.csv")
politicalData <- read.csv("politicalData_training15K_studentVersion.csv")

joinData <- left_join(donationsData, consumerData, by = c("tmpID"))
joinData <- left_join(joinData, inHouseData, by = c("tmpID"))
joinData <- left_join(joinData, magazineData, by = c("tmpID"))
joinData <- left_join(joinData, politicalData, by = c("tmpID"))


write.csv(joinData, "training_housing.csv", row.names = FALSE, quote = FALSE)
summary(joinData)
colnames(joinData)
colSums(is.na(joinData))
colSums(joinData == "")

######################################################
# Identify null of empty variables
######################################################
train_col_null <- colnames(joinData[colSums(is.na(joinData)) > 0])
# no null column will be drop
# all numerical fixed with the mean
for (i in train_col_null) {
  joinData[[i]][is.na(joinData[[i]])] <- mean(joinData[[i]], na.rm = TRUE)
}


######################################################
# find columns that have more than 5000 empty entries
######################################################
train_col_empty <- colnames(joinData[colSums(joinData == "") > 5000])
######################################################
# Drop columns with too many empty entries
######################################################
joinData <- joinData[, !(names(joinData) %in% train_col_empty)]

#  LandValue, EstHomeValue these are numeric values that are string

joinData$LandValue <- as.numeric(gsub("\\$", "", as.character(joinData$LandValue)))
joinData$EstHomeValue <- as.numeric(gsub("\\$", "", as.character(joinData$EstHomeValue)))

# verify convert on numeric correctly
summary(joinData$LandValue)
summary(joinData$EstHomeValue)

joinData$LandValue[is.na(joinData$LandValue)] <- mean(joinData$LandValue, na.rm = TRUE)
joinData$EstHomeValue[is.na(joinData$EstHomeValue)] <- mean(joinData$EstHomeValue, na.rm = TRUE)
######################################################
# fixing empty entries
######################################################
train_col_empty_fix <- colnames(joinData[colSums(joinData == "") > 0])

# fix all categorial fixed with the mode
for (i in train_col_empty_fix) {
  joinData[[i]][joinData[[i]] == ""] <- NA
  joinData[[i]][is.na(joinData[[i]])] <- Mode(joinData[[i]])
}

######################################################
# Clean training set and save csv file
######################################################
write.csv(joinData, "training_housing_clean.csv", row.names = FALSE, quote = FALSE)

######################################################
# split informative variable from outcome variable
######################################################

# variables : TelephonesFullPhone, FirstName, LastName, lon, lat, EthnicDescription
# removed lon, lat, similar tan county, ethicDescription with BroadEthnicGroupings
all_coll <- colnames(joinData)
all_coll
informativeFeatureNames <- all_coll[all_coll %in% c("tmpID", "yHat", "TelephonesFullPhone", "FirstName", "LastName", "lon", "lat", "EthnicDescription") == FALSE]
informativeFeatureNames
outcomeVariableName <- "yHat"
yHat <- joinData$yHat
y <- yHat # just to have as simple y name


######################################################
# Plot informative variables
######################################################
for (i in informativeFeatureNames) {
  barplot(table(joinData[[i]]), main = i)
}

for (i in informativeFeatureNames) {
  x <- joinData[[i]]
  print(ggplot(joinData, aes(x = x, y = y)) +
    geom_point() +
    labs(title = "Scatterplot", x = i, y = "yHat") +
    geom_smooth(method = "lm"))
}


x <- joinData$MedianEducationYears
df_gg <- data.frame(value = x)
ggplot(data = df_gg, aes(df_gg$value)) +
  geom_histogram(aes(y = ..density..),
    # bins = 10,
    col = "skyblue3",
    fill = "skyblue1",
    alpha = .2
  ) +
  geom_density(col = 2) +
  labs(title = "Histogram MedianEducationYears", x = "MedianEducationYears", y = "Counts")
ggplot(joinData, aes(x = as.factor(PropertyType))) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  geom_bar(color = "skyblue3", fill = "skyblue1") +
  labs(title = "PropertyType", x = "PropertyType", y = "Counts")


ggplot(data = joinData) +
  aes(x = PropertyType, y = yHat) +
  scale_color_brewer(palette = "Blue") +
  geom_jitter()