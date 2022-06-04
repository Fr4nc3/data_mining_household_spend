######################################################
# Francia F. Riesco
# Housing Spend Modeling 
# Linear Regression
######################################################


######################################################
# Verify correlation between individual variables and the dependent variable
######################################################
for (i in informativeFeatureNames){
  print(i)
  x = joinData[[i]]
  
  aov1 = aov(y~ x)
  print(summary(aov1))
  if(class(joinData[[i]]) =="integer" | class(joinData[[i]]) =="numeric" ){
    print(cor(y,x))
    print(cor.test(y,x))
  }
}

# train set preparation 
trainSet <-joinData

dataPlan     <- designTreatmentsN(joinData, 
                                  informativeFeatureNames, 
                                  outcomeVariableName)

# treat the three dataset with tme same plan  
treatedTrain <- prepare(dataPlan, trainSet)
treatedTest <- prepare(dataPlan, joinTestData)
treatedProspects <- prepare(dataPlan, joinProspData)

# linear regression model 
set.seed(1234)
fit <- train(yHat ~ .,
                data = treatedTrain,
                method = "lm",
                verbose = FALSE
)
fit # summary results
lm_resid = resid(fit)
lm_fitted = fitted(fit)


# Plot the residuals to check the assumptions.
plot(x = lm_fitted, y = lm_resid,axes=TRUE, main="Fitted Values vs Residuals", frame.plot=TRUE, xlab = "fitted values", ylab="residuals")
abline(h = 0)

ggplot(data=,mapping=aes(x=fits,y=resids)) +
  geom_point() +
  geom_hline(yintercept=0,linetype="dashed")

# review if residuals are normaly distributed 
df<- data.frame(resid = lm_resid, fits=lm_fitted)
ggplot(data=df,mapping=aes(x=fits,y=resid)) +
  geom_point(color="skyblue3", 
             fill="skyblue1", ) +
  geom_hline(yintercept=0,linetype="dashed")+ 
  geom_smooth() +
labs(title="", x="Residuals", y="fitters")


ggplot(data=df, aes(df$resid)) + 
  geom_histogram(aes(y =..density..), 
                 #breaks=seq(), 
                 bins = 10,
                 col="skyblue3", 
                 fill="skyblue1", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title="", x="Residuals", y="Counts")

######################################################
# Validate model with prediction
######################################################

trainingPreds <- predict(fit, treatedTrain)

g <- roc(joinData$yHat ~ trainingPreds )
ggroc(g)
plot(density(trainingPreds))
ggplot(data.frame(density = trainingPreds), aes(x=trainingPreds)) +
  geom_density(fill="skyblue1", color="skyblue3", alpha=0.8)

#Organize training set preds
trainingResults <-data.frame(actuals        = treatedTrain$yHat,
                             predicted      = trainingPreds,
                             residualErrors = treatedTrain$yHat-trainingPreds )
head(trainingResults)


(trainRMSE <- MLmetrics::RMSE(trainingResults$predicted, 
                              trainingResults$actuals))

# What is the MAPE?
(trainMAPE <- MLmetrics::MAPE(trainingResults$predicted, 
                              trainingResults$actuals))

testPreds   <- predict(fit, treatedTest) 
plot(density(testPreds))
#Organize training set preds
testResults <- data.frame(actuals   = joinTestData$yHat,
                          predicted = testPreds)
head(testResults)

# KPI
(testRMSE <- MLmetrics::RMSE(testResults$predicted, 
                             testResults$actuals))

# What is the MAPE?
(testMAPE <- MLmetrics::MAPE(testResults$predicted, 
                             testResults$actuals))

# Side by Side
trainRMSE
testRMSE

trainMAPE
testMAPE


forecast::accuracy(testResults$predicted, testResults$actuals)

######################################################
# prospect data 
######################################################

prospPreds   <- predict(fit, treatedProspects) 
plot(density(prospPreds))

######################################################
# Linear Regression is my best model therefore this is 
# predicted prospect CSV
######################################################
#model results, and provide the predicted prospect values in a CSV
predicted_prospect <- data.frame(tmpID=joinProspData$tmpID,
                                 FirsName= joinProspData$FirstName,
                                 LastName= joinProspData$LastName,
                                   prospectSpend = prospPreds 
)
predicted_prospect

write.csv(predicted_prospect, "PredictedPropspect.csv", row.names = FALSE, quote = FALSE)



