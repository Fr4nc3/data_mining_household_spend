######################################################
# Francia F. Riesco
# Housing Spend Modeling 
# Model random forest
######################################################
set.seed(1234)
rf_fit <- train(yHat ~ .,
             data = treatedTrain,
             method = "rf",
             verbose = FALSE,
             ntree = 10
             ) 
rf_fit # summary results

rf_resid = resid(rf_fit)
rf_fitted = fitted(fit)



# Plot the residuals to check the assumptions.
plot(x = rf_fitted, y = rf_resid,axes=TRUE, main="Fitted Values vs Residuals", frame.plot=TRUE, xlab = "fitted values", ylab="residuals")
abline(h = 0)

df_rf<- data.frame(resid = rf_resid, fits=rf_fitted)
ggplot(data=df_rf,mapping=aes(x=fits,y=resid)) +
  geom_point(color="skyblue3", 
             fill="skyblue1", ) +
  geom_hline(yintercept=0,linetype="dashed")+ 
  geom_smooth() +
  labs(title="", x="Residuals", y="fitters")


ggplot(data=df_rf, aes(df_rf$resid)) + 
  geom_histogram(aes(y =..density..), 
                 #breaks=seq(), 
                 bins = 10,
                 col="skyblue3", 
                 fill="skyblue1", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title="", x="Residuals", y="Counts")


trainingPreds_rf <- predict(rf_fit, treatedTrain)

g <- roc(joinData$yHat ~ trainingPreds_rf )
plot(g) 
ggroc(g)
plot(density(trainingPreds_rf))

ggplot(data.frame(density = trainingPreds_rf), aes(x=trainingPreds_rf)) +
  geom_density(fill="skyblue1", color="skyblue3", alpha=0.8)


#Organize training set preds
trainingResults_rf <-data.frame(actuals        = treatedTrain$yHat,
                             predicted      = trainingPreds_rf,
                             residualErrors = treatedTrain$yHat-trainingPreds_rf )
head(trainingResults_rf)


(trainRMSE_rf <- MLmetrics::RMSE(trainingResults_rf$predicted, 
                              trainingResults_rf$actuals))

# What is the MAPE?
(trainMAPE_rf <- MLmetrics::MAPE(trainingResults_rf$predicted, 
                              trainingResults_rf$actuals))


# Since we haven't looked at the test set, we *could* go back and adjust the model.
# Let's continue to the test set evaluation



testPreds_rf   <- predict(rf_fit, treatedTest) 
plot(density(testPreds_rf))
#Organize training set preds
testResults_rf <- data.frame(actuals   = joinTestData$yHat,
                          predicted = testPreds_rf)
head(testResults_rf)

# KPI
(testRMSE_rf <- MLmetrics::RMSE(testResults_rf$predicted, 
                             testResults_rf$actuals))

# What is the MAPE?
(testMAPE_rf <- MLmetrics::MAPE(testResults_rf$predicted, 
                             testResults_rf$actuals))

# Side by Side
trainRMSE_rf
testRMSE_rf

trainMAPE_rf
testMAPE_rf


forecast::accuracy(testResults_rf$predicted, testResults_rf$actuals)

######################################################
# prospect data 
######################################################

prospPreds_rf   <- predict(rf_fit, treatedProspects) 
plot(density(prospPreds_rf))
