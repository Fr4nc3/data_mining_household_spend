######################################################
# Francia F. Riesco
# Housing Spend Modeling 
# Model k-Nearest neighbor KNN
######################################################

set.seed(1234)
knn_fit <- train(yHat ~ .,
                data = treatedTrain,
                method = "knn",
                verbose = FALSE,
                preProcess = c("center", "scale")
) 
knn_fit # summary results

knn_resid = resid(knn_fit)
knn_fitted = fitted(fit)



# Plot the residuals to check the assumptions.
plot(x = knn_fitted, y = knn_resid,axes=TRUE, main="Fitted Values vs Residuals", frame.plot=TRUE, xlab = "fitted values", ylab="residuals")
abline(h = 0)

df_knn<- data.frame(resid = knn_resid, fits=knn_fitted)
ggplot(data=df_knn,mapping=aes(x=fits,y=resid)) +
  geom_point(color="skyblue3", 
             fill="skyblue1", ) +
  geom_hline(yintercept=0,linetype="dashed")+ 
  geom_smooth() +
  labs(title="", x="Residuals", y="fitters")

ggplot(data=df_knn, aes(df_knn$resid)) + 
  geom_histogram(aes(y =..density..), 
                 #breaks=seq(), 
                 bins = 10,
                 col="skyblue3", 
                 fill="skyblue1", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title="", x="Residuals", y="Counts")



trainingPreds_knn <- predict(knn_fit, treatedTrain)

g <- roc(joinData$yHat ~ trainingPreds_knn )
plot(g) 
plot(density(trainingPreds_knn))
ggplot(data.frame(density = trainingPreds_knn), aes(x=trainingPreds_knn)) +
  geom_density(fill="skyblue1", color="skyblue3", alpha=0.8)

#Organize training set preds
trainingResults_knn <-data.frame(actuals        = treatedTrain$yHat,
                                predicted      = trainingPreds_knn,
                                residualErrors = treatedTrain$yHat-trainingPreds_knn )
head(trainingResults_knn)


(trainRMSE_knn <- MLmetrics::RMSE(trainingResults_knn$predicted, 
                                 trainingResults_knn$actuals))

# What is the MAPE?
(trainMAPE_knn <- MLmetrics::MAPE(trainingResults_knn$predicted, 
                                 trainingResults_knn$actuals))


# Since we haven't looked at the test set, we *could* go back and adjust the model.
# Let's continue to the test set evaluation



testPreds_knn   <- predict(knn_fit, treatedTest) 
plot(density(testPreds_knn))
#Organize training set preds
testResults_knn <- data.frame(actuals   = joinTestData$yHat,
                             predicted = testPreds_knn)
head(testResults_knn)

# KPI
(testRMSE_knn <- MLmetrics::RMSE(testResults_knn$predicted, 
                                testResults_knn$actuals))

# What is the MAPE?
(testMAPE_knn <- MLmetrics::MAPE(testResults_knn$predicted, 
                                testResults_knn$actuals))

# Side by Side
trainRMSE_knn
testRMSE_knn

trainMAPE_knn
testMAPE_knn


forecast::accuracy(testResults_knn$predicted, testResults_knn$actuals)

######################################################
# prospect data 
######################################################

prospPreds_knn   <- predict(knn_fit, treatedProspects) 
plot(density(prospPreds_knn))

