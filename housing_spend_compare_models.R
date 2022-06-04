######################################################
# Francia F. Riesco
# Housing Spend Modeling 
# Compare Models
######################################################


fit$finalModel #linear regression
rf_fit$finalModel
knn_fit$finalModel


fit$results
rf_fit$results
knn_fit$results

# the lowest RMSE and highest R-squared is the best model 
model_list <- list(lm = fit, rf = rf_fit, knn=knn_fit)
res <- resamples(model_list)
summary(res)