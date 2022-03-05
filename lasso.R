###LASSO REGRESSION
set.seed(1900)

#cross validation 10 fold
ctrlspecs <- trainControl(method='cv', number=10,
                          savePredictions = 'all')


#Train Lasso Regression

#lambda values
lambda_vector <- 10^seq(5, -5, length=500)

#set a seed
set.seed(1900)

lasso_model <- train(ic50_Omicron~ ., data = newdata,
                     preProcess = c("center","scale"),
                     method = "glmnet",
                     tuneGrid = expand.grid(alpha=1, lambda = lambda_vector),
                     trControl = ctrlspecs,
                     na.action = na.omit)

#Optimal tuning parameter (alpha and lambda)
lasso_model$bestTune
lasso_model$bestTune$lambda

#lasso regression model coefficients
round(coef(lasso_model$finalModel, lasso_model$bestTune$lambda), 4)
#dotted ones are eliminated because they have no regression coefficients

#RMSE and log(lambda)
plot(log(lasso_model$results$lambda),
     lasso_model$results$RMSE,
     xlab = "log(lambda)",
     ylab = "RMSE")

#Variable importance
varImp(lasso_model)
ggplot(varImp(lasso_model))


#Model Prediction
prediction <- predict(lasso_model, newdata=newdata.test)

#accuracy
perf <- data.frame(RMSE= RMSE(prediction, newdata$ic50_Omicron))
perf

fwrite(test[,.(Id,prediction)], './project/volume/data/processed/lasso.csv')

#Need to use saveRDS to save the model
saveRDS(prediction, './project/volume/models/lasso.model')


