###RIDGE REGRESSION

# Setting the range of lambda values
lambda_seq <- 10^seq(2, -2, by = -.1)

# Using glmnet function to build the ridge regression in r
x_var <- data.matrix(subset(newdata, select = -c(ic50_Omicron)))

# Getting the dependent variable
y_var <- newdata[, "ic50_Omicron"]

x <- data.matrix(newdata.test)

fit <- glmnet(x_var, y_var, alpha = 0, lambda  = lambda_seq)

# Checking the model
summary(fit)

ridge_cv <- cv.glmnet(x_var, y_var, alpha = 0, lambda = lambda_seq)

# Best lambda value
best_lambda <- ridge_cv$lambda.min
best_lambda

best_fit <- ridge_cv$glmnet.fit

head(best_fit)

best_ridge <- glmnet(x_var, y_var, alpha = 0, lambda = 1.995262)

coef(best_ridge)

ridge <- predict(best_ridge, s = best_lambda, newx = x)

fwrite(test[,.(Id,ridge)], './project/volume/data/processed/ridge.csv')

saveRDS(ridge_cv, './project/volume/modelsridge.model')



