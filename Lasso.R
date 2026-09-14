library(readxl)
Dataset <- read_excel('data penelitian.xlsx')
replace_na_with_zero <- function(vec) {
  vec[is.na(vec)] <- 0
  return(vec)
}
Dataset <- apply(Dataset, 2, replace_na_with_zero)
Dataset <- Dataset[,-1]
Dataset <- data.frame(Dataset)

library(dplyr)
char_cols <- sapply(Dataset, is.character)
# Convert character columns to numeric
Dataset[char_cols] <- lapply(Dataset[char_cols], as.numeric)

library(glmnet)

ind <- sample(2, nrow(Dataset), replace = TRUE, prob = c(0.7, 0.3))

train <- Dataset[ind==1,]

test <- Dataset[ind==2,]

X.train <- train[,-1]
Y.train <- train[,1]
X.test <- test[,-1]
Y.test <- test[,1]
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(as.matrix(X.train), Y.train, alpha = 1)

#find optimal lambda value that minimizes test MSE
lambda <- cv_model$lambda
lambda

#produce plot of test MSE by lambda value
plot(cv_model)

#find coefficients of best model
coefs <- matrix(0, nrow = length(lambda), ncol = ncol(X.train))

for (i in 1:length(lambda)) {
  fit <- glmnet(X.train, Y.train, alpha = 1, lambda = lambda[i])
  coefs[i,] <- coef(fit, s = lambda[i])[-1]  # Excluding intercept
}

df <- data.frame(lambdas = lambda, coefs)
df_melted <- reshape2::melt(df, id.vars = "lambdas")

library(ggplot2)
# Create and display the plot
p <- ggplot(df_melted, aes(x = lambdas, y = value, color = variable)) +
  geom_line() +
  scale_x_log10() +
  labs(x = "Lambda (log scale)", y = "Coefficients") +
  theme_minimal()

print(p)

#use fitted best model to make predictions
best_lambda <- cv_model$lambda.min
best_model <- glmnet(as.matrix(X.train), Y.train, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = as.matrix(X.test))

#find SST and SSE
sst <- sum((Y.test - mean(Y.test))^2)
sse <- sum((y_predicted - Y.test)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

rmse <- function(predictions, actuals) {
  sqrt(mean((predictions - actuals)^2))
}

lasso_rmse <- rmse(y_predicted, Y.test)

r_squared <- function(predictions, actuals) {
  1 - sum((actuals - predictions)^2) / sum((actuals - mean(actuals))^2)
}

lasso_r_squared <- r_squared(y_predicted, Y.test)

