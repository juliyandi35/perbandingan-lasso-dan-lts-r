library(readxl)
Dataset <- read_excel('data penelitian.xlsx')
replace_na_with_zero <- function(vec) {
  vec[is.na(vec)] <- 0
  return(vec)
}
Dataset <- apply(Dataset, 2, replace_na_with_zero)
Dataset <- Dataset[,-1]
Dataset <- data.frame(Dataset)

rmse <- function(predictions, actuals) {
  sqrt(mean((predictions - actuals)^2))
}

library(dplyr)
char_cols <- sapply(Dataset, is.character)
# Convert character columns to numeric
Dataset[char_cols] <- lapply(Dataset[char_cols], as.numeric)

library(glmnet)

ind <- sample(2, nrow(Dataset), replace = TRUE, prob = c(0.7, 0.3))

train <- Dataset[ind==1,]

test <- Dataset[ind==2,]

library(robustHD)
X.train <- train[,-1]
Y.train <- train[,1]
X.test <- test[,-1]
Y.test <- test[,1]
model <- sparseLTS(X.train,Y.train)
coef(model)

lambdas <- 10^seq(10, -2, length = 100)
sparselts_rmse <- matrix(0,ncol = length(lambdas),nrow = 1)
for (i in 1:length(lambdas)){
  y_predicted <- predict(model,s=lambdas[i], newx = as.matrix(X.test))
  sparselts_rmse[,i] <- rmse(y_predicted, Y.test)
}
sparselts_rmse <- as.vector(sparselts_rmse)

r_squared <- function(predictions, actuals) {
  1 - sum((actuals - predictions)^2) / sum((actuals - mean(actuals))^2)
}

sparselts_r_squared <- r_squared(y_predicted, Y.test)
sparselts_r_squared

library(ggplot2)
# Create a scatter plot
ggplot(model, aes(x = model$fitted.values, y = (model$residuals)^2,color='orange')) +
  geom_point()+
  labs(title = "Residuals VS Fitted Values",
       x = "Fitted Values",
       y = "Standarized Residuals")

df <- data.frame(lambdas,sparselts_rmse)
ggplot(df, aes(x = lambdas, y = sparselts_rmse)) +
  geom_line() +
  labs(x = "Lambda", y = "Prediction Error (RMSE)" )+
  theme_minimal()

