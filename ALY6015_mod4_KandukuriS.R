install.packages("ISLR")
install.packages("caret")
library(ISLR)
library(ggplot2)
summary(College)
data(College)
library(glmnet)

#Create Train and Test set - random sample (70/30 split)
trainIndex = sort(sample(x = nrow(College), size = nrow(College) * 0.7))
sample_train = College[trainIndex,]
sample_test = College[-trainIndex,]


# Convert the 'Private' variable to a binary numeric variable
College$Private <- as.numeric(College$Private == "Yes")

# Extract the target variable 'Grad.Rate' and convert it to a numeric vector (y)
y <- as.numeric(College$Grad.Rate)

# Extract the input features (independent variables) and convert them to a matrix (X)
X <- as.matrix(College[, c("Private", "Apps", "Accept", "Enroll", "Top10perc", "Top25perc", "F.Undergrad",
                           "P.Undergrad", "Outstate", "Room.Board", "Books", "Personal", "PhD", "Terminal",
                           "S.F.Ratio", "perc.alumni", "Expend")])

# Perform cross-validation to estimate lambda.min and lambda.1se for Ridge and Lasso
ridge_cv <- cv.glmnet(X, y, alpha = 0)  # alpha=0 for Ridge
lasso_cv <- cv.glmnet(X, y, alpha = 1)  # alpha=1 for Lasso

# Get the optimal lambda values
lambda_min_ridge <- ridge_cv$lambda.min
lambda_1se_ridge <- ridge_cv$lambda.1se

lambda_min_lasso <- lasso_cv$lambda.min
lambda_1se_lasso <- lasso_cv$lambda.1se

# Print the lambda values
cat("Ridge - lambda.min:", lambda_min_ridge, "\n")
cat("Ridge - lambda.1se:", lambda_1se_ridge, "\n")
cat("Lasso - lambda.min:", lambda_min_lasso, "\n")
cat("Lasso - lambda.1se:", lambda_1se_lasso, "\n")


# Plot the results
plot(ridge_cv, main = "Cross-Validated MSE for Ridge Regression", xvar = "lambda")
plot(lasso_cv, main = "Cross-Validated MSE for Lasso Regression", xvar = "lambda")


# Convert the 'Private' variable to a binary numeric variable
College$Private <- as.numeric(College$Private == "Yes")

# Extract the target variable 'Grad.Rate' and convert it to a numeric vector (y)
y_train <- as.numeric(sample_train$Grad.Rate)

# Extract the input features (independent variables) and convert them to a matrix (X)
X_train <- as.matrix(sample_train[, c( "Apps", "Accept", "Enroll", "Top10perc", "Top25perc", "PhD")])

# Fit the Ridge regression model using cross-validated lambda (alpha=0 for Ridge)
ridge_model <- glmnet(X_train, y_train, alpha = 0)

# Get the coefficients from the Ridge regression model
coefficients <- coef(ridge_model)

# Convert the sparse matrix to a regular matrix
coefficients_matrix <- as.matrix(coefficients)

# Create a data frame to represent the coefficients in a table format
coefficients_df <- data.frame(coefficients_matrix)

# Print the coefficients table
print(coefficients_df)

#
#RMSE OF TRAIN sample
# Predict the target variable 'Grad.Rate' on the training set
y_pred <- predict(ridge_model, newx = X_train)

# Calculate the root mean square error (RMSE)
rmse <- sqrt(mean((y_train - y_pred)^2))

# Print the RMSE
print(rmse)

#
#RMSE OF TEST SET 
#extracting variables from test sample 
X_test <- as.matrix(sample_test[, c( "Apps", "Accept", "Enroll", "Top10perc", "Top25perc", "PhD")])

y_test <- as.numeric(sample_test$Grad.Rate)

# Fit the Ridge regression model on the training set
ridge_model <- glmnet(X_train, y_train, alpha = 0)

# Make predictions on the test set
y_pred_test <- predict(ridge_model, newx = X_test)

# Calculate RMSE on the test set
rmse_test <- sqrt(mean((y_test - y_pred_test)^2))

print(rmse_test)

#LASSO
# Load required library
library(glmnet)
# Convert the 'Private' variable to a binary numeric variable
College$Private <- as.numeric(College$Private == "Yes")

# Extract the target variable 'Grad.Rate' and convert it to a numeric vector (y)
y_train1 <- as.numeric(sample_train$Grad.Rate)

# Extract the input features (independent variables) and convert them to a matrix (X)
X_train1 <- as.matrix(sample_train[, c("Apps", "Accept", "Enroll", "Top10perc", "Top25perc", "PhD")])

# Fit the Lasso regression model using cross-validated lambda (alpha=1 for Lasso)
lasso_model <- glmnet(X_train1, y_train1, alpha = 1)

# Get the coefficients from the Lasso regression model
coefficients1 <- coef(lasso_model)


# Convert the sparse matrix to a regular matrix
coefficients_matrix1 <- as.matrix(coefficients1)

# Create a data frame to represent the coefficients in a table format
coefficients_df1 <- data.frame(coefficients_matrix1)

# Print the coefficients table
print(coefficients_df1)


# Calculate the root mean square error (RMSE) of TRAIN SET

y_pred1 <- predict(lasso_model, newx = X_train1)

rmse1 <- sqrt(mean((y_train1 - y_pred1)^2))

# Print the RMSE
print(rmse1)
#calculating the RMSE for TEST set 
y_test1 <- as.numeric(sample_test$Grad.Rate)

# Extract the input features (independent variables) and convert them to a matrix (X)
X_test1 <- as.matrix(sample_test[, c("Apps", "Accept", "Enroll", "Top10perc", "Top25perc", "PhD")])


# Make predictions on the test set
y_pred_test1 <- predict(lasso_model, newx = X_test1)

# Calculate the root mean square error (RMSE) on the test set
rmse_test1 <- sqrt(mean((y_test1 - y_pred_test1)^2))

# Print the RMSE on the test set
print(rmse_test1)

# Load the MASS library for AIC calculation
library(MASS)

# Calculate the number of observations (n) and number of features (p)
n <- nrow(X_train)
p <- ncol(X_train)

# Calculate the log-likelihood for Ridge model
logLik_ridge <- -0.5 * n * log(2 * pi * mean((y_train - y_pred_ridge_train)^2)) - (n + 2) * (p + 1)

# Calculate the AIC for Ridge model
aic_ridge <- -2 * logLik_ridge + 2 * (p + 1)

# Calculate the BIC for Ridge model
bic_ridge <- -2 * logLik_ridge + (p + 1) * log(n)

# Calculate the log-likelihood for LASSO model
logLik_lasso <- -0.5 * n * log(2 * pi * mean((y_train - y_pred_lasso_train)^2)) - (n + 2) * (p + 1)

# Calculate the AIC for LASSO model
aic_lasso <- -2 * logLik_lasso + 2 * (p + 1)

# Calculate the BIC for LASSO model
bic_lasso <- -2 * logLik_lasso + (p + 1) * log(n)

# Print the AIC and BIC values for both models
print("AIC for Ridge Model:")
print(aic_ridge)

print("AIC for LASSO Model:")
print(aic_lasso)

print("BIC for Ridge Model:")
print(bic_ridge)

print("BIC for LASSO Model:")
print(bic_lasso)

