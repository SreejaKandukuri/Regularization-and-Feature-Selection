install.packages("ISLR")
install.packages("caret")
library(ISLR)
library(ggplot2)
summary(College)
data(College)

# View the structure of the dataset (columns, data types)
str(College)

# View the summary statistics of the numeric variables
summary(College)

#histogram of faculty with PhD
ggplot(College, aes(x = PhD)) +
  geom_histogram(color = "indianred") +
  labs(title = "Histogram of Faculty with PhD", x = "Pct of Faculty with PhD", y = "Frequency")

#Private or Public Colleges vs number of applications received
ggplot(College, aes(x = Private, y = Apps)) +
  geom_boxplot(fill = "olivedrab") +
  labs(title = "Box Plot of Applications Received by Private/ Public Colleges", x = "Private/Not", y = "Applications Received")

#Graduation Rate bar plot 
ggplot(College, aes(x = Grad.Rate)) +
  geom_bar(fill = "plum2") +
  labs(title = "Bar Plot of Graduation Rate of Colleges", x = "Graduation Rate", y = "Count")

#Create Train and Test set - random sample (70/30 split)
trainIndex = sort(sample(x = nrow(College), size = nrow(College) * 0.7))
sample_train = College[trainIndex,]
sample_test = College[-trainIndex,]


# Create Train and Test set - maintain % of event rate (70/30 split)
library(caret)
set.seed(3456)
trainIndex = createDataPartition(College$Private, p = 0.7, list = FALSE, times = 1)
caret_train = College[ trainIndex,]
caret_test = College[-trainIndex,]

# regression model 
logisticmodel = glm(Private ~ Accept + PhD + Grad.Rate,
                      data = College,
                    family = binomial(link = "logit"))

summary(logisticmodel)


# Predict using the logistic regression model on the train set
sample_train$predicted_private = predict(logistic_model, newdata = sample_train, type = "response")
sample_train$predicted_private = ifelse(sample_train$predicted_private >= 0.5, "Yes", "No")

confusion_matrix = table(sample_train$Private, sample_train$predicted_private)

print(confusion_matrix)

# Predict using the logistic regression model on the train set
sample_train$predicted_private = predict(logisticmodel, newdata = sample_train, type = "response")
sample_train$predicted_private = ifelse(sample_train$predicted_private >= 0.5, "Yes", "No")

confusion_matrix = table(Actual = sample_train$Private, Predicted = sample_train$predicted_private)

print(confusion_matrix)

accuracy = (confusion_matrix[1,1] + confusion_matrix[2,2]) / sum(confusion_matrix)
precision = confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[1,2])
recall = confusion_matrix[2,2] / (confusion_matrix[2,2] + confusion_matrix[2,1])
specificity = confusion_matrix[1,1] / (confusion_matrix[1,1] + confusion_matrix[1,2])

# Step 5: Print the metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")
#
#
# Predict using the logistic regression model on the test set
sample_test$predicted_private = predict(logisticmodel, newdata = sample_test, type = "response")
sample_test$predicted_private = ifelse(sample_test$predicted_private >= 0.5, "Yes", "No")

confusion_matrix_test = table(Actual = sample_test$Private, Predicted = sample_test$predicted_private)

print(confusion_matrix_test)

accuracy = (confusion_matrix_test[1,1] + confusion_matrix_test[2,2]) / sum(confusion_matrix_test)
precision = confusion_matrix_test[2,2] / (confusion_matrix_test[2,2] + confusion_matrix_test[1,2])
recall = confusion_matrix_test[2,2] / (confusion_matrix_test[2,2] + confusion_matrix_test[2,1])
specificity = confusion_matrix_test[1,1] / (confusion_matrix_test[1,1] + confusion_matrix_test[1,2])

# Step 5: Print the metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("Specificity:", specificity, "\n")

#ROC CURVE
library(pROC)

#  Predict probabilities using the logistic regression model on the test set
sample_test$predicted_prob = predict(logisticmodel, newdata = sample_test, type = "response")

roc_curve_test = roc(sample_test$Private_numeric, sample_test$predicted_prob)

plot(roc_curve_test, col = "blue", main = "ROC Curve of Logistic Regression Model (Test Set)", lwd = 2)
lines(x = c(0, 1), y = c(0, 1), col = "gray", lty = 2, lwd = 2)

auc_value_test = auc(roc_curve_test)

cat("AUC (Test Set):", auc_value_test, "\n")