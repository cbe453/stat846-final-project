#Selecting Predictors model 

# Data Management
head(mytrain)
summary(mytrain)
dim(mytrain)
str(mytrain)
is.data.frame(mytrain)
table(mytrain$AsthmaStatus)
which(colnames(mytrain) == "AsthmaStatus")

# Create new dataset
trainingset <-  mytrain[c(-509)]
dim(trainingset)
head(trainingset)


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(2021)
split = sample.split(trainingset$AsthmaStatus, SplitRatio = 0.8)
training_set = subset(trainingset, split == TRUE)
test_set = subset(trainingset, split == FALSE)

# Fitting XGBoost to the Training set
# install.packages('xgboost')
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-6]), label = training_set$AsthmaStatus, nrounds = 20)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-6]))
y_pred = (y_pred >= 0.5)

y# Making the Confusion Matrix
cm = table(test_set[, -6], y_pred)

mean((test_set[,6] - y_pred)^2) #mse
caret::MAE(test_set[,6], y_pred) #mae
caret::RMSE(test_set[,6], y_pred) #rmse

mean(test_set[,6] == y_pred)
mean(test_set[,6] != y_pred)

table(y_pred)
# Applying k-Fold Cross Validation
#install.packages('caret')
#install.packages('dplyr')

library(caret)
folds = createFolds(training_set$AsthmaStatus, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-6]), 
                       label = training_set$AsthmaStatus, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-6]))
  y_pred =(y_pred >= 0.5)
  cm = table(as.matrix(test_fold[, 6]), y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy


