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
traindata <-  mytrain[c(-509,-7,-8)]
head(traindata)
summary(traindata)
dim(traindata)
str(traindata)
is.data.frame(traindata)
table(traindata$AsthmaStatus)
which(colnames(traindata) == "AsthmaStatus")


# Splitting the dataset into the Training set and Test set

#install.packages('caTools')
library(caTools)
set.seed(2021)
split = sample.split(traindata$AsthmaStatus, SplitRatio = 0.80)
train_set = subset(traindata, split == TRUE)
test_set = subset(traindata, split == FALSE)

# Fitting XGBoost to the Training set

#install.packages('xgboost')
library(xgboost)
classifier = xgboost(data = as.matrix(train_set[-5]),
                     label = train_set$AsthmaStatus, 
                     nrounds = 10, objective = 'binary:logistic')


#Important featture
importance_matrix <- xgb.importance(model = classifier)
print(importance_matrix$Gain)
print(importance_matrix$Importance)
print(importance_matrix$Frequency)
xgb.plot.importance(importance_matrix = importance_matrix)

#Trees from XGBoost
xgb.plot.tree(model = classifier)


# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-5]))
y_pred = (y_pred >= 0.5)

# Making the Confusion Matrix
cm = table(test_set[, -5], y_pred)


mean(test_set[,5] == y_pred) #0.8649131
mean(test_set[,5] != y_pred) #0.1350869

table(y_pred)
# Applying k-Fold Cross Validation
#install.packages('caret')
#install.packages('dplyr')

library(caret)
library(dplyr)

folds = createFolds(train_set$AsthmaStatus, k = 10)
cv = lapply(folds, function(x) {
  training_fold = train_set[-x, ]
  test_fold = train_set[x, ]
  classifier = xgboost(data = as.matrix(train_set[-5]), 
                       label = train_set$AsthmaStatus, 
                       nrounds = 20)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-5]))
  y_pred =(y_pred >= 0.5)
  cm = table(as.matrix(test_fold[, 5]), y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy

#87.09 Accurracy

