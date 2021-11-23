
# XGBOOST
# Akeem Yusuf


# Data Management

load("~/Stats_447/MLP/train (2).rda")
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



# Splitting the dataset into the Training set and Test set**

#install.packages('caTools')
library(caTools)
set.seed(2021)
split = sample.split(trainingset$AsthmaStatus, SplitRatio = 0.8)
training_set = subset(trainingset, split == TRUE)
test_set = subset(trainingset, split == FALSE)


# Fitting XGBoost to the Training set


# install.packages('xgboost')

library(xgboost)

classifier = xgboost(data = as.matrix(training_set[-6]), 
                     label = training_set$AsthmaStatus, nrounds = 200)

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-6]))
y_pred = (y_pred >= 0.5)

# Making the Confusion Matrix

cm = table(as.matrix(test_set[, 6]), y_pred)


mean(test_set[,6] == y_pred)
mean(test_set[,6] != y_pred)
print(cm)
summary(classifier$evaluation_log)


# Applying k-Fold Cross Validation
#install.packages('caret')

# Model Acurracy



library(caret)
folds = createFolds(training_set$AsthmaStatus, k = 5)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-6]), 
                       label = training_set$AsthmaStatus, nrounds = 200)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-6]))
  y_pred =(y_pred >= 0.5)
  cm = table(as.matrix(test_fold[, 6]), y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})



accuracy = mean(as.numeric(cv))
accuracy

#The Cross validation accuracy is 90 percent




# Model Precision

folds = createFolds(training_set$AsthmaStatus, k = 5)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-6]), 
                       label = training_set$AsthmaStatus, nrounds = 200)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-6]))
  y_pred =(y_pred >= 0.5)
  cm = table(as.matrix(test_fold[, 6]), y_pred)
 precision = (cm[1,1])/(cm[1,1] + cm[1,2])
  return(precision)
})




precision = mean(as.numeric(cv))
precision


# The Cross validation precision is 99 percent



# Model Recall

folds = createFolds(training_set$AsthmaStatus, k = 5)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-6]), 
                       label = training_set$AsthmaStatus, nrounds = 200)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-6]))
  y_pred =(y_pred >= 0.5)
  cm = table(as.matrix(test_fold[, 6]), y_pred)
recall = (cm[1,1])/ (cm[1,1] + cm[2,1])
return(recall)
})


recall = mean(as.numeric(cv))
recall

#The Cross validation recall is 90 percent



# Model fscore

folds = createFolds(training_set$AsthmaStatus, k = 5)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-6]), 
                       label = training_set$AsthmaStatus, nrounds = 200)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-6]))
  y_pred =(y_pred >= 0.5)
  cm = table(as.matrix(test_fold[, 6]), y_pred)
fscore = 2 * ((precision * recall)/(precision + recall))
return(fscore)
})


fscore = mean(as.numeric(cv))
fscore

#The Cross validation fcore is 95 percent
  
  
  
  
