
# XGBOOST
# Akeem Yusuf
#2021-11-25

# Data Management

load("~/Stats_447/MLP/train (2).rda")
load("~/Stats_447/MLP/test (2).rda")
head(mytrain)
summary(mytrain)
dim(mytrain)
str(mytrain)
is.data.frame(mytrain)
table(mytrain$AsthmaStatus)
which(colnames(mytrain) == "AsthmaStatus")


# Create new dataset
trainingset <-  mytrain[c(-509,-7,-8)]
dim(trainingset)
head(trainingset)
hist(traindata$BMI)
hist(traindata$Age)


# Splitting the dataset into the Training set and Test set**
#install.packages('caTools')
library(caTools)
set.seed(2021)
split = sample.split(trainingset$AsthmaStatus, SplitRatio = 0.8)
training_set = subset(trainingset, split == TRUE)
test_set = subset(trainingset, split == FALSE)

# Transform BMI into log
training_set$BMI <- log(training_set$BMI)
test_set$BMI <- log(test_set$BMI)

# Fitting XGBoost to the Training set
# install.packages('xgboost')

library(xgboost)
classifier1 = xgboost(data = as.matrix(training_set[-6]), 
                     label = training_set$AsthmaStatus, nrounds = 200,
                     objective= "binary:logistic")

xgb.save(classifier, "xgboost.model")
 xgb.load('xgboost.model')

# Predicting the Test set/testset results
y_pred1 = predict(classifier1, newdata = as.matrix(mytest2)) 
y_pred1 = (y_pred1 >= 0.5)

y_pred2 = predict(classifier1, newdata = as.matrix(test_set[-6]))
y_pred2 = (y_pred2 >= 0.5)

# Making the Confusion Matrix
cm2 = table(as.matrix(test_set[, 6]), y_pred2)
cm2


#Analyzing the data
install.packages('vip')
install.packages("DiagrammeR")
install.packages("Ckmeans.1d.dp")

library(vip) 
library(DiagrammeR)
library(Ckmeans.1d.dp)


#Explianing machine learning model
# Variable importance plot

vip(classifier, num_features = 10)  # 10 is the default
importance_matrix <- xgb.importance(model = classifier1)
print(importance_matrix$Gain)
print(importance_matrix$Importance)
print(importance_matrix$Frequency)
sum(importance_matrix$Gain)  

#XGBoost importance plot
xgb.ggplot.importance(
  importance_matrix = importance_matrix,
  top_n = 20,
  measure = NULL,
  rel_to_first = FALSE,
  n_clusters = c(1:10),
)

#Model complexity
xgb.ggplot.deepness(
  model = classifier1,
  which = c("2x1", "max.depth", "med.depth", "med.weight")
)

# Shap plot
xgb.ggplot.shap.summary(
  data = as.matrix(training_set[-6]),
  shap_contrib = NULL,
  features = NULL,
  top_n = 10,
  model = classifier1,
  trees = NULL,
  target_class = NULL,
  approxcontrib = FALSE,
  subsample = NULL
)

#shap depenedecy plot
xgb.plot.shap(
  data= as.matrix(training_set[-6]),
  shap_contrib = NULL,
  features = NULL,
  top_n = c(8),
  model = classifier1,
  trees = NULL,
  target_class = NULL,
  approxcontrib = FALSE,
  subsample = NULL,
  n_col = 4,
  col = rgb(0, 0, 1, 0.2),
  pch = ".",
  discrete_n_uniq = 5,
  discrete_jitter = 0.01,
  ylab = "SHAP",
  plot_NA = TRUE,
  col_NA = rgb(0.7, 0, 1, 0.6),
  pch_NA = ".",
  pos_NA = 1.07,
  plot_loess = TRUE,
  col_loess = 2,
  span_loess = 0.5,
  which = c("1d", "2d"),
  plot = TRUE,
)

# Applying k-Fold Cross Validation
#install.packages('caret')
library(caret)

# Model Acurracy
library(caret)
folds = createFolds(training_set$AsthmaStatus, k = 5)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-6]), 
                       label = training_set$AsthmaStatus, nrounds = 200, objective = "binary:logistic")
  y_pred = predict(classifier1, newdata = as.matrix(test_fold[-6]))
  y_pred =(y_pred >= 0.5)
  cm = table(as.matrix(test_fold[, 6]), y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy

# Model Precision
folds = createFolds(training_set$AsthmaStatus, k = 5)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-6]), 
                       label = training_set$AsthmaStatus, nrounds = 200, objective = "binary:logistic")
  y_pred = predict(classifier1, newdata = as.matrix(test_fold[-6]))
  y_pred =(y_pred >= 0.5)
  cm = table(as.matrix(test_fold[, 6]), y_pred)
 precision = (cm[1,1])/(cm[1,1] + cm[1,2])
  return(precision)
})
precision = mean(as.numeric(cv))
precision

# Model Recall
folds = createFolds(training_set$AsthmaStatus, k = 5)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-6]), 
                       label = training_set$AsthmaStatus, nrounds = 200, objective = "binary:logistic")
  y_pred = predict(classifier1, newdata = as.matrix(test_fold[-6]))
  y_pred =(y_pred >= 0.5)
  cm = table(as.matrix(test_fold[, 6]), y_pred)
recall = (cm[1,1])/ (cm[1,1] + cm[2,1])
return(recall)
})

recall = mean(as.numeric(cv))
recall

# Model fscore
folds = createFolds(training_set$AsthmaStatus, k = 5)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-6]), 
                       label = training_set$AsthmaStatus, nrounds = 200, objective = "binary:logistic")
  y_pred = predict(classifier1, newdata = as.matrix(test_fold[-6]))
  y_pred =(y_pred >= 0.5)
  cm = table(as.matrix(test_fold[, 6]), y_pred)
fscore = 2 * ((precision * recall)/(precision + recall))
return(fscore)
})
fscore = mean(as.numeric(cv))
fscore



  
  
  
  
