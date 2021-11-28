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

train_set$AsthmaStatus = as.factor(train_set$AsthmaStatus)
test_set$AsthmaStatus = as.factor(test_set$AsthmaStatus)

# Fitting XGBoost to the Training set

#install.packages('xgboost')
library("caret")

xgb_caret <- train(x = train_set[-5], y = train_set$AsthmaStatus,
                   method = 'xgbTree',
                                            verboseIter = TRUE),
                   tuneGrid = expand.grid(nrounds = c(500,1000, 1500), 
                   objective = 'reg:squarederror',
                   trControl = trainControl(method = 'repeatedcv',
                                            number = 3,
                                            repeats = 2,
                                          eta = c(0.01,0.05),
                                          max_depth = c(2,4,6),
                                          colsample_bytree = c(0.5,1),
                                          subsample = c(0.5,1),
                                          gamma = c(0, 50),
                                          min_child_weight = c(0, 20)))
confusionMatrix(data = train_set[-5], train_set$AsthmaStatus)




