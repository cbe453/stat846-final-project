# Qi Zhao and Connor Burbridge 
rm(list = ls()) 

# reading data. Put the test.rda and train.rad in the same folder with the R file. 
traindata <- get(load("train.rda")) 
head(traindata) 
testdata <- get(load("test.rda")) 
head(testdata,2) 

# create a new data set by deleting  COPD, Cancer, Patid from traindata 
mytrain=traindata[-6][-6][-6][-506] 
mytrain$case=traindata$AsthmaStatus 
mytrain=data.frame(mytrain) 

# check the new data set 
names(mytrain) 

## Feature selection run by Qi using t-test 
myresult=rep(NA,505) 
for (ii in 1:505) { 
  myresult[ii]=t.test(mytrain[,ii]~mytrain$case)$p.value 
} 
sig=which(myresult<(0.05/505)) 

# export the important predictors based on the t-test result 
colnames(mytrain[,sig]) 

# create a new data set according to the important predictors 
mydata=mytrain[,sig] 
mydata$case=mytrain$case

# Feature selection run by Qi using Lasso 
library(Matrix) 
library(glmnet) 
lasso=glmnet(mytrain[,1:505],mytrain[,506],alpha = 1) 

#the cross-validation error as a function of 𝜆 
cv_lasso=cv.glmnet(as.matrix(mytrain[,1:505]),mytrain[,506],alpha=1) 

#find the best model 
bestlasso=cv_lasso$lambda.min 

#Which gene type has a coefficient based on lasso 
siglassoc = predict(lasso, s=bestlasso, type = 'coefficients')@i 
siglassocname = predict(lasso, s=bestlasso, type = 'coefficients') 
siglassocname 
siglassoc 

# export the important predictors based on Lasso 
colnames(mytrain[,siglassoc]) 

# identify overalpping features between t-test and Lasso 
bothsig=intersect(siglassoc,sig) 
bothsig 

# # split the train data set to two parts: train data (80%) and test data (20%) 
library(caret) 

# sig for t-test, siglassoc for Lasso predictors, bothsig for overlapping subset from both 
mydata=mytrain[, bothsig]   
mydata 
mydata$case=mytrain$case 
set.seed(2021) 
train <- createDataPartition(mydata$case, p = .80,  
                             list = FALSE,  
                             times = 1) 
mytr.train <- mydata[train, ] 
mytr.test <- mydata[-train, ] 

# LDA run by Qi based on lasso 
library("PRROC") 
library(MASS) 
lda.fit <- lda(case~., mytr.train) 
lda.fit 
lda.pred <- predict(lda.fit, mytr.test) 
names(lda.pred) 
lda.class <- lda.pred$class 

#Confusion Matrix  
table(lda.class, mytr.test$case) 
ldaMis = mean(lda.class != mytr.test$case) 
ldaCM = confusionMatrix(lda.class, as.factor(mytr.test$case)) 

# QDA run by Qi 
set.seed(2021) 
qda.fit <- qda(case~., mytr.train) 
qda.fit 

# Fitted value 
qda.pred <- predict(qda.fit, mytr.test) 
typeof(qda.pred) 
qda.class <- qda.pred$class 
qda.class 

#Confusion Matrix For Training Error  
table(qda.class, mytr.test$case) 
qdaMis = mean(qda.class != mytr.test$case) 
qdaCM = confusionMatrix(qda.class, as.factor(mytr.test$case)) 

# Elastic Net run by Connor
# Elastic net requires a factor as input outcomes
trainFactor = as.factor(mytr.train$case)

# Prepare and run elastic net
cv_5 = trainControl(method = "cv", number = 5)
elasticNet = train(as.factor(case) ~ ., data = mytr.train, method = "glmnet",
  trControl = cv_5, tuneLength = 20)

# Summary
elasticNet

# Identify best model
best = order(elasticNet$results$Accuracy, decreasing=TRUE)[1]
bestModel = elasticNet$results[best,]
bestModel

# Method for error used in class
predict = predict(elasticNet, newdata = mytr.test)
enMis = mean(predict != mytr.test$case)
enCM = confusionMatrix(predict, as.factor(mytr.test$case))

# Apply model to testdata.rda
typeof(testdata)
mytest = as.data.frame(testdata)
testData = mytest[,siglassoc]
testPredict = predict(elasticNet, newdate = testData)

# Random Forest
myrfdata=mytrain[, bothsig]
myrfdata$case=mytrain$case
set.seed(2021)
rftrain <- createDataPartition(mydata$case, p = .80, 
                             list = FALSE, 
                             times = 1)
myrftr.train <- myrfdata[rftrain, ]
myrftr.test <- myrfdata[-rftrain, ]


library("randomForest")

ncol(myrftr.train)
observations = as.matrix(myrftr.train[,1:28])
forestModel = randomForest(as.matrix(myrftr.train[,1:28]), as.factor(myrftr.train$case), ntree = 250)
forestModel
forestModel.pred = predict(forestModel, newdata = myrftr.test[,1:28])
forestModel.pred

rfMis = mean(forestModel.pred != mytr.test$case)
rfCM = confusionMatrix(forestModel.pred, as.factor(myrftr.test$case))[2]

library("vip")
vip(lda.fit, num_features = 10)
vip(qda.fit, num_features = 10)
vip(elasticNet, num_features = 20)
vip(randomForest, num_features = 10)

ldaCM[2]
ldaMis
qdaCM[2]
qdaMis
enCM[2]
enMis
rfCM
rfMis
