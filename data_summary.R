#Data analysis
#Akeem Yusuf

library(ggplot2)
library(tidyr)
library(caret)
library(MASS)
library(class)
library(glmnet)
library(leaps)
library(e1071)
library(vcd)
library(knitr)

load("~/Stats_447/MLP/train (2).rda")
summary(mytrain$Sex)
summary(mytrain$BMI)
summary(mytrain$Age)
summary(mytrain$FEV1Z)




# Data visualizatio
par(mfrow=c(2,2))
boxplot(Age ~ AsthmaStatus, data = mytrain, main = "Age")
boxplot(BMI ~ AsthmaStatus, data = mytrain, main = "BMI")
boxplot(FEV1Z ~ AsthmaStatus, data = mytrain, main = "FEV1Z")



par(mfrow=c(2,2))
mosaic( AsthmaStatus ~ Sex, data = mytrain,
highlighting_fill=c("lightblue", "pink"))
mosaic( AsthmaStatus ~ Smoking_Status, data = mytrain,
highlighting_fill=c("lightgreen", "orange"))





