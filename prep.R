
####DATA PREPROPECESSING

# Data Management
str(mytest2[,1:8])
dim(mytest2[,1:8])
head(mytest2[,1:10])
mytest2$patid
pairs(mytest2[,1:5])

str(mytrain[,1:10])
dim(mytrain)
head(mytrain[,1:10])
mytrain$patid
cor.test(mytrain[,1:8])

install.packages("DiagrammeR")
library(DiagrammeR)

# Data Pre processing
traindata <-  mytrain[c(-509,-7,-8,)] 
head(traindata)
summary(traindata)
dim(traindata)
str(traindata)
is.data.frame(traindata)
table(traindata$AsthmaStatus)
which(colnames(traindata) == "AsthmaStatus")

# Splitting the dataset into the Training set and Test set

install.packages('caTools')
library(caTools)
set.seed(2021)
split = sample.split(traindata$AsthmaStatus, SplitRatio = 0.80)
train_set = subset(traindata, split == TRUE)
test_set = subset(traindata, split == FALSE)


