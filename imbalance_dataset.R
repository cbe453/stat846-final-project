# Akeem Yusuf
#2021-11-25

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

#Convert to factor
training_set$AsthmaStatus <- as.factor(training_set$AsthmaStatus)
training_set$AsthmaStatus <- as.factor(test_set$AsthmaStatus)

str(training_set)

#Imbalanced dataset for train?
table(training_set$AsthmaStatus) #Yes. will need to resample
train_up_sample <- upSample(x=training_set, y = training_set$AsthmaStatus)
str(train_up_sample)
table(train_up_sample$AsthmaStatus)

train_up_sample$AsthmaStatus <- as.numeric(as.character(train_up_sample$AsthmaStatus))
str(train_up_sample)
table(train_up_sample$AsthmaStatus)
training_set1 <- train_up_sample[c(-507)]
str(training_set1)
table(training_set1$AsthmaStatus)
