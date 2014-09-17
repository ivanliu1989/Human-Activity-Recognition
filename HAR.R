# setup and load data
setwd("/Users/ivan/Work_directory/Human-Activity-Recognition/")
library(caret)
train <- read.csv('data/pml-training.csv', stringsAsFactor=F)
# explore data
summary(train)
names(train)
str(train)
dim(train)
# NA percents of each variable
na_train <- sapply(1:length(names(train)), function(i, na_base=c()){
    na_col <- mean(is.na(train[i]))
    na_base <- c(na_base, na_col)})
names(na_train) <- names(train)
na_train <- sort(na_train,decreasing = T) 
train2 <- na.omit(train)
# non zero variable
nzv <- nearZeroVar(train2,saveMetrics = T)
nzv
training <- train2[,-nzv]
dim(train2)
head(training)
# split train and test