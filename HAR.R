# setup and load data
setwd("/Users/ivan/Work_directory/Human-Activity-Recognition/")
library(caret)
train <- read.table('data/pml-training.csv', stringsAsFactor=F, sep=','
                    ,header = T,na.strings = c("NA",""))
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
    # na_train <- sort(na_train,decreasing = T)
na_index <- na_train < .5
train2 <- train[,na_index]
# non zero variable
nzv <- nearZeroVar(train2,saveMetrics = F)
training <- train2[,-nzv]
dim(training)
# split train and test

# run the model
set.seed(888)
# classProbs=TRUE, savePred=T, 
fitControl <- trainControl(method = "cv",number = 10)
gbmGrid <-  expand.grid(interaction.depth = 5, n.trees = 300, shrinkage = 0.1)
fit<- train(as.factor(classe)~., data=training, method = 'gbm', trControl=fitControl, tuneGrid = gbmGrid)
pred <- predict(fit, training)
result <- confusionMatrix(pred, training$classe)

# test on test dataset
test <- read.table('data/pml-testing.csv', stringsAsFactor=F, sep=','
                    ,header = T,na.strings = c("NA",""))
test2 <- test[,na_index]
testing <- test2[,-nzv]
pred_test <- predict(fit, newdata=test)
