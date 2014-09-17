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
dim(train2)
dim(training)
# split train and test

# run the model
set.seed(888)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), n.trees = (1:30)*50, shrinkage = 0.1)
fit<- train(classe~., data=training, method = 'gbm', trControl=fitControl, tuneGrid = gbmGrid)
pred <- predict(fit, training)
result <- confusionMatrix(pred, training$classe)