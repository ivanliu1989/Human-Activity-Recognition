# setup and load data
setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Human-Activity-Recognition")
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
train2 <- train2[,-c(1, 2)]
# non zero variable
nzv <- nearZeroVar(train2,saveMetrics = F)
training <- train2[,-nzv]
dim(training)
# split train and test
index <- createDataPartition(training$classe, p=0.8, list=F)
train_md <- training[index,]
test_md <- training[-index,]
# run the model
set.seed(888)
# classProbs=TRUE, savePred=T, 
fitControl <- trainControl(method = "cv",number = 10)
# fitControl <- trainControl(method = "none")
gbmGrid <-  expand.grid(interaction.depth = 5, n.trees = 300, shrinkage = 0.1)
fit<- train(as.factor(classe)~., data=train_md, method = 'gbm', trControl=fitControl, tuneGrid = gbmGrid)
gbmImp <- varImp(fit,scale=F)
plot(gbmImp, top=10)

# Out of sample error
pred <- predict(fit, train_md)
result <- confusionMatrix(pred, train_md$classe)
pred_t <- predict(fit, test_md)
result_t <- confusionMatrix(pred_t, test_md$classe)
# test on test dataset
test <- read.table('data/pml-testing.csv', stringsAsFactor=F, sep=','
                    ,header = T,na.strings = c("NA",""))
# test2 <- test[,na_index]
# testing <- test2[,-nzv]
pred_test <- predict(fit, newdata=test)
results <- data.frame(test$problem_id, test$user_name, pred_test)
names(results) <- c('Problem_id','User_name','Classe')
results
