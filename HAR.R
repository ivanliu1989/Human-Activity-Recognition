setwd("/Users/ivan/Work_directory/Human-Activity-Recognition/")
train <- read.csv('data/pml-training.csv')
summary(train)
names(train)
a <- sapply(1:length(names(train)), function(i, index=c()) {
    ind <- 'class' %in% names(train)[i]
    index <- c(index, ind)
}
)
a
