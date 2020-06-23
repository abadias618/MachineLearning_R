library(e1071)
#library(MASS)
#read data
vowel <- read.csv("./vowel.csv")
#separate training and testing set
index <- 1:nrow(vowel)
testindex <- sample(index, trunc(length(index)*30/100))
testset <- vowel[testindex,]
trainset <- vowel[-testindex,]
#train the model
model <- svm(formula = Class~., data = trainset, kernel = "radial", gamma = 0.1, cost = 10)
summary(model)
#make the predictions
prediction <- predict(model, testset[,-13])
#confussion matrix
tab <- table(pred = prediction, true = testset[,13])
#print cm
tab
#accuracy taken from the confusion matrix
classAgreement(tab)
