library(e1071)
#read data
lettersData <- read.csv("./letters.csv")
View(lettersData)
#separate training and testing set
index <- 1:nrow(lettersData)
testindex <- sample(index, trunc(length(index)*30/100))
testset <- lettersData[testindex,]
trainset <- lettersData[-testindex,]
#train the model
model <- svm(formula = letter~., data = trainset, kernel = "radial", gamma = 0.1, cost = 10)
summary(model)
#make the predictions
prediction <- predict(model, testset[,-1])
#confussion matrix
tab <- table(pred = prediction, true = testset[,1])
#print cm
tab
#accuracy taken from the confusion matrix
classAgreement(tab)
