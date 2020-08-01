library(readr)
library(caret)
library(gbm)
library(ggplot2)
library(class)
library(tree)
library(rpart)
library(e1071)
library(randomForest)
library(caTools)
#TITANIC
# dataset from https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/problem12.html

titanic <- read_csv("https://web.stanford.edu/class/archive/cs/cs109/cs109.1166/stuff/titanic.csv")
inTrain <- createDataPartition(y = titanic$Survived, 
                              p = 750/887, 
                              list = FALSE)
Train <- titanic[inTrain,]
Test <- titanic[-inTrain,]
#GBM implementation
simpleGbm <-gbm(Survived~.-Name-Sex,
                distribution = "bernoulli",
                data=Train,
                n.trees = 2000,
                interaction.depth = 4,
                shrinkage = 0.01,
                cv.folds = 3)
print(simpleGbm)
summary(simpleGbm)

ntree_opt_cv <- gbm.perf(simpleGbm, method = "cv")
Predictions <- predict(object =  simpleGbm,
                       newdata = Test,
                       n.trees = ntree_opt_cv,
                       type = "response")
PredictionBinaries <- as.factor(ifelse(Predictions > 0.7,1,0))
Test$Survived <- as.factor(Test$Survived)
confusionMatrix(PredictionBinaries, Test$Survived)

#KNN implementation
##normalization function
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
##normalized data
titanic_nor <- as.data.frame(lapply(titanic[,c(1,2,5,6,7,8)], nor))

##sets
knn_train <- titanic_nor[inTrain,]
knn_test <- titanic_nor[-inTrain,]

train_target <- as.factor(titanic_nor[inTrain,1])
test_target <- as.factor(titanic_nor[-inTrain,1])
predict <- knn(knn_train,knn_test,cl=train_target,k=20)
#confusion matrix
tb <- table(predict,test_target)
tb
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)

##Trees implementation
#sets
tree_train <- titanic[inTrain,]
tree_test <- titanic[-inTrain,]
colnames(titanic)
model <- rpart(formula = Survived ~ Pclass + Sex + Age + `Siblings/Spouses Aboard` + `Parents/Children Aboard` + Fare,
               data = tree_train, method = "class")
tree_test$predictions <- predict(object = model,
                                 newdata = tree_test,
                                 type = "class")
#make sure that they have the same levels
levels(tree_test$predictions)
levels(tree_test$Survived)
tree_test$Survived <- as.factor(tree_test$Survived)
confusionMatrix(data = tree_test$predictions,
                reference = tree_test$Survived)

##SVM implementation
#sets
svm_train <- titanic[inTrain,]
svm_test <- titanic[-inTrain,]

svm_model <- svm(formula = Survived~.-Name-Sex, data = svm_train, kernel = "radial", gamma = 0.1, cost = 10)
svm_prediction <- predict(svm_model, svm_test[,-1])
#round to 1 or 0
svm_prediction <- round(svm_prediction, digits = 0)
#confusion matrix
svm_test$Survived <- as.factor(svm_test$Survived)
svm_prediction <- as.factor(svm_prediction)
confusionMatrix(data = svm_prediction,
                reference = svm_test$Survived)

##Random Forest

#sets
sapply(titanic,class)

rf_train <- titanic[inTrain,]
rf_test <- titanic[-inTrain,]
#make our character columns factors
rf_train <- transform(
  rf_train,
  Sex=as.factor(Sex),
  Survived=as.factor(Survived)
)
rf_test <- transform(
  rf_test,
  Sex=as.factor(Sex),
  Survived=as.factor(Survived)
)
#without this it wouldn't recognize all the columns
names(rf_train) <- make.names(names(rf_train))
names(rf_test) <- make.names(names(rf_test))
rf <- randomForest(
  Survived ~ .-Name,
  data = rf_train
)
pred = predict(rf, newdata = rf_test[,-1])
#confusion matrix
rf_test$Survived <- as.factor(rf_test$Survived)
confusionMatrix(data = pred,
                reference = rf_test$Survived)