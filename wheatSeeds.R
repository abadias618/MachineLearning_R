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

#WHEAT SEEDS DATASET
#from ICS 

##GBM implementation
seeds <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt", 
                    sep = "", 
                    stringsAsFactors = TRUE, 
                    col.names = c("area", "perim", "copact", "length", "width", "asymmetry", "length_groove", "class")) 
inTrain <- createDataPartition(y = seeds$class,
                               p = 150/210, 
                               list = FALSE)
#sets
gbm_train <- seeds[inTrain,]
gbm_test <- seeds[-inTrain,]
#fit
seeds_gbm <- gbm(class~.,
               data = gbm_train,
               n.trees = 2000,
               verbose = FALSE)
#predict
predictions <- predict(seeds_gbm, 
                       gbm_test,
                       n.trees=2000, 
                       type="response")
#transform predictions to compare to the test targets
predictions
gbm_test$class
predictions <- round(predictions, digits = 0)
#confusion matrix
levels(predictions)
levels(gbm_test$class)
tb <- table(predictions, gbm_test$class)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)

##KNN implementation
#normalization function
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
#normalized data
seeds_nor <- as.data.frame(lapply(seeds[,c(1,2,3,4,5,6,7)], nor))
#convert labels to numeric
seeds_nor[8] <- seeds[8]
#sets
knn_train <- seeds_nor[inTrain,1:7]
knn_test <- seeds_nor[-inTrain,1:7]

train_target <- as.factor(seeds_nor[inTrain,8])
test_target <- as.factor(seeds_nor[-inTrain,8])
#predictions
predict <- knn(knn_train,knn_test,cl=train_target,k=20)
predict <- as.factor(predict)
#confusion matrix
confusionMatrix(predict, test_target)

##Trees implementation
#sets
tree_train <- seeds[inTrain,]
tree_test <- seeds[-inTrain,]
colnames(seeds)
model <- rpart(formula = class ~ area + perim + copact + length + width + asymmetry + length_groove,
               data = tree_train, method = "class")
tree_test$predictions <- predict(object = model,
                                 newdata = tree_test,
                                 type = "class")
#make sure that they have the same levels
levels(tree_test$predictions)
levels(tree_test$class)
tree_test$class <- as.factor(tree_test$class)
#confusion matrix
confusionMatrix(data = tree_test$predictions,
                reference = tree_test$class)

##SVM implementation
#sets
svm_train <- seeds[inTrain,]
svm_test <- seeds[-inTrain,]
#fit
svm_model <- svm(formula = class~., data = svm_train, kernel = "radial", gamma = 0.1, cost = 10)
#predict
svm_prediction <- predict(svm_model, svm_test[,-8])
svm_prediction
#round to 1 or 0
svm_prediction <- round(svm_prediction, digits = 0)
svm_prediction <- as.data.frame(svm_prediction)
levels(svm_prediction)
levels(length(svm_test$class))
#confusion matrix
tb_svm <- table(svm_prediction, svm_test$class)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb_svm)

##Random Forest
#sets
sapply(seeds,class)

rf_train <- seeds[inTrain,]
rf_test <- seeds[-inTrain,]
#fit
rf <- randomForest(
  class ~ .,
  data = rf_train
)
#predict
pred <- predict(rf, newdata = rf_test[,-8])
pred <- round(pred, digits = 0)
#confusion matrix
tb_rf <- table(pred, rf_test$class)
#defined in previous section
accuracy(tb_rf)