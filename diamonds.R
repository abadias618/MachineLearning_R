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

#DIAMONDS DATASET
#dataset from ggplot2 

##GBM implementation
dms <- diamonds
inTrain <- createDataPartition(y = dms$cut,
                               p = 50000/53940, 
                               list = FALSE)
#sets
gbm_train <- dms[inTrain,]
gbm_test <- dms[-inTrain,]
#fit
dms_gbm <- gbm(cut~.-color-clarity,
                   data = gbm_train,
                   n.trees = 200,
                   verbose = FALSE)
#predict
predictions <- predict(dms_gbm, 
                    gbm_test,
                    n.trees=200, 
                    type="response")
#transform predictions to compare to the test targets
predictions <- as.data.frame(predictions)
colnames(predictions)[1] = "Fair"
colnames(predictions)[2] = "Good"
colnames(predictions)[3] = "Very Good"
colnames(predictions)[4] = "Premium"
colnames(predictions)[5] = "Ideal"
predictions <- as.factor(colnames(predictions)[max.col(predictions)])
#confusion matrix
confusionMatrix(predictions, gbm_test$cut)

##KNN implementation
#normalization function
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
#normalized data
dms_nor <- as.data.frame(lapply(dms[,c(1,2,5,6,7,8,9,10)], nor))
#convert labels to numeric
dms_nor[2] <- sapply(dms[2], as.numeric)
#sets
knn_train <- dms_nor[inTrain,]
knn_test <- dms_nor[-inTrain,]
train_target <- as.factor(dms_nor[inTrain,2])
test_target <- as.factor(dms_nor[-inTrain,2])
#predictions
predict <- knn(knn_train,knn_test,cl=train_target,k=20)
predict <- as.factor(predict)
#confusion matrix
confusionMatrix(predict, test_target)

##Trees implementation
#sets
tree_train <- dms[inTrain,]
tree_test <- dms[-inTrain,]
colnames(dms)
model <- rpart(formula = cut ~ carat + color + clarity + depth + table + price + x + y + z,
               data = tree_train, method = "class")
tree_test$predictions <- predict(object = model,
                                 newdata = tree_test,
                                 type = "class")
#make sure that they have the same levels
levels(tree_test$predictions)
levels(tree_test$cut)
#confusion matrix
confusionMatrix(data = tree_test$predictions,
                reference = tree_test$cut)

##SVM implementation
#sets
svm_train <- dms[inTrain,]
svm_test <- dms[-inTrain,]
#fit
svm_model <- svm(formula = cut~.-color-clarity, data = svm_train, kernel = "radial", gamma = 0.1, cost = 10)
#predict
svm_prediction <- predict(svm_model, svm_test[,-2])
svm_prediction
#round to 1 or 0
svm_prediction <- round(svm_prediction, digits = 0)
#confusion matrix
confusionMatrix(data = svm_prediction,
                reference = svm_test$cut)

##Random Forest
#sets
sapply(dms,class)

rf_train <- dms[inTrain,]
rf_test <- dms[-inTrain,]
#fit
rf <- randomForest(
  cut ~ .-color-clarity,
  data = rf_train
)
#predict
pred = predict(rf, newdata = rf_test[,-2])
#confusion matrix
confusionMatrix(data = pred,
                reference = rf_test$cut)