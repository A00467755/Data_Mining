# install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))

library(caret)
library(ggplot2,lattice)

carData=read.csv("D:/#Spring 2023/5580 - Text Mining/Assignment2/car.csv",header=T)

x=carData[,1:6]
y=carData[,7]

yy = factor(y)

print (y)
print (yy)

# Config
seed <- 7
metric <- "Accuracy"
set.seed(seed)

# Preprocess categorical variable to number variables
dummies_model <- dummyVars(shouldBuy~.,data=carData)

trainData_mat <- predict(dummies_model, newdata = carData)

# # Convert to dataframe
x <- data.frame(trainData_mat)


control <- trainControl(method="repeatedcv", number=10, repeats=3)
rf_default <- train(x,yy, method="rf", metric=metric, trControl=control)

print(rf_default)

varImp(rf_default)
ggplot(varImp(rf_default))


predictions <- predict(rf_default, newdata = x)
confusionMatrix(predictions,yy)



"""
number	
Either the number of folds or number of resampling iterations

repeats	
For repeated k-fold cross-validation only: the number of complete sets of folds to compute

"""
