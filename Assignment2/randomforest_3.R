library(rpart.plot)
library(randomForest)
library(pROC)

carData=read.csv('D:/#Spring 2023/5580 - Text Mining/Assignment2/car.csv',header = TRUE)

trainIndex <- createDataPartition(carData$shouldBuy, p = 0.8, list = FALSE)
train <- carData[trainIndex,]
test <- carData[-trainIndex,]


train$shouldBuy <- factor(train$shouldBuy , levels=c("unacc", "acc", "good", "vgood"), 
                         ordered=TRUE)
test$shouldBuy <- factor(test$shouldBuy , levels=c("unacc", "acc", "good", "vgood"), 
                          ordered=TRUE)

set.seed(123)

# Perform hyperparameter tuning
# 1. mtry
#model <- tuneRF(train[,1:6], train[,7] , mtryStart = 2, ntree= 37)
#model <- tuneRF(train[,1:6], train[,7] , mtryStart = 2, ntree= 100)

model <- tuneRF(train[,1:6], train[,7] , mtryStart = 2)

# 2. nodesize
# Try for the best value of node size

"
# ntree =50
rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 1, ntree= 50) 

rfp <- predict(rf, newdata = test)
rfCM = table(rfp,test$shouldBuy)
rfProb =predict(rf, newdata = test,type="prob")
roc.multi <-multiclass.roc(test$shouldBuy, rfProb[,2])
rfCM
sum(diag(rfCM))/sum(rfCM)


rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 5, ntree= 50) 

rfp <- predict(rf, newdata = test)
rfCM = table(rfp,test$shouldBuy)
rfProb =predict(rf, newdata = test,type="prob")
roc.multi <-multiclass.roc(test$shouldBuy, rfProb[,2])
rfCM
sum(diag(rfCM))/sum(rfCM)


rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 10, ntree= 50) 

rfp <- predict(rf, newdata = test)
rfCM = table(rfp,test$shouldBuy)
rfProb =predict(rf, newdata = test,type="prob")
roc.multi <-multiclass.roc(test$shouldBuy, rfProb[,2])
rfCM
sum(diag(rfCM))/sum(rfCM)

# ntree =100
rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 1, ntree= 100) 

rfp <- predict(rf, newdata = test)
rfCM = table(rfp,test$shouldBuy)
rfProb =predict(rf, newdata = test,type="prob")
roc.multi <-multiclass.roc(test$shouldBuy, rfProb[,2])
rfCM
sum(diag(rfCM))/sum(rfCM)


rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 5, ntree= 100) 

rfp <- predict(rf, newdata = test)
rfCM = table(rfp,test$shouldBuy)
rfProb =predict(rf, newdata = test,type="prob")
roc.multi <-multiclass.roc(test$shouldBuy, rfProb[,2])
rfCM
sum(diag(rfCM))/sum(rfCM)


rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 10, ntree= 100) 

rfp <- predict(rf, newdata = test)
rfCM = table(rfp,test$shouldBuy)
rfProb =predict(rf, newdata = test,type="prob")
roc.multi <-multiclass.roc(test$shouldBuy, rfProb[,2])
rfCM
sum(diag(rfCM))/sum(rfCM)
"


rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 1) 

rfp <- predict(rf, newdata = test)
rfCM = table(rfp,test$shouldBuy)
rfProb =predict(rf, newdata = test,type="prob")
roc.multi <-multiclass.roc(test$shouldBuy, rfProb[,2])
rfCM
sum(diag(rfCM))/sum(rfCM)


rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 5) 

rfp <- predict(rf, newdata = test)
rfCM = table(rfp,test$shouldBuy)
rfProb =predict(rf, newdata = test,type="prob")
roc.multi <-multiclass.roc(test$shouldBuy, rfProb[,2])
rfCM
sum(diag(rfCM))/sum(rfCM)


rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 10) 

rfp <- predict(rf, newdata = test)
rfCM = table(rfp,test$shouldBuy)
rfProb =predict(rf, newdata = test,type="prob")
roc.multi <-multiclass.roc(test$shouldBuy, rfProb[,2])
rfCM
sum(diag(rfCM))/sum(rfCM)




# Setup up models
rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 1, ntree= 1500) #Model 1

#Calculate result different models

#Model 1
# Make predictions on the testing set
rfp_1 <- predict(rf_1, newdata = test)
rfCM = table(rfp_1,test$shouldBuy)
rfProb_1=predict(rf_1, newdata = test,type="prob")

# Calculate ROC
roc.multi <-multiclass.roc(test$shouldBuy, rfProb_1[,2])
roc <- roc.multi[['rocs']]
rs <- roc.multi[['rocs']]

#Print AUC
roc.multi$auc

#Print Confusion Matrix
rfCM

#Print Accuracy
sum(diag(rfCM))/sum(rfCM)

#Plot ROC
plot.roc(rs[[1]])
plot.roc(rs[[2]])
plot.roc(rs[[3]])
plot.roc(rs[[4]])
plot.roc(rs[[5]])
plot.roc(rs[[6]])

# Print Sensitivity(Recall) & Specificity                
confusionMatrix(rfp_1, test$shouldBuy)


#Model 2
# Make predictions on the testing set
rfp_2 <- predict(rf_2, newdata = test)
rfCM = table(rfp_2,test$shouldBuy)
rfProb_2=predict(rf_2, newdata = test,type="prob")

# Calculate ROC
roc.multi <-multiclass.roc(test$shouldBuy, rfProb_2[,2])
roc <- roc.multi[['rocs']]
rs <- roc.multi[['rocs']]

#Print AUC
roc.multi$auc

#Print Confusion Matrix
rfCM

#Print Accuracy
sum(diag(rfCM))/sum(rfCM)

#Plot ROC
plot.roc(rs[[1]])
plot.roc(rs[[2]])
plot.roc(rs[[3]])
plot.roc(rs[[4]])
plot.roc(rs[[5]])
plot.roc(rs[[6]])

# Print Sensitivity(Recall) & Specificity                
confusionMatrix(rfp_1, test$shouldBuy)

#Model 3
# Make predictions on the testing set
rfp_3 <- predict(rf_3, newdata = test)
rfCM = table(rfp_3,test$shouldBuy)
rfProb_3=predict(rf_3, newdata = test,type="prob")

# Calculate ROC
roc.multi <-multiclass.roc(test$shouldBuy, rfProb_3[,2])
roc <- roc.multi[['rocs']]
rs <- roc.multi[['rocs']]

#Print AUC
roc.multi$auc

#Print Confusion Matrix
rfCM

#Print Accuracy
sum(diag(rfCM))/sum(rfCM)

#Plot ROC
plot.roc(rs[[1]])
plot.roc(rs[[2]])
plot.roc(rs[[3]])
plot.roc(rs[[4]])
plot.roc(rs[[5]])
plot.roc(rs[[6]])

# Print Sensitivity(Recall) & Specificity                
confusionMatrix(rfp_1, test$shouldBuy)


