library(rpart.plot)
library(caret)
library(randomForest)
library(pROC)
#library(multiROC)
#install.packages("multiROC")

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
model <- tuneRF(train[,1:6], train[,7] , mtryStart = 2)

# 2. nodesize
# Try for the best value of node size

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


# Build the optimized model
rf=randomForest(shouldBuy~price+maintenance+doors+seats+storage+safety,data=train, mtry =6, nodesize= 1)

# Make predictions on the testing set
rfp <- predict(rf, newdata = test)
rfCM = table(rfp,test$shouldBuy)
rfProb=predict(rf, newdata = test,type="prob")

# plot ROC for different classes
predictions <- as.data.frame(predict(rf, newdata = test, type = "prob"))

predictions$predict <- names(predictions)[1:4][apply(predictions[,1:4], 1, which.max)]
predictions$observed <- test$shouldBuy
head(predictions)

roc.unacc <- roc(ifelse(predictions$observed=="unacc", "unacc", "non-unacc"), as.numeric(predictions$unacc))
roc.acc <- roc(ifelse(predictions$observed=="acc", "acc", "non-acc"), as.numeric(predictions$acc))
roc.good <- roc(ifelse(predictions$observed=="good", "good", "non-good"), as.numeric(predictions$good))
roc.vgood <- roc(ifelse(predictions$observed=="vgood", "vgood", "non-vgood"), as.numeric(predictions$vgood))

plot(roc.unacc)
plot(roc.acc)
plot(roc.good)
plot(roc.vgood)


#Print Confusion Matrix & stat
confusionMatrix(rfp, test$shouldBuy)


#Evaluate variable importance
importance(rf)
varImpPlot(rf)

#Print AUC
rfProb <- as.numeric(predict(rf, newdata = test, type = 'response'))
roc.multi <- multiclass.roc(test$shouldBuy, rfProb)
roc.multi$auc


# Calculate ROC

#old code
rfProb=predict(rf, newdata = test,type="prob")
roc.multi <-multiclass.roc(test$shouldBuy, rfProb[,2])
rs <- roc.multi[['rocs']]



plot.roc(rs[[1]])
plot.roc(rs[[2]])
plot.roc(rs[[3]])
plot.roc(rs[[4]])
plot.roc(rs[[5]])
plot.roc(rs[[6]])

# New code
rfProb <- as.numeric(predict(rf, newdata = test, type = 'response'))
roc.multi <- multiclass.roc(test$shouldBuy, rfProb)
rs <- roc.multi[['rocs']]

roc_res <- multi_roc(test)
roc_res_df <- plot_roc_data(roc_res)

plot_roc_data(rs )

#Plot ROC
plot.roc(rs[[1]])
plot.roc(rs[[2]])
plot.roc(rs[[3]])
plot.roc(rs[[4]])
plot.roc(rs[[5]])
plot.roc(rs[[6]])










roc_curve <- multiclass.roc(test$shouldBuy, rfProb[,2])
auc <- as.numeric(auc(roc_curve))

# Plot ROC curve
plot(roc_curve, print.auc = TRUE, main = "Multiclass ROC Curve")

# Add legend
legend("bottomright", colnames(roc_curve$roc), col = roc_curve$colors, lty = 1)



#multi roc in a plot
"

roc.unacc <- roc(ifelse(predictions$observed=="unacc", "unacc", "non-unacc"), as.numeric(predictions$unacc))
plot(roc.unacc, col = "gray60")

# others
roc.acc <- roc(ifelse(predictions$observed=="acc", "acc", "non-acc"), as.numeric(predictions$acc))
roc.good <- roc(ifelse(predictions$observed=="good", "good", "non-good"), as.numeric(predictions$good))
roc.vgood <- roc(ifelse(predictions$observed=="vgood", "vgood", "non-vgood"), as.numeric(predictions$vgood))
lines(roc.acc, col = "blue")
lines(roc.good, col = "red")
lines(roc.vgood, col = "green")
"
"
