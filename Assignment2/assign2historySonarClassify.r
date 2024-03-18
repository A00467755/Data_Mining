sonarData=read.csv("D:/#Spring 2023/5580 - Text Mining/Assignment2/sonar.all-data",header=F)
summary(sonarData)
x=sonarData[,1:60]
y=sonarData[,61]

# Try rpart
library(rpart)
treeSonar = rpart(V61~.,data=sonarData,method="class",control=rpart.control(minsplit=10))
treeSonar
predSonar=predict(treeSonar,newdata=sonarData,type="class")
head(predSonar)
treeCM=table(sonarData[,61],predSonar)
treeCM
sum(diag(treeCM))/sum(treeCM)
library(pROC)
predSonarProb=predict(treeSonar,newdata=sonarData,type="prob")
roc(sonarData[,61],predSonarProb[,2])
plot(roc(sonarData[,61],predSonarProb[,2]))


#Try randomForest


y<- factor(y , levels=c("R","M"), 
                         ordered=TRUE)
library(randomForest)
rf=randomForest(x,y)
rfp=predict(rf,x)
rfCM=table(rfp,y)
rfCM
sum(diag(rfCM))/sum(rfCM)
rfProb=predict(rf,x,type="prob")
a= roc(sonarData[,61],rfProb[,2])
plot(roc(sonarData[,61],rfProb[,2]))

rf=randomForest(x,y,nodesize=50)
rfp=predict(rf,x)
rfCM=table(rfp,y)
rfCM
sum(diag(rfCM))/sum(rfCM)
rfProb=predict(rf,x,type="prob")

# sensitivity is the ability to correctly identify a given class
# specificity is the ability to correctly identify those who are in different class(es)
# Here, we are using the second column that means class 'M'

# If we have more than two classes you need to use multiclass.roc
roc(sonarData[,61],rfProb[,2])

plot(roc(sonarData[,61],rfProb[,2]))
# If you have more than two classes, you can run this separately for each class
# plot(roc(sonarData[,61],rfProb[,1]))
# plot(roc(sonarData[,61],rfProb[,2]))
# plot(roc(sonarData[,61],rfProb[,3]))
# plot(roc(sonarData[,61],rfProb[,4]))

rf=randomForest(x,y,nodesize=10)
rfp=predict(rf,x)
rfCM=table(rfp,y)
rfCM
sum(diag(rfCM))/sum(rfCM)
rfProb=predict(rf,x,type="prob")
roc(sonarData[,61],rfProb[,2])
plot(roc(sonarData[,61],rfProb[,2]))



# Use caret package for 10 fold cross validation
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
rf_default <- train(x,y, method="rf", metric=metric, trControl=control)
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
rf_default <- train(x,y, method="rf", metric=metric, trControl=control)
print(rf_default)
varImp(rf_default)
ggplot(varImp(rf_default))
