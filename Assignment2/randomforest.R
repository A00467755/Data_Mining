
library(rpart.plot)
library(pROC)

carData=read.csv('D:/#Spring 2023/5580 - Text Mining/Assignment2/car.csv',header = TRUE)

x=carData[,1:6]
y=carData[,7]


library(randomForest)

y <- factor(y, levels=c("unacc", "acc", "good", "vgood"), 
                         ordered=TRUE)

# Trial for given code #1
rf=randomForest(x,y)
rfp=predict(rf,x)
rfCM=table(rfp,y)
rfCM
sum(diag(rfCM))/sum(rfCM)
rfProb=predict(rf,x,type="prob")

roc.multi <-multiclass.roc(carData[,7], rfProb[,2])
roc <- roc.multi[['rocs']]

rs <- roc.multi[['rocs']]


plot.roc(rs[[1]])
plot.roc(rs[[2]])
plot.roc(rs[[3]])
plot.roc(rs[[4]])
plot.roc(rs[[5]])
plot.roc(rs[[6]])

# Trial for given code #2
rf_2=randomForest(x,y,nodesize=5)
rfp=predict(rf_2,x)
rfCM=table(rfp,y)
rfCM
sum(diag(rfCM))/sum(rfCM)
rfProb=predict(rf,x,type="prob")
multiclass.roc(carData[,7],rfProb[,2])
plot(roc(carData[,7],rfProb[,2]))

######################
# Use tuneRF



tune_grid <- data.frame(mtry = c(2,3,4),splitrule = c("gini"))


# Perform random search for hyperparameter tuning

set.seed(123)
model <- tuneRF(x, y, mtryStart = 2, ntreeTry = 50, importance = TRUE, ntreeWV = NULL, doBest = TRUE, replace = FALSE, tunecontrol = tuneControl(random = TRUE), splitrule = tune_grid$splitrule, trace = TRUE)


######

tune_grid <- data.frame(mtry = c(2,3,4),splitrule = c("gini","extratrees","class"))

#model <- tuneRF(x, y, mtryStart = 2, ntreeTry = 50, stepFactor = 1.5, improve = 0.05, importance = TRUE, ntreeWV = NULL, doBest = TRUE, replace = FALSE, tunecontrol = tuneControl(random = TRUE), splitrule = tune_grid$splitrule, trace = TRUE)

model <- tuneRF(x, y, mtryStart = 2, ntreeTry = 50, splitrule = tune_grid$splitrule, trace = TRUE)

model_500 <- tuneRF(x, y, mtryStart = 2, ntreeTry = 500, splitrule = tune_grid$splitrule, trace = TRUE,stepFactor=1.5,improve=0.05, plot=TRUE)

model_500 <- tuneRF(x, y, mtryStart = 2, ntreeTry = 50, splitrule = tune_grid$splitrule, trace = TRUE,stepFactor=1.5,improve=0.05)

model_1000 <- tuneRF(x, y, mtryStart = 2, ntreeTry = 1000, splitrule = tune_grid$splitrule, trace = TRUE)
model_1000 <- tuneRF(x, y, mtryStart = 2, ntreeTry = 1000, splitrule = tune_grid$splitrule, trace = TRUE,stepFactor=1.5)

model_1000$best$mtry

rf_3=randomForest(x, y, mtry =model$best$mtry, splitrule = model$best$splitrule, ntree = model$best$ntree)


rf_1=randomForest(x, y, mtry =4, splitrule = "gini", ntree = 500)
rf_2=randomForest(x, y, mtry =5, splitrule = "gini", ntree = 500)
rf_3=randomForest(x, y, mtry =6, splitrule = "gini", ntree = 500)


rfp=predict(rf_3,x)
rfCM=table(rfp,y)
rfCM
sum(diag(rfCM))/sum(rfCM)
rfProb=predict(rf,x,type="prob")
multiclass.roc(carData[,7],rfProb[,2])

plot(multiclass.roc(carData[,7],rfProb[,2]))






