library(rpart.plot)
library(caret)
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




#Plot of a Single Tree in the Random Forecast model

# Plot function
library(dplyr)
library(ggraph)
library(igraph)

tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}

# Plot tree
tree_func(final_model = rf, 1)
