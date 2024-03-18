library(tidyverse)
library(caret)
library(e1071)

setwd("D:/#Spring 2023/5580 - Text Mining/Assignment5")

set.seed(1)

# Read the data file which has info about top 6 products from sales219
data <- read.csv('data.csv')

# Get top 6 product ids in list
top_products <- data %>% 
  group_by(item_sk) %>% 
  summarize(total_quantity = sum(quantity)) %>% 
  select(item_sk, total_quantity) %>% 
  arrange(desc(total_quantity))

#---------------------------------------------------------------------------
# selected product = Top 1st
#11740941
#11741274
#11629829
product <- top_products$item_sk[1]
product

# Filter the data for selected product only
xy <- data %>% 
  filter(item_sk == product, quantity != 3) %>% 
  select(-item_sk)

# Check for any missing dates in the data

is_continuous <- all(diff(xy$Date) == 1)

# Remove the data as well
xy$date <- NULL

# Reshape the dataframe to have 7 independent and 1 dependent (8 in total cols)

total_rows = nrow(xy)
rows_to_reshape = total_rows - (total_rows %% 8)
reshaped_xy <- 
  as.data.frame(matrix(xy[1:rows_to_reshape, 1], ncol = 8, byrow = TRUE))


trainIndex<-createDataPartition(reshaped_xy$V8, p=0.8, list=FALSE)
train<-reshaped_xy[trainIndex,]
test<-reshaped_xy[-trainIndex,]


# Set X and y
X <- train[,1:7]
y <- train[,8]

X_test <- test[,1:7]
y_test <- test[,8]

myCvControl <- trainControl(method = "repeatedCV",
                            number=10,
                            repeats = 5)


# Support Vector Regression
# method = svmRadial
cat(product , "svmRadial")
svmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)

# method = svmLinear
cat(product , "svmLinear")
svmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "svmLinear",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)


# method = svmPoly
cat(product , "svmPoly")
svmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "svmPoly",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl,
                    tuneGrid = expand.grid(
                      C = c(1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                      degree = c(1, 2, 3, 4, 5, 6),
                      scale = 0.001)
                    )
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)

# Kernel = “sigmoid”
cat(product , "sigmoid")

svmFitTime=svm(V8 ~ .,
               data = train,kernek="sigmoid")
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)

#---------------------------------------------------------------------------
# selected product =  3 rd Top
#11740941
#11741274
#11629829
product <- top_products$item_sk[3]
product

# Filter the data for selected product only
xy <- data %>% 
  filter(item_sk == product, quantity != 1) %>% 
  select(-item_sk)

# Check for any missing dates in the data

is_continuous <- all(diff(xy$Date) == 1)

# Remove the data as well
xy$date <- NULL

# Reshape the dataframe to have 7 independent and 1 dependent (8 in total cols)

total_rows = nrow(xy)
rows_to_reshape = total_rows - (total_rows %% 8)
reshaped_xy <- 
  as.data.frame(matrix(xy[1:rows_to_reshape, 1], ncol = 8, byrow = TRUE))


trainIndex<-createDataPartition(reshaped_xy$V8, p=0.8, list=FALSE)
train<-reshaped_xy[trainIndex,]
test<-reshaped_xy[-trainIndex,]


# Set X and y
X <- train[,1:7]
y <- train[,8]

X_test <- test[,1:7]
y_test <- test[,8]

myCvControl <- trainControl(method = "repeatedCV",
                            number=10,
                            repeats = 5)


# Support Vector Regression
# method = svmRadial
cat(product , "svmRadial")
svmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)

# method = svmLinear
cat(product , "svmLinear")
svmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "svmLinear",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)


# method = svmPoly
cat(product , "svmPoly")
svmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "svmPoly",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl,
                    tuneGrid = expand.grid(
                      C = c(1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                      degree = c(1, 2, 3, 4, 5, 6),
                      scale = 0.001)
)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)

# Kernel = “sigmoid”
cat(product , "sigmoid")

svmFitTime=svm(V8 ~ .,
               data = train,kernek="sigmoid")
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)

#---------------------------------------------------------------------------
# selected product = Top 5 th
#11740941
#11741274
#11629829
product <- top_products$item_sk[5]
product

# Filter the data for selected product only
xy <- data %>% 
  filter(item_sk == product, quantity != 603) %>% 
  select(-item_sk)

# Check for any missing dates in the data

is_continuous <- all(diff(xy$Date) == 1)

# Remove the data as well
xy$date <- NULL

# Reshape the dataframe to have 7 independent and 1 dependent (8 in total cols)

total_rows = nrow(xy)
rows_to_reshape = total_rows - (total_rows %% 8)
reshaped_xy <- 
  as.data.frame(matrix(xy[1:rows_to_reshape, 1], ncol = 8, byrow = TRUE))


trainIndex<-createDataPartition(reshaped_xy$V8, p=0.8, list=FALSE)
train<-reshaped_xy[trainIndex,]
test<-reshaped_xy[-trainIndex,]


# Set X and y
X <- train[,1:7]
y <- train[,8]

X_test <- test[,1:7]
y_test <- test[,8]

myCvControl <- trainControl(method = "repeatedCV",
                            number=10,
                            repeats = 5)


# Support Vector Regression
# method = svmRadial
cat(product , "svmRadial")
svmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "svmRadial",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)

# method = svmLinear
cat(product , "svmLinear")
svmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "svmLinear",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)


# method = svmPoly
cat(product , "svmPoly")
svmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "svmPoly",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl,
                    tuneGrid = expand.grid(
                      C = c(1e-1, 1e0, 1e1, 1e2, 1e3, 1e4),
                      degree = c(1, 2, 3, 4, 5, 6),
                      scale = 0.001)
)
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)

# Kernel = “sigmoid”
cat(product , "sigmoid")

svmFitTime=svm(V8 ~ .,
               data = train,kernek="sigmoid")
svmFitTime
summary(svmFitTime)
y_hat = predict(svmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y)
y_hat2 = predict(svmFitTime, newdata = X_test)
mean(100*abs(y_hat2-y_test)/y_test)
