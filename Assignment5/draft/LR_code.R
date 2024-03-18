library(tidyverse)
library(caret)

setwd("D:/#Spring 2023/5580 - Text Mining/Assignment5")

# setup functions
mape <- function(actual,pred) {
  mape <- mean(abs((actual-pred)/actual))*100
  return (mape)
}

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

###########################

# Linear Regression
# Using pre-sliced data
# Linear regression
glmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "glm",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
glmFitTime
summary(glmFitTime)

# MAPE Training
y_hat = predict(glmFitTime, newdata = X)
mape(y,y_hat)
# MAPE Testing
y_hat2 = predict(glmFitTime, newdata = X_test)
mape(y_test,y_hat2)


#---------------------------------------------------------------------------
# selected product = Top 3th
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

###########################

# Linear Regression
# Using pre-sliced data
# Linear regression
glmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "glm",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
glmFitTime
summary(glmFitTime)

# MAPE Training
y_hat = predict(glmFitTime, newdata = X)
mape(y,y_hat)
# MAPE Testing
y_hat2 = predict(glmFitTime, newdata = X_test)
mape(y_test,y_hat2)

#---------------------------------------------------------------------------
# selected product = Top 5th
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

###########################

# Linear Regression
# Using pre-sliced data
# Linear regression
glmFitTime <- train(V8 ~ .,
                    data = train,
                    method = "glm",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
glmFitTime
summary(glmFitTime)

# MAPE Training
y_hat = predict(glmFitTime, newdata = X)
mape(y,y_hat)
# MAPE Testing
y_hat2 = predict(glmFitTime, newdata = X_test)
mape(y_test,y_hat2)




