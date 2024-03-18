library(tidyverse)
library(caret)

setwd("C:/Users/ajay2/OneDrive/OneDrive - Saint Marys University/Desktop/MCDA5580 Mining/Assignmen4")

# Read the data file which has info about top 6 products from sales219
data <- read.csv('data.csv')

# Get top 6 product ids in list
top_products <- data %>% 
  group_by(item_sk) %>% 
  summarize(total_quantity = sum(quantity)) %>% 
  select(item_sk, total_quantity) %>% 
  arrange(desc(total_quantity))

# Get the first product and analyze

product <- top_products$item_sk[1]

# Filter the data for selected product only

xy <- data %>% 
  filter(item_sk == product) %>% 
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

# Set X and y

X <- reshaped_xy[,1:7]
y <- reshaped_xy[,8]

# Linear Regression
# Using pre-sliced data
myCvControl <- trainControl(method = "repeatedCV",
                            number=10,
                            repeats = 5)

# Linear regression
glmFitTime <- train(V8 ~ .,
                    data = reshaped_xy,
                    method = "glm",
                    preProc = c("center", "scale"),
                    tuneLength = 10,
                    trControl = myCvControl)
glmFitTime
summary(glmFitTime)
y_hat = predict(glmFitTime, newdata = X)
mean(100*abs(y_hat-y)/y) 
#Mean Absolute Percentage Error
# Your error with linear regression

