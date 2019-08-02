############################################################################
## Author   : Rakesh Pardeshi                                             ##
## Name     : 1_DT_CODE                                                   ##
## Details  : Decision Tree-Model Development on HR Attrition Data        ##
##                                                                        ##
## Date Midified :                                                        ##
##                                                                        ##
##                                                                        ##
##                                                                        ##
############################################################################

### 1. Clean the workspace : This will remove previously stored objects from the R-workspace.
  rm(list = ls())

### 2. Intialize the workspace by loading required libraries
  library(rpart)
  library(rattle)
  library(caret)

### 3. Import the dataset
  hr_data= read.csv(file = ".\\Data\\HR_Attrition_Dataset.csv")
  sapply(mydata, class)

### 4. Preprocessing Data
  ## 4.1 Make dependent variable as a factor (categorical)
  hr_data$left = as.factor(hr_data$left)
 

### 5. Divide the data into 80% Training and 20% Testing datasets
  set.seed(3456)
  trainIndex <- createDataPartition(hr_data$left, p = 0.8,  list = FALSE, times = 1)
  hr_train <- hr_data[trainIndex, ]
  hr_test <- hr_data[-trainIndex, ]
  

### 6. Construct Decision Tree Model
  mtree <- rpart(formula=left ~., data = hr_train, method="class")
  mtree
  fancyRpartPlot(mtree)
  


### 7.   Evaluate the performance of the model
  ##7.1 Performance on training data
  train_prediction_label = predict(mtree, newdata = hr_test, type = "class")
  train_prediction_label
  train_result = cbind.data.frame(hr_train, prediction_label = train_prediction_label)
  

  ##7.2 Performance on testing data
  test_prediction_label = predict(mtree,  newdata = hr_test, type = "class")
  test_prediction_label
  test_result = cbind.data.frame(hr_test, prediction_label = test_prediction_label)


### 8. Evaluate the performance of the model
train_cf  = confusionMatrix(train_result$prediction_label, train_result$left, positive = "1")
test_cf = confusionMatrix(test_result$prediction_label, test_result$left, positive = "1")

