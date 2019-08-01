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

### 3. Import the dataset
  mydata= read.csv(file = ".\\Data\\HR_Attrition_Dataset.csv")
  sapply(mydata, class)

### 4. Preprocessing Data
  ## 4.1 Make dependent variable as a factor (categorical)
  mydata$left = as.factor(mydata$left)
 

### 5. Divide the data into 80% Training and 20% Testing datasets


### 6. Construct Decision Tree Model
  ## 6.1 Define the formula


mtree <- rpart(formula=left ~., data = mydata, method="class")
mtree

#control = rpart.control(minsplit = 1, minbucket = 1, maxdepth = 10, usesurrogate = 2, xval =10 )
fancyRpartPlot(mtree)

predict_label = predict(mtree, type = "class")
#predict_label

tree_result = cbind.data.frame(mydata, predict_label = predict_label)


### 7   Evaluate the performance of the model
#CARET package functions
#install.packages("e1071")
library(caret)
confusionMatrix(tree_result$predict_label, tree_result$left, positive = "1")


### 8   Finding Optimal Decision tree model using pre-pruning



### 9   Finding Optimal Decision tree model using post-pruning



### 10  Model Selection based on performance metrics



### 11. Extracting Rules
asRules(mtree)
