#The purpose of this program is to run 4 models using SMOTE technique 
#to try and deal with class imbalance


#packages needed
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)
require(rsample)
require(RANN) 
require(data.table) 
library(DMwR)

#Read in dataset
ds1 <- read.csv(file='C:/Users/bgitar/Documents/DSU/data/WA_Fn-UseC_-HR-Employee-Attrition.csv', header=TRUE)

#drop unwanted variables
drop <- c("EmployeeCount","Over18")
ds1 = ds1[,!(names(ds1) %in% drop)]

set.seed(3001)
ds1_split <- initial_split(ds1, prop = .7)
ds1_train <- training(ds1_split)
ds1_test <- testing(ds1_split)

setDT(ds1_train)
setDT(ds1_test)

vars2Use <- c('YearsInCurrentRole', 'StockOptionLevel', 'Gender2', 'Education', 'DistanceFromHome', 'Age', 'MonthlyIncome', 'MaritalStatus2', 'JobSatisfaction', 'BusinessTravel2', 'WorkLifeBalance')
vars2Use

#check how many "yes's" are in the training set
as.data.frame(table(ds1_train$Attrition))

## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
ds1_train_b <- SMOTE(Attrition ~., ds1_train, perc.over = 100, k = 9, perc.under = 300)

as.data.frame(table(ds1_train_b$Attrition))

#############################################################################
#Logistic
mylogit <- glm(Attrition ~ YearsInCurrentRole + StockOptionLevel + Gender + Education + DistanceFromHome + Age + MonthlyIncome + MaritalStatus2 + JobSatisfaction + BusinessTravel2 + WorkLifeBalance,
                family ="binomial", data=ds1_train_b)
summary(mylogit)

tidy(mylogit)

exp(coef(mylogit))


## Predict the Values
predict <- predict(mylogit, ds1_test, type = 'response')

## Create Confusion Matrix
table(ds1_test$Attrition, predict > 0.5)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, ds1_test$Attrition)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

###################################################################
#random forrest

rf = randomForest(Attrition~YearsInCurrentRole + StockOptionLevel + Gender + Education + DistanceFromHome + Age + MonthlyIncome + MaritalStatus2 + JobSatisfaction + BusinessTravel2 + WorkLifeBalance,  
                  ntree = 100,
                  data = ds1_train_b)
plot(rf)
varImp(rf)

varImpPlot(rf,  
           sort = T,
           n.var=25,
           main="Variable Importance")

predicted.response <- predict(rf, ds1_test)

confusionMatrix(data=predicted.response,  
                reference=ds1_test$Attrition)

######################################################################
#xgboost
dtrain <-  
  xgb.DMatrix( # xgb specific data structure 
    data = as.matrix(ds1_train_b[, .SD, .SDcols = vars2Use]), 
    label = ds1_train_b$Attrition2 
  ) 

system.time( 
  xgb5.train <-  
    xgb.cv( 
      data = dtrain,  
      nrounds = 30,  
      nthread = 3, 
      metrics = list("auc", "error"), 
      nfold = 5, 
      max_depth = 8,  
      eta = 0.1,   
      objective = "binary:logistic", 
      prediction = TRUE 
    ) 
) 


system.time( 
  xgb5.train <-  
    xgboost( 
      data = dtrain,  
      nrounds = 30,  
      nthread = 3, 
      metrics = list("rmse","auc", "error"), 
      max_depth = 8,  
      eta = 0.1,   
      objective = "binary:logistic", 
      prediction = TRUE 
    ) 
) 

xgb.importance( 
  feature_names = vars2Use, 
  model = xgb5.train 
) 


# Confusion Matrix 
( 
  twoByTwo.xgb.test <- 
    table( 
      yhat.xgb.test > 0.5,  
      truth = factor(ds1_test$Attrition2) 
    ) 
) 

#accuracy (1-accuracy)
1 - sum( 
  diag( 
    twoByTwo.xgb.test 
  ) 
) /  
  sum(twoByTwo.xgb.test)   

##################################################################
#bayesian classification


