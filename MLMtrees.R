####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+ 


# Bret G

# MLMtrees.R 


require(data.table) 
require(ggplot2) 
require(rpart) 
require(rpart.plot) 
require(rsample)

#read in dataset
ds1 <- read.csv(file='C:/Users/bgitar/Documents/DSU/data/WA_Fn-UseC_-HR-Employee-Attrition.csv', header=TRUE)

#drop unwanted variables
drop <- c("EmployeeCount","Over18")
ds1 = ds1[,!(names(ds1) %in% drop)]

set.seed(3000)
ds1_split <- initial_split(ds1, prop = .7)
ds1_train <- training(ds1_split)
ds1_test <- testing(ds1_split)

setDT(ds1_train)
setDT(ds1_test)

vars2Use <- c('YearsInCurrentRole', 'StockOptionLevel', 'Gender2', 'Education', 'DistanceFromHome', 'Age', 'MonthlyIncome', 'JobSatisfaction', 'BusinessTravel2', 'WorkLifeBalance', 'PercentSalaryHike', 'NumCompaniesWorked','OverTime2')
vars2Use

vars3Use <- c('YearsInCurrentRole', 'StockOptionLevel', 'Education', 'DistanceFromHome', 'Age', 'MonthlyIncome', 'JobSatisfaction')
str(ds1_train) 
str(ds1_test) 
str(vars2Use)
#create formula to build models
fmla <-  
  as.formula( 
    paste0( 
      'factor(Attrition) ~ ',  
      paste( 
        vars2Use,  
        collapse = '+' 
      ) 
    ) 
  )


rp1.train <-  
  rpart( 
    fmla,  
    ds1_train,  
    method = 'class', # class => classification 
    control = rpart.control( 
      minsplit = 20,  # minimum # obserations in a node 
      cp = 0.01 # complexity: minimum GINI score improvement 
    ) 
  ) 


rp1.train 
summary(rp1.train) 
rpart.plot(rp1.train) 


#rpart prediction in train dataset 
yhat.rp1.train <-  
  predict( 
    rp1.train, # rpart model object  
    newdata = ds1_train, # data to apply rpart model to 
    type = 'class' # type of predicted value returned: 
    # class => a factor of classifications based on responses 
  ) 


# Confusion Matrix 
( # Extra parens are a little trick in R 
  twoByTwo.rp1.train <-  
    table( 
      ds1_train$Attrition,  
      yhat.rp1.train 
    ) 
) 


# Accuracy ((TP + TN) / Total) 
sum(diag(twoByTwo.rp1.train)) / sum(twoByTwo.rp1.train)  


# rpart prediction in test dataset 
yhat.rp1.test <-  
  predict( 
    rp1.train,  
    newdata = ds1_test,  
    type = 'class' 
  ) 


# Confusion Matrix 
( # Remember this parens trick implicitly prints the object 
  twoByTwo.rp1.test <-  
    table( 
      y = ds1_test$Attrition,  
      predicted = yhat.rp1.test 
    ) 
) 


# Accuracy ((TP + TN) / Total) 
sum(diag(twoByTwo.rp1.test)) / sum(twoByTwo.rp1.test)  

#### 
require(randomForest) 


set.seed(3000) # all random forests start with a random set of features and 
#               # trees if you want to build the same model each time, then you 
#               # must set.seed 


system.time( # Captures the length of time that the enclosed fxn takes to run 
  rf1.train <-  
    randomForest( 
      ds1_train [, .SD, .SDcols = vars2Use], # data.table notation 
      factor(ds1_train$Attrition), 
      ntree = 100, # Hyperparameter: Nbr of trees in forest 
      maxnodes = 16, # Hyperparameter: Max nbr of terminal nodes (leaves or  
      # depth) 
      nodesize = 20, # Hyperparameter: minimum size of terminal nodes 
      importance = TRUE # Collect the stats for variable importance 
    ) 
) 


rf1.train 
summary(rf1.train) 
varImpPlot(rf1.train) # N.B. Accuracy and GINI disagree on mammogram_All_Rate 
# and DM_All_Rate 

#predict on test data
rf1.test <- predict(rf1.train,ds1_test)

library(caret)
confusionMatrix (rf1.test,ds1_test$Attrition)
# Confusion Matrix 
( 
  twoByTwo.rf1.test <- 
    table( 
      rf1.test > 0.5,  
      truth = factor(ds1_test$Attrition) 
    ) 
)

1 - sum( 
  diag( 
    twoByTwo.rf1.test 
  ) 
) /  
  sum(twoByTwo.rf1.test) 
##################################

############ 
require(xgboost) # You should learn to become very familiar with this package 
#                # XGBoost is one of the most commonly used classification 
#                # algorithms because it works almost _too_ well. 

dtrain <-  
  xgb.DMatrix( # xgb specific data structure 
    data = as.matrix(ds1_train[, .SD, .SDcols = vars2Use]), 
    label = ds1_train$Attrition2 
  ) 


set.seed(3000) # all random forests start with a random set of features and 
#               # trees if you want to build the same model each time, then you 
#               # must set.seed 


system.time( 
  xgb5.train <-  
    xgb.cv( 
      data = dtrain,  
      nrounds = 100,  
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
      nrounds = 100,  
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


yhat.xgb.train <-  
  predict( 
    xgb5.train,  
    dtrain 
  ) 


( 
  twoByTwo.xgb.train <- 
    table( 
      yhat.xgb.train > 0.5,  
      truth = factor(ds1_train$Attrition2) 
    ) 
) 


# 1 - Accuracy = ? 
1 - sum( 
  diag( 
    twoByTwo.xgb.train 
  ) 
) /  
  sum(twoByTwo.xgb.train)  


yhat.xgb.test <-  
  predict( 
    xgb5.train,  
    newdata = xgb.DMatrix( 
      data = as.matrix( 
        ds1_test[, .SD, .SDcols = vars2Use] 
      ) 
    ) 
  ) 


# Confusion Matrix 
( 
  twoByTwo.xgb.test <- 
    table( 
      yhat.xgb.test > 0.5,  
      truth = factor(ds1_test$Attrition2) 
    ) 
) 


1 - sum( 
  diag( 
    twoByTwo.xgb.test 
  ) 
) /  
  sum(twoByTwo.xgb.test)   
# may have over fitting issue since accuracy decreased by ~14% on test data
#How to deal with class imbalance?? Model could be more valuable by trying to predict more people who leave

######### 
require(Boruta) 




set.seed(3000) # all random forests start with a random set of features and 
#               # trees if you want to build the same model each time, then you 
#               # must set.seed 


# Boruta will take some time to run because it is performing multiple RFs, each  
# of which is building multiple trees 
system.time( 
  bor1.train <-  
    Boruta( 
      x = ds1_train[, .SD, .SDcols = vars3Use],  
      y = factor(ds1_train$Attrition) 
    ) 
) 


plot(bor1.train) 
bor1.train

rm( 
  list = ls() 
) # Clears global environment of all visible objects (some hidden objects 
#   # may remain) 




# End 
