####################################################
#MLM_project_code.R

#Bret G.

#load in packages needed

#read in dataset
ds1 <- read.csv(file='C:/Users/bgitar/Documents/DSU/data/WA_Fn-UseC_-HR-Employee-Attrition.csv', header=TRUE)

#drop unwanted variables
drop <- c("EmployeeCount","Over18")
ds1 = ds1[,!(names(ds1) %in% drop)]

#split dataset into test/train sets
ds1_split <- initial_split(ds1, prop = .7)
ds1_train1 <- training(ds1_split)
ds1_test1 <- testing(ds1_split)

ds1_train <- setDT(ds1_train1)
ds1_test <- setDT(ds1_test1)
str(ds1_train1)

#create subset of variables wanted in models
ds1_check<- subset(ds1, ,c('Attrition2', 'YearsInCurrentRole', 'StockOptionLevel', 'Gender2', 'Education', 'DistanceFromHome', 'Age', 'MonthlyIncome', 'MaritalStatus2', 'JobSatisfaction', 'BusinessTravel2', 'WorkLifeBalance', 'PercentSalaryHike', 'NumCompaniesWorked','YearsSinceLastPromotion'))
str(ds1_check) 

#check correlation matrix for any mutlicollinearity issues
cormatrix <- cor(ds1_check)
round(cormatrix, 3)

library(corrplot)
corrplot(cormatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#check distribution of dependent variable
as.data.frame(table(ds1$Attrition))

p<-ggplot(ds1, aes(x=Attrition2)) + 
  geom_histogram(binwidth=1, color="black", fill="Orange")
p

#drop correlated features
#create subset of variables wanted in models
ds1_train <- subset(ds1_train, ,c('Attrition2', 'YearsInCurrentRole', 'StockOptionLevel', 'Gender2', 'Education', 'DistanceFromHome', 'Age', 'MonthlyIncome', 'JobSatisfaction', 'BusinessTravel2', 'WorkLifeBalance', 'PercentSalaryHike', 'NumCompaniesWorked','OverTime2'))
ds1_test <- subset(ds1_test, ,c('Attrition2', 'YearsInCurrentRole', 'StockOptionLevel', 'Gender2', 'Education', 'DistanceFromHome', 'Age', 'MonthlyIncome', 'JobSatisfaction', 'BusinessTravel2', 'WorkLifeBalance', 'PercentSalaryHike', 'NumCompaniesWorked','OverTime2'))

##########################################################################################
#Logistic regression
str(ds1_train1)
#Use ds1_train1 to keep factor variables for categorical features in logisiic 
library(tidyverse)
mylogit <- glm(Attrition ~ YearsInCurrentRole + StockOptionLevel + Gender + DistanceFromHome + MonthlyIncome  + JobSatisfaction + BusinessTravel + WorkLifeBalance + PercentSalaryHike + NumCompaniesWorked + OverTime,
               family ="binomial", data=ds1_train1)
summary(mylogit)
tidy(mylogit)
exp(coef(mylogit))


## Predict the Values on test dataset
predict <- predict(mylogit, ds1_test1, type = 'response')

## Create Confusion Matrix
table(ds1_test1$Attrition, predict > 0.5)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, ds1_test1$Attrition)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

##########################################################################################
#Random Forest 

require(randomForest) 

vars2Use <- c('YearsInCurrentRole', 'StockOptionLevel', 'Gender2', 'DistanceFromHome', 'Age', 'MonthlyIncome', 'JobSatisfaction', 'BusinessTravel2', 'WorkLifeBalance', 'PercentSalaryHike', 'NumCompaniesWorked','OverTime2')
set.seed(48)

system.time( # Captures the length of time that the enclosed fxn takes to run 
  rf1.train <-  
    randomForest( 
      ds1_train [, .SD, .SDcols = vars2Use], # data.table notation 
      factor(ds1_train$Attrition2), 
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

confusionMatrix(
  factor(rf1.test),
  factor(ds1_test$Attrition2)
)

##############################################################################################
#XGBOOST

#xgboost
dtrain <-  
  xgb.DMatrix( # xgb specific data structure 
    data = as.matrix(ds1_train[, .SD, .SDcols = vars2Use]), 
    label = ds1_train$Attrition2 
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

#predict on test data

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

######### 
require(Boruta) 


set.seed(15) 

# Boruta will take some time to run because it is performing multiple RFs, each  
# of which is building multiple trees 
system.time( 
  bor1.train <-  
    Boruta( 
      x = ds1_train[, .SD, .SDcols = vars2Use],  
      y = factor(ds1_train$Attrition2) 
    ) 
) 


plot(bor1.train) 
bor1.train

######################################################################################
#Bayesian Classification

#create subset of variables wanted in models
bayes_train <- subset(ds1_train, ,c('Attrition2', 'StockOptionLevel',  'DistanceFromHome',  'MonthlyIncome', 'JobSatisfaction', 'BusinessTravel2', 'WorkLifeBalance', 'PercentSalaryHike', 'NumCompaniesWorked','OverTime2'))
bayes_test <- subset(ds1_test, ,c('Attrition2',  'StockOptionLevel',  'DistanceFromHome', 'MonthlyIncome', 'JobSatisfaction', 'BusinessTravel2', 'WorkLifeBalance', 'PercentSalaryHike', 'NumCompaniesWorked','OverTime2'))

set.seed(16)  

#create naive bayes model
library(caret)
library(dplyr)
library(corrplot)

#convert numeric variables to factors for nb model to run
bayestrain <- bayes_train %>%
  mutate(
    Attrition2 = factor(Attrition2),
    StockOptionLevel = factor(StockOptionLevel),
    OverTime2 = factor(OverTime2),
    JobSatisfaction = factor(JobSatisfaction),
    WorkLifeBalance = factor(WorkLifeBalance),
    BusinessTravel2 = factor(BusinessTravel2)
  )

bayestest <- bayes_test %>%
  mutate(
    Attrition2 = factor(Attrition2),
    StockOptionLevel = factor(StockOptionLevel),
    OverTime2 = factor(OverTime2),
    JobSatisfaction = factor(JobSatisfaction),
    WorkLifeBalance = factor(WorkLifeBalance),
    BusinessTravel2 = factor(BusinessTravel2)
  )

table(bayestrain$Attrition2) %>% prop.table()

table(bayestest$Attrition2) %>% prop.table()
str(bayestrain)
#check correlations in a nice plot
bayestrain %>%
  filter(Attrition2 == 1) %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#check distributions of independent features
bayestrain %>% 
  select(DistanceFromHome, MonthlyIncome, StockOptionLevel, Gender2, Education, OverTime2,JobSatisfaction, WorkLifeBalance, BusinessTravel2) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")

# create response and feature data
features <- setdiff(names(bayestrain), "Attrition2")
x <- bayestrain[, features]
y <- bayestrain$Attrition2

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)
train_control
# train model
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)

# results
confusionMatrix(nb.m1)

#predict on test dataset
predm1 <- predict(nb.m1, newdata = bayestest)
confusionMatrix(predm1, bayestest$Attrition2)

# set up tuning grid to try and improve prediciton results
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

# top 5 models
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# plot search grid results
plot(nb.m2)

# results for best model
confusionMatrix(nb.m2)

#predict on test dataset
predm2 <- predict(nb.m2, newdata = bayestest)
confusionMatrix(predm2, bayestest$Attrition2)

#Turns out tuning the model did not make it more accurate in this case

#########################################################################


rm( 
  list = ls() 
) # Clears global environment of all visible objects (some hidden objects 
#   # may remain) 


# End 
