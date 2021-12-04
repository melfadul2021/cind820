library(Metrics)
#install.packages("performance")
library(performance)
#install.packages("glmnet")
library(glmnet)
library(dplyr)
library(caret)
library(tidyverse)
library(corrplot)

d1=read.csv('C:/Users/myada/OneDrive/Desktop/ryreson/CIND820/student-mat.csv',sep=";",header=TRUE)


d1=subset(d1,select = -c(G1,G2))
d1=select_if(d1, is.numeric)
summary(d1)
head(d1)
hist(d1$G3)
corrplot(cor(d1))


#math dataset - spilt data(trainset 80% ,testset 20%)

set.seed(11)
samp <- sample(1:nrow(d1), (nrow(d1)*.8))
train.mat <- d1[samp,]
test.mat <- d1[-samp,]
dim(train.mat)
dim(test.mat)

#scale the numeric data 
cols = c("age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime", "goout","Dalc","Walc","health","absences")
pre_proc_val <- preProcess(train.mat[,cols], method = c("center", "scale"))

train.mat[,cols] = predict(pre_proc_val, train.mat[,cols])
test.mat[,cols] = predict(pre_proc_val, test.mat[,cols])

summary(train.mat)


#Baseline model 

baseline = mean(train.mat$G3)
summary(baseline)
TestRmse_Baseline <- sqrt(mean((test.mat$G3 - baseline)^2))
TestRmse_Baseline
TestMAE_Baseline <- mean(abs(test.mat$G3-baseline))
TestMAE_Baseline
TrainRmse_Baseline <- sqrt(mean((train.mat$G3 - baseline)^2))
TrainRmse_Baseline
TrainMAE_Baseline <- mean(abs(train.mat$G3-baseline))
TrainMAE_Baseline


#Linear Regression Model 

#train dataset
model_mat_all_train <- lm(formula=G3~.,data = train.mat)
summary(model_mat_all_train)



#model performance

# create the evaluation metrics function

eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2=as.character(round(summary(model)$r.squared, 2))
  adj_r2=as.character(round(summary(model)$adj.r.squared, 2))
  #print(adj_r2) #Adjusted R-squared
  RMSE=as.character(round(sqrt(sum(resids2)/N), 2))
  MAE=as.character(round(mean(abs(resids)),2))
 # print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
  
  data.frame(
    RMSE = RMSE,
    MAE=MAE,
    Rsquare = r2,
    Adjusted_Rsquared = adj_r2)}

#  predicting and evaluating the model on train data
predictions = predict(model_mat_all_train, newdata = train.mat)
eval_metrics(model_mat_all_train, train.mat, predictions, target = 'G3')




#test dataset
model_mat_all_test <- lm(formula=G3~.,data = test.mat)
summary(model_mat_all_test)


#model performance


# predicting and evaluating the model on test data
predictions = predict(model_mat_all_test, newdata = test.mat)
eval_metrics(model_mat_all_test, test.mat, predictions, target = 'G3')



#Lasso Regression Model
#Regularization
#Creating model matrics - glment does not support dataframes

cols_reg =names(d1)
dummies <- dummyVars(G3 ~ ., data = d1[,cols_reg])

train_dummies = predict(dummies, newdata = train.mat[,cols_reg])

test_dummies = predict(dummies, newdata = test.mat[,cols_reg])

print(dim(train_dummies))
print(dim(test_dummies))

#Bulid Lasso Model 

x_train = as.matrix(train_dummies)
y_train = train.mat$G3

x_test = as.matrix(test_dummies)
y_test = test.mat$G3

lambdas <- 10^seq(2, -3, by = -.1)
# Setting alpha = 1 implements lasso regression
#fit lasso regression model using k-fold cross-validation
lasso_reg <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# display optimal lambda value
lambda_best <- lasso_reg$lambda.min 
lambda_best  #0.3162278

lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)
coef(lasso_model)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x_train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)

#bulit evaluation function to see the result on test data and train data
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  MAE = (sum(abs(predicted-true)))/nrow(df)
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square,
    MAE = MAE
  )
}

eval_results(y_train, predictions_train, train.mat)
eval_results(y_test, predictions_test, test.mat)

#por dataset 
d2=read.csv('C:/Users/myada/OneDrive/Desktop/ryreson/CIND820/student-por.csv',sep=";",header=TRUE)

d2=subset(d2,select = -c(G1,G2))
d2=select_if(d2, is.numeric)
summary(d2)
head(d2)
hist(d2$G3)
corrplot(cor(d2))
mysamp <- sample(1:nrow(d2), (nrow(d2)*.8))
train.por <- d2[mysamp,]
test.por <- d2[-mysamp,]
dim(train.por)
dim(test.por)


#scale the numeric data 
cols = c("age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime", "goout","Dalc","Walc","health","absences")
por_pre_proc_val <- preProcess(train.por[,cols], method = c("center", "scale"))

train.por[,cols] = predict(por_pre_proc_val, train.por[,cols])
test.por[,cols] = predict(por_pre_proc_val, test.por[,cols])

summary(train.por)

#Baseline model 

por_baseline = mean(train.por$G3)
por_baseline
por_TestRmse_Baseline <- sqrt(mean((test.por$G3 - por_baseline)^2))
por_TestRmse_Baseline
por_TestMAE_Baseline <- mean(abs(test.por$G3-por_baseline))
por_TestMAE_Baseline
por_TrainRmse_Baseline <- sqrt(mean((train.por$G3 -por_baseline)^2))
por_TrainRmse_Baseline
por_TrainMAE_Baseline <- mean(abs(train.por$G3-por_baseline))
por_TrainMAE_Baseline

#Linear Regression Model 

#train dataset
model_por_all_train <- lm(formula=G3~.,data = train.por)
summary(model_por_all_train)

#model performance

#  predicting and evaluating the model on train data
por_train_predictions = predict(model_por_all_train, newdata = train.por)
eval_metrics(model_por_all_train, train.por, por_train_predictions, target = 'G3')


#test dataset
model_por_all_test <- lm(formula=G3~.,data = test.por)
summary(model_por_all_test)


#model performance

#  predicting and evaluating the model on test data
por_test_predictions = predict(model_por_all_test, newdata = test.por)
eval_metrics(model_por_all_test, test.por, por_test_predictions, target = 'G3')



#Lasso Regression Model
#Regularization

cols_reg_por =names(d2)
dummies_por <- dummyVars(G3 ~ ., data = d2[,cols_reg_por])

train_dummies_por = predict(dummies_por, newdata = train.por[,cols_reg_por])

test_dummies_por = predict(dummies_por, newdata = test.por[,cols_reg_por])

print(dim(train_dummies_por))
print(dim(test_dummies_por))

#Bulid Lasso Model 

x_train_por = as.matrix(train_dummies_por)
y_train_por = train.por$G3

x_test_por = as.matrix(test_dummies_por)
y_test_por = test.por$G3


#fit lasso regression model using k-fold cross-validation
lasso_reg_por <- cv.glmnet(x_train_por, y_train_por, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# display optimal lambda value
lambda_best_por <- lasso_reg_por$lambda.min 
lambda_best_por  #0.01258925

lasso_model_por <- glmnet(x_train_por, y_train_por, alpha = 1, lambda = lambda_best_por, standardize = TRUE)
coef(lasso_model_por)

predictions_train_por <- predict(lasso_model_por, s = lambda_best_por, newx = x_train_por)

predictions_test_por <- predict(lasso_model_por, s = lambda_best_por, newx = x_test_por)

#evaluation result for train dataset and testdata set for por

x<-eval_results(y_train_por, predictions_train_por, train.por)
y<-eval_results(y_test_por, predictions_test_por, test.por)








