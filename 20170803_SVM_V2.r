setwd("C:/Users/akins/Documents/thesisCode")

library(e1071)
library(pROC)
library(caret)


library(foreach)
library(reshape2)
library(data.table)
library(XLConnect)
library(zoo)
library(doMC)
library(randomForest)
library(lubridate)
library(caret)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
library(Deducer)
library(class)
library(kernlab)
library(pROC)


load("./Combined_Data/Sample_Data_2006-16 V3.Rda")



Data=as.data.table(Data)

df=Data[sample(.N,1000000)]

df$Loan_Status=as.character(df$Loan_Status)

df=df[df$Loan_Status!="Prepay",]

df$Loan_Status=as.factor(as.character(df$Loan_Status))

df=as.data.table(df)

sample_size=100000

df=df[sample(.N,sample_size)]

df=as.data.frame(df)

train_size=round(sample_size/2,0)

train=df[1:train_size,]
test=df[(train_size+1):sample_size,]

#Build SVM model with nonlinear kernel on training data
model1 <- svm(Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B + ORIG_RT.x + 
                Rent_ratio  + OCLTV + DTI + OCC_STAT + PURPOSE + FTHB_FLG +  STATE  , train,
              kernel="linear",probability=TRUE)


#ROC curve. 
train$predicted_prob1 <- predict(model1, train, probability=TRUE)
train$predicted_prob1<-attr(train$predicted_prob1, "prob")[,2]

roc(train$Loan_Status,train$predicted_prob1, auc=TRUE, plot=TRUE)


# Prediction and Confusiom  matrix
test$predictedY1 <- predict(model1, test)

table(test$Loan_Status,test$predictedY1)


