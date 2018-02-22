setwd("C:/Users/akins/Documents/thesisCode")

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
set.seed(1234)

load("./Combined_Data/Sample_Data_2006-16 V3.Rda")



Data=as.data.table(Data)


# Random Forest
df=Data[sample(.N,100000)]
save(df, file=paste("./Combined_Data/Sample_Data_RF.Rda"))
# load("./Combined_Data/Sample_Data_RF.Rda")

train=df[1:80000]
test=df[80001:100000]

formula= Loan_Status ~ STATE + OCLTV + DTI + CSCORE_B + Year + ORIG_RT.x + FTHB_FLG + 
  PURPOSE + OCC_STAT + Loan.Age + Unemployment_Rate +Rent_ratio

model <- randomForest(formula,
                      data = train,importance=TRUE, ntree=80)

pred<- predict(model, test)

table(test$Loan_Status,pred)

result <- confusionMatrix(pred, test$Loan_Status)



result$overall['Accuracy']


varImpPlot(model)




Accuracy=data.frame(ntree=c(1:15))

formula0= Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B + ORIG_RT.x + 
  Rent_ratio  + OCLTV + DTI + OCC_STAT + PURPOSE + FTHB_FLG +  STATE 
Accuracy$formula0=""

formula1=Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B + ORIG_RT.x + 
  Rent_ratio  + OCLTV + DTI + OCC_STAT + PURPOSE + FTHB_FLG
Accuracy$formula1=""

formula2=Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B + ORIG_RT.x + 
  Rent_ratio  + OCLTV + DTI + OCC_STAT + PURPOSE
Accuracy$formula2=""

formula3=Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B + ORIG_RT.x + 
  Rent_ratio  + OCLTV + DTI + OCC_STAT
Accuracy$formula3=""

formula4=Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B + ORIG_RT.x + 
  Rent_ratio  + OCLTV + DTI
Accuracy$formula4=""

formula5=Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B + ORIG_RT.x + 
  Rent_ratio  + OCLTV 
Accuracy$formula5=""

formula6=Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B + ORIG_RT.x + 
  Rent_ratio
Accuracy$formula6=""

formula7=Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B + ORIG_RT.x
Accuracy$formula7=""

formula8=Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B
Accuracy$formula8=""

formula8=Loan_Status ~ Loan.Age + Unemployment_Rate + Year
Accuracy$formula8=""

formula9=Loan_Status ~ Loan.Age + Unemployment_Rate 
Accuracy$formula9=""


for ( j in 0:9){
  for (i in 1:15){
    print(paste(j,"-",i))
    model <- randomForest(get(paste("formula",j,sep = "")),
                          data = train,importance=TRUE, ntree=(10*i))
    
    pred<- predict(model, test)
    
    table(test$Loan_Status,pred)
    
    result <- confusionMatrix(pred, test$Loan_Status)
    
    Accuracy[i,paste("formula",j,sep = "")]=result$overall['Accuracy']
    print(result$overall['Accuracy'])
  }
}

write.csv(Accuracy,"RF Accuracy.csv",row.names = FALSE)




# Multinomial Logit

train=Data[sample(.N,1000000)]

train$stat2 <- relevel(train$Loan_Status, ref = "Default")
model<- multinom(Loan_Status ~ Loan.Age + Unemployment_Rate + Year + CSCORE_B + ORIG_RT.x + 
                   Rent_ratio  + OCLTV + DTI + OCC_STAT + PURPOSE + FTHB_FLG +  STATE 
                 ,
                 data = train)

model
summary(model)



predictMNL <- function(model, newdata) {
  
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
    
    # Draw random values
    vals <- runif(nrow(newdata))
    
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
    
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
    
    # Return the values
    return(ids)
  }
}


test=Data[sample(.N,1000000)]

pred <- predictMNL(model,test)

table(pred,test$Loan_Status)



###############Simple Logit################3

df=Data[sample(.N,2500000)]

df$Loan_Status=as.character(df$Loan_Status)

df=df[df$Loan_Status!="Prepay",]

df$Loan_Status=as.factor(as.character(df$Loan_Status))

df=as.data.frame(df)
#df = df[, c("Loan_Status","Loan.Age","Unemployment_Rate","Year","CSCORE_B",
            "ORIG_RT.x","Rent_ratio","OCLTV","DTI")]

df = df[,c("Loan_Status","Loan.Age","Unemployment_Rate","Year","CSCORE_B",
           "ORIG_RT.x","Rent_ratio","OCLTV","DTI","OCC_STAT","PURPOSE",
           "FTHB_FLG","STATE")]


train=df[1:500000,]
test=df[500001:1178137,]


class(train$Loan_Status)



model <- glm(as.factor(Loan_Status) ~.,family=binomial(link='logit'),
             data=train[,c("Loan_Status","Loan.Age","Unemployment_Rate","Year","CSCORE_B",
                           "ORIG_RT.x","Rent_ratio","OCLTV","DTI","OCC_STAT","PURPOSE",
                           "FTHB_FLG","STATE")])

X=summary(model)

X$coefficients

write.csv(X$coefficients,"./op/Simple_Logit_Coefficient.csv")

rocplot(model)


test=test[test$PURPOSE %in% train$PURPOSE,]

pred<-predict(model, test)

modFit2 <- data.table(ifelse(pred > 1,1,0))

table(modFit2$V1,test$Loan_Status)





########## KNN #########################33

df=Data[sample(.N,120000)]
  
myvars <- c("Loan.Age","Unemployment_Rate","Year","CSCORE_B","ORIG_RT.x","Rent_ratio",
            "OCLTV","DTI")

# ,"OCC_STAT","PURPOSE","FTHB_FLG","STATE"

df=as.data.frame(df)
gc.subset <- df[,myvars]

train.gc <- gc.subset[1:80000,]
test.gc <- gc.subset[80001:120000,]

train.def <- df$Loan_Status[1:80000]
test.def <- df$Loan_Status[80001:120000]


op=data.frame()

for (i in 1:50){
  knn.1 <-  knn(train.gc, test.gc, train.def, k=i)
  
  accu=sum(test.def == knn.1)/nrow(test.gc)
  op=rbind(op,data.frame(k=i,accuracy=accu))
  print(paste(i,"-",accu))
  print(table(knn.1,test.def))
}


write.csv(op,"./op/KNN_Accuracy.csv",row.names = FALSE)

plot(op$k,op$accuracy,xlab = "k values" , ylab = "Accuracy",
     main = "Percent Accuracy vs K values")



####### Survival ##########3

df=Data[1:100000,]

library(survival)

df$defaulted=ifelse(df$Loan_Status=="Default",1,0)

formula = Surv(Loan.Age - 1, Loan.Age, defaulted) ~
  CSCORE_B + OLTV + DTI +   ORIG_RT.x + 

formula <- Surv(Loan.Age - 1, Loan.Age, defaulted) ~  Unemployment_Rate + Year + CSCORE_B +
  ORIG_RT.x +Rent_ratio  + OCLTV + DTI + OCC_STAT + PURPOSE + FTHB_FLG 

cox_model = coxph(formula, data = df)

summary(cox_model)

sink("./op/Survival Cox modelV2.txt")
summary(cox_model)
sink()


df$cox_risk = predict(cox_model, type = "risk")

hazard_multiplier = function(name) {
  display_name = c(OLTV = "Current loan-to-value ratio",
                   CSCORE_B = "Credit score",
                   DTI = "Debt-to-income ratio",
                   ORIG_RT.x = "Original Rate")[name]
  
  range = quantile(df[, name], c(0.02, 0.98))
  vals = seq(range[1], range[2], length.out = 500)
  ix = match(name, names(cox_model$coefficients))
  coef = cox_model$coefficients[ix]
  mean = cox_model$means[ix]
  haz = data.frame(variable = as.character(display_name),
                   x = vals,
                   y = exp(coef * (vals - mean)))
  
  return(haz)
}

df=as.data.frame(df)


hazard_rates = do.call(rbind, lapply(c("OLTV", "CSCORE_B", "DTI", "ORIG_RT.x"), hazard_multiplier))


png(filename = "./op/hazard_rates.png", height = 1000, width = 1000)
ggplot(data = hazard_rates, aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(~variable, ncol = 2, scales = "free_x") +
  scale_x_continuous("") +
  scale_y_continuous("Default rate multiplier\n") +
  labs(title = "Cox model default rate multipliers\n") +
  # theme_tws(base_size = 18) +
  theme(strip.text = element_text(size = rel(1)),
        panel.margin = unit(2, "lines"))
dev.off()





stime <- c(as.numeric(df$Loan.Age))
status <- c(df$Loan_Status!="Paying")
sur_x=Surv(stime, status)
fit1 <- survfit(Surv(stime,status))
plot(fit1)




## Naive bayes  i did this myself###

#load("./Combined_Data/Sample_Data_2006-16 V3.Rda")
Data=as.data.table(Data)
Data=as.data.table(Data)
df=Data[sample(.N,500)]
df = df[,c("Loan_Status","loanAge","Unemployment_Rate","Year","CSCORE_B",
                                              "ORIG_RT.x","Rent_ratio","OCLTV","DTI","OCC_STAT","PURPOSE",
                                             "FTHB_FLG","STATE")]
df$Loan_Status=as.character(df$Loan_Status)
df$Loan_Status=as.factor(as.character(df$Loan_Status))
df=as.data.table(df)
sample_size=500
df=df[sample(.N,sample_size)]
df=as.data.frame(df)
train_size=round(sample_size/2,0)
train=df[1:train_size,]
test=df[(train_size+1):sample_size,]
library(rpart)
classifier = rpart(formula = Loan_Status~.,data= train)
y_pred = predict(classifier, newdata = test_set[-1])  ## gives prob
y_pred1 = predict(classifier, newdata = test_set[-1], type = 'class') ## givs class

# Making the Confusion Matrix
cm = table(test_set[,1], y_pred1)
### Plotting the decision tree
plot(classifier)
text(classifier)



MyData <- read.csv(file="C:/Users/akins/Documents/thesisCode/RF Accuracy.csv", header=TRUE, sep=",")
