setwd("C:/Users/akins/Documents/thesisCode")

require('survival');
library(data.table)

load("./Combined_Data/Sample_Data_2006-16 V3.Rda")

Data=as.data.table(Data)

df=Data[sample(.N,1000000)]

df$Status=1
df$Status[df$Loan_Status=="Paying"]=2

df=df[df$OCC_STAT %in% c("I","P"),]

# SF.SUR_MASTER= survfit(Surv(Loan.Age,Status) ~ OCC_STAT, data = df)

SF.SUR_MASTER= survfit(Surv(Loan.Age,Status) ~ 1, data = df)

plot(SF.SUR_MASTER,xlab="Months", ylab="Probability of Survival",main="Survival Curve") 

# legend("topright", c("OCC_STAT=I","OCC_STAT=P"), lty=c(2,3),fill = c("Red","Blue"),
#        cex = 0.75,text.width=c(4))




#SVM



