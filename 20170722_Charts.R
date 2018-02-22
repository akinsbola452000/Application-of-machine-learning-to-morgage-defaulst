setwd("C:\\Users\\Hitendra\\Desktop\\Five\\akinsbola452000")

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

load("./Combined_Data/Complete_Data_2006-16 V2.Rda")


# Default rate by vintage

df_total=Data[, .(Total=.N), by = Year]

Data_default=Data[Data$Loan_Status=="Default",]

df_default=Data_default[, .(Default=.N), by = Year]

df=merge(df_total,df_default,all.x = TRUE,by="Year")

df=df[df$Year>=2006,]

df$Default_Rate=df$Default/df$Total*100


barplot(df$Default_Rate,names.arg = df$Year,xlab = "Year",ylab = "Default Rate",
        main="Default Rate by Vintage")


# Default rate by FICO

df_total=Data[, .(Total=.N), by = CSCORE_B]

Data_default=Data[Data$Loan_Status=="Default",]

df_default=Data_default[, .(Default=.N), by = CSCORE_B]



df=data.frame(CSCORE_B=c(550:850))

df$bin=cut(df$CSCORE_B, 15, include.lowest=TRUE, labels=seq(560, 840, by = 20))

df=merge(df,df_default,all.x = TRUE,by="CSCORE_B")

df=merge(df,df_total,all.x = TRUE,by="CSCORE_B")

df=as.data.table(df)

df=df[, lapply(.SD, sum, na.rm=TRUE), by=bin ]

df$Default_Rate=df$Default/df$Total*100

df$bin=as.numeric(as.character(df$bin))

plot(df$bin, df$Default_Rate, type="l", xlab="Credit Score",
     ylab="Default Rate", main = "Default Rate by Credit Score" ) 


### Default rate by LTV

Data_09_11=Data[Data$Year %in% c(2009,2010,2011),]

df_total=Data_09_11[, .(Total=.N), by = OLTV]

Data_default=Data_09_11[Data_09_11$Loan_Status=="Default",]

df_default=Data_default[, .(Default=.N), by = OLTV]


df=data.frame(OLTV=c(1:100))

df$bin=cut(df$OLTV, 10, include.lowest=TRUE, labels=seq(5, 95, by = 10))

df=merge(df,df_default,all.x = TRUE,by="OLTV")

df=merge(df,df_total,all.x = TRUE,by="OLTV")

df=as.data.table(df)

df=df[, lapply(.SD, sum, na.rm=TRUE), by=bin ]

df$Default_Rate=df$Default/df$Total*100

df$bin=as.numeric(as.character(df$bin))

plot(df$bin, df$Default_Rate, type="l", xlab="LTV",
     ylab="Default Rate", main = "Default Rate LTV" ) 


## Average credit scor by vintage


df=Data[,list(Year,CSCORE_B)]
  
df=df[, lapply(.SD, mean, na.rm=TRUE), by=Year ]

df=df[df$Year>=2006,]

df=df[order(df$Year),]

plot(df$Year, df$CSCORE_B, type="l", xlab="Year",ylim = c(600,850),
     ylab="Credit Score", main = "Averge credit scor eby vintage" ) 

## Default Rate by cities


df_total=Data[, .(Total=.N), by = STATE]

Data_default=Data[Data$Loan_Status=="Default",]

df_default=Data_default[, .(Default=.N), by = STATE]

df=merge(df_total,df_default,all.x = TRUE,by="STATE")


df$Default_Rate=df$Default/df$Total*100


barplot(df$Default_Rate,names.arg = df$STATE,xlab = "STATE",ylab = "Default Rate",
        main="Default Rate by State",las=2)



########## Annulaized default rate by month #########33333

# Data_06_08=Data[Data$Year %in% c(2006,2007,2008),]

# df=Data_06_08[1:100,]

Data$Last_Date=Data$Date+months(Data$Loan.Age)

Data$Last_Year=year(Data$Last_Date)

df=Data[Data$Year<=2008 & Data$Year>=2006,]

df_total=df[, .(Total=.N), by = Last_Year]

Data_default=df[df$Loan_Status=="Default",]

df_default=Data_default[, .(Default=.N), by = Last_Year]

df=merge(df_total,df_default,all.x = TRUE,by="Last_Year")

df=df[df$Last_Year>=2006 & df$Last_Year<=2015,]

df$Default_Rate=df$Default/df$Total*100


plot(df$Last_Year, df$Default_Rate, type="l", xlab="credit Score",
     ylab="Default Rate", main = "Default Rate by Year" ) 

library(scales)


#######
# Default rate by Purpose
Data=Data[!is.na(Data$Loan_Status),]

df=Data[, .(Total=.N), by = c("PURPOSE","Loan_Status")]

ggplot(df,aes(x = PURPOSE, y = Total,fill = Loan_Status)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format())


# Default rate by OCC_STAT

df=Data[, .(Total=.N), by = c("OCC_STAT","Loan_Status")]

ggplot(df,aes(x = OCC_STAT, y = Total,fill = Loan_Status)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format())


# Default rate by OCC_STAT

df=Data[, .(Total=.N), by = c("FTHB_FLG","Loan_Status")]

ggplot(df,aes(x = FTHB_FLG, y = Total,fill = Loan_Status)) + 
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format())










