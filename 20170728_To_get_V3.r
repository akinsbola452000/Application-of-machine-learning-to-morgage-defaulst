setwd("/wrk/bolarinw/DONOTREMOVE/")

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


load("./Combined_Data/Sample_Data_2006-16 V2.Rda")

# rm(list=setdiff(ls(), "Data"))

# df=Data[100000:101000,]

Data$Date<-as.Date(paste("01/",Data$ORIG_DTE,sep = ""), "%d/%m/%Y")


unemployment <- readxl::read_excel("Unemployment rate.xlsx", sheet = 1)

unemployment=melt(unemployment,id.vars=c("Year"))
names(unemployment)=c("Year","Month","Unemployment_Rate")

unemployment$Date<-as.Date(paste("01/",unemployment$Month,"/",unemployment$Year,
                                 sep = ""), "%d/%b/%Y")

Data=merge(Data,unemployment[,c("Date","Unemployment_Rate")],
           by.x = "Date",by.y = "Date" ,all.x = TRUE )


Rent_ratio=read.csv("State_PriceToRentRatio_AllHomes1.csv")

Rent_ratio=Rent_ratio[,c(-1,-3)]

Rent_ratio=melt(Rent_ratio,id.vars=c("RegionName"))


Rent_ratio$Date_month <- substring(Rent_ratio$variable, 2)

Rent_ratio$Date<-as.Date(paste(Rent_ratio$Date_month,".01",sep = ""), "%Y.%m.%d")


State_lookup=readxl::read_excel("States Lookup.xlsx", sheet = 1)

Rent_ratio=merge(Rent_ratio,State_lookup,all.x=TRUE)

colnames(Rent_ratio)[3]="Rent_ratio"

Rent_ratio=Rent_ratio[,c("STATE","Date","Rent_ratio")]


Rent_ratio_2=readxl::read_excel("dlmrentsprices2016q1.xls", sheet = 2)

Rent_ratio_2=Rent_ratio_2[,c(1,6)]  

Rent_ratio_2$Date=as.character(round(Rent_ratio_2$Date,1))

Rent_ratio_2$Year=substring(Rent_ratio_2$Date, 1,4)  

Rent_ratio_2$Quarter=substring(Rent_ratio_2$Date, 6)   

Rent_ratio_2=merge(Rent_ratio_2,data.frame(Quarter=c(1,2,3,4),Month=c(1,4,7,10)),all.x = TRUE)

Rent_ratio_3=Rent_ratio_2
Rent_ratio_3$Month=Rent_ratio_3$Month+1

Rent_ratio_4=Rent_ratio_2
Rent_ratio_4$Month=Rent_ratio_4$Month+2

Rent_ratio_2000to10=rbind(Rent_ratio_2,Rent_ratio_3,Rent_ratio_4)

Rent_ratio_2000to10$Date=as.Date(paste(Rent_ratio_2000to10$Year,"-",Rent_ratio_2000to10$Month,
                                       "-01",sep = ""),format="%Y-%m-%d")

names(Rent_ratio_2000to10)[3]='rent-price ratio'

Rent_ratio_2000to10=Rent_ratio_2000to10[,c("Date","rent-price ratio")]
names(Rent_ratio_2000to10)[2]="Rent_ratio"  


Data_after2010=Data[Data$Date>="2010-10-01",]  

Data_after2010=merge(Data_after2010,Rent_ratio,by=c("STATE","Date"),all.x = TRUE)  


Data_before2010=Data[Data$Date<"2010-10-01",]  

Data_before2010=merge(Data_before2010,Rent_ratio_2000to10,by=c("Date"),all.x = TRUE)  


Data=rbind(Data_after2010,Data_before2010)
rm(Data_after2010)
rm(Data_before2010)


rm(list=setdiff(ls(), "Data"))


# any(is.na(Data$Rent_ratio))
# 
summary(Data$Unemployment_Rate)
# 
# table(Data$Unemployment_Rate)

Data=Data[!is.na(Data$DTI),]

Data=Data[!is.na(Data$Rent_ratio),]

Data=Data[!is.na(Data$CSCORE_B),]

Data=Data[!is.na(Data$Unemployment_Rate),]


Data$Year=year(Data$Date)


table(Data$Loan_Status)

Data=Data[Data$STATE!="VI",]

Data$Loan_Status=as.factor(Data$Loan_Status)
Data$STATE=as.factor(as.character(Data$STATE))
Data$FTHB_FLG=as.factor(as.character(Data$FTHB_FLG))
Data$PURPOSE=as.factor(as.character(Data$PURPOSE))
Data$OCC_STAT=as.factor(as.character(Data$OCC_STAT))


save(Data, file=paste("./Combined_Data/Sample_Data_2006-16 V3.Rda"))
