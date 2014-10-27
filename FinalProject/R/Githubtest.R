rm(list=ls())
setwd("D:/CU/W4249/Final project")
train<-read.csv("train.csv",header=T)
stores<-read.csv("stores.csv",header=T)
features<-read.csv("featuress.csv",header=T)

#missing values
require(mice) 
md.pattern(features)
require(VIM) 
aggr(features)
attach(features)

#fill in the missing values of CPI

###I dont think it can be done by simple lm, cuz there are same date. The data is a mess.

CPI[is.na(CPI)==T]<--99
plot(x=Date,y=CPI)
CPI2<-CPI[CPI!=-99]
Date2<-as.numeric(Date[CPI!=-99])
lm.CPI<-lm(CPI2~Date2)
Date1<-as.numeric(Date[CPI==-99])
CPI[CPI==-99]<-lm.CPI$coefficients[1]+lm.CPI$coefficients[2]*Date1
#fill in the missing values of Unemployment
Unemployment[is.na(Unemployment)==T]<--99
plot(x=Date,y=Unemployment)
Unemployment2<-Unemployment[Unemployment!=-99]
Date2<-as.numeric(Date[Unemployment!=-99])
lm.unem<-lm(Unemployment2~Date2)
Date1<-as.numeric(Date[Unemployment==-99])
Unemployment[Unemployment==-99]<-lm.unem$coefficients[1]+lm.unem$coefficients[2]*Date1

detach(features)


