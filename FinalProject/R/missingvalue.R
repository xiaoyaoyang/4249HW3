rm(list=ls())
setwd("D:/CU/W4249/Final project")
train<-read.csv("train.csv",header=T)
stores<-read.csv("stores.csv",header=T)
features<-read.csv("features.csv",header=T)

#missing values
require(mice) 
md.pattern(features)
require(VIM) 
aggr(features)
attach(features)

#fill in the missing values of CPI
CPI[is.na(CPI)==T]<--99
plot(x=Date[1:100],y=CPI[1:100])
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


######missing value part 2
features <- read.csv("~/Desktop/features.csv")
View(features)
require(Hmisc)
describe(features)
require(caret)
require(VIM)
aggr(features)
require(mice)
imp1<-mice(features,m=5)
imp1
##features_new is the data set after dealing with missing value
features_new<-complete(imp1,inc=TRUE)
describe(features_new)
##correlation
cor_features<-cor(features_new[,c(3,4,5,6,7,8,9,10,11)])
dim(cor_features)
cor_features[1:9,1:9]
require(corrplot)
corrplot(cor_features,order="hclust")



Temp=c()
for(i in 1:45)
{
    Temp_i=features$Temperature[features$Store==i]
    Temp_mean=mean(Temp_i)
    Temp=c(Temp,Temp_mean)
}

Fuel=c()
for(i in 1:45)
{
  Fuel_i=features$Fuel_Price[features$Store==i]
  Fuel_mean=mean(Fuel_i)
  Fuel=c(Fuel,Fuel_mean)
}

CPI=c()
for(i in 1:45)
{
  CPI_i=features$CPI[features$Store==i]
  CPI_mean=mean(CPI_i)
  CPI=c(CPI,CPI_mean)
}

Un=c()
for(i in 1:45)
{
  Un_i=features$Un[features$Store==i]
  Un_mean=mean(Un_i)
  Un=c(Un,Un_mean)
}

features_new=read.csv("~/Documents/team_27/FinalProject/Data/storewithmark.csv")
features_new=na.omit(features_new)
cor_features_new<-cor(features_new[,c(10,14:18)])
corrplot(cor_features_new,order="hclust")
plot(features_new[,3],features_new[,14])

require(ggplot2)
require(lubridate)
require(reshape2)
require(coefplot)
features_new1<-subset(features_new,Year==2012)

ggplot(features_new1,aes(MarkDown1,Week_Sales,group=Store,color=Store))+geom_line()
ggplot(features_new1,aes(MarkDown2,Week_Sales,group=Store,color=Store))+geom_line()
ggplot(features_new1,aes(MarkDown3,Week_Sales,group=Store,color=Store))+geom_line()
ggplot(features_new1,aes(MarkDown4,Week_Sales,group=Store,color=Store))+geom_line()
ggplot(features_new1,aes(MarkDown5,Week_Sales,group=Store,color=Store))+geom_line()



mark=melt(features_new1,id="Date",measure=c("MarkDown1","MarkDown2","MarkDown3",
                                            "MarkDown4","MarkDown5"))
ggplot(mark,aes(Date,value,color=variable))+geom_line()

mark_new=data.frame(features_new2[,c(3,14:18)])
ggplot(mark_new,aes(Date,y=value,color=variable))+
  geom_point(aes(y = MarkDown1, col = "MarkDown1"))+ 
  geom_point(aes(y = MarkDown2, col = "MarkDown2"))+
  geom_point(aes(y = MarkDown3, col = "MarkDown3"))+
  geom_point(aes(y = MarkDown4, col = "MarkDown4"))+
  geom_point(aes(y = MarkDown5, col = "MarkDown5"))



ggplot(features_new,aes(Date,MarkDown1))+geom_line(aes(color="MarkDown1"))+
  geom_line(data=features_new,aes(color="Second line"))+
  labs(color="Legend text")


ggplot(mark_new,aes(x=Date,y=MarkDown5))+geom_point()








stores <- read.csv("~/Documents/team_27/FinalProject/Data/stores.csv")
data=cbind(Temp,Fuel,CPI,Un)

require(corrplot)
cor_features<-cor(features[,c(1,3,4,10,11,12)])
corrplot(cor_features,order="hclust")