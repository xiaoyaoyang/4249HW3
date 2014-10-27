train<-read.csv("train.csv",header=T,sep=",")
features<-read.csv("features.csv",header=T,sep=",")
stores<-read.csv("stores.csv",header=T,sep=",")
cor(stores)
str(stores)
#change classified variables into dummy variables

as.factor(features$IsHoliday)
features$IsHoliday[features$IsHoliday=='TRUE']<-'1'
features$IsHoliday[features$IsHoliday=='FALSE']<-'0'

#fill in missing value of CPI
require(forecast)
fore_m=matrix(nrow=13,ncol=0)
for(i in 1:45)
{
  temp<-subset(features,Store==i)
  tempfull<-temp[complete.cases(temp[,10:11]),]
  ts<-ts(tempfull$CPI,frequency=52,start=c(2010,2,5))
  fore=data.frame(forecast(ts, h=13))
  fore_m=cbind(fore_m,fore[,1])
}
fore_m

#fill in missing value of Unemployment
fore_n=matrix(nrow=13,ncol=0)
for(i in 1:45)
{
  temp<-subset(features,Store==i)
  tempfull<-temp[complete.cases(temp[,10:11]),]
  ts<-ts(tempfull$Unemployment,frequency=52,start=c(2010,2,5))
  fore=data.frame(forecast(ts, h=13))
  fore_n=cbind(fore_n,fore[,1])
}
fore_n
write.csv(fore_n, file ="/Users/Kimberly/Desktop/ADSproject/unemploymentfill.csv")


