setwd("D:/CU/W4249/Final project")
store2<-read.csv("store_withoutdept.csv",header=T)
date<-as.Date(store2$Date,'%m/%d/%Y')
store2$Year<-year(date)
store2<-store2[,-3]

store2$timeDiff<-rep(0,dim(store2)[1])
attach(store2)
timeDiff[Year==2010]<-as.Date(Date)-as.Date("1/1/2010")
timeDiff[Year==2011]<-as.Date(Date)-as.Date("1/1/2011")
timeDiff[Year==2012]<-as.Date(Date)-as.Date("1/1/2012")
timeDiff[Year==2013]<-as.Date(Date)-as.Date("1/1/2013")
write.csv(store2,file="D:/CU/W4249/Final project")
Store<-as.factor(Store)store_2010<-subset(store2,Year==2010)
ggplot(store2,aes(Date,Week_Sales,group=Store,color=Store))+geom_line()+ facet_grid(Year~.)
