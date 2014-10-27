#loading
require(plyr)
require(VIM)
require(gdata)
require(lubridate)
require(useful)
require(glmnet)

#read data
setwd("/Users/hubei/Google Drive/Schoolwork/2014Spring/FinalProject_4249/team_27/FinalProject//Data")
features <- read.table('featureswithoutmissingvalue.csv',sep=',',header=T,stringsAsFactors=F)
stores <- read.table('stores.csv',sep=',',header=T,stringsAsFactors=F)
train <- read.table('train.csv',sep=',',header=T,stringsAsFactors=F)
test <- read.table('test.csv',sep=',',header=T,stringsAsFactors=F)
# features <- read.xls('featureswithoutmissingvalue.xlsx')

features$Date <- as.character(as.Date(features$Date,format='%m/%d/%y'))



#  This Part is Data preparation for Regression Analysis

# #aggregate by date
# head(train$Date)
# ##train_date
# train_date <- aggregate(x=train,by=list(train$Date),FUN=function(x)mean(x))
# train_date <- train_date[,-c(2,3,4)]
# names(train_date)[1] <- 'Date'
# date <- as.Date(train_date[,1])
# train_date[,1] <- unique(format(date, format="%m%d"))
# head(train_date[order(train_date[,1]),])
# #date from 1/1
# timeDiff <- as.Date(train_date$Date,format='%m%d')-as.Date('2014-01-01')
# train_date <- data.frame(timeDiff=as.numeric(timeDiff),train_date)
# ##features_date
# features_date <- aggregate(x=features,by=list(features$Date),FUN=function(x)mean(x))
# features_date <- features_date[,-c(2,3)]
# names(features_date)[1] <- 'Date'
# date <- as.Date(features_date$Date,format='%Y-%m-%d')
# features_date$Date <- unique(format(date,format='%m%d'))
# head(features_date[order(features_date[,1]),])
# #date from 1/1
# timeDiff <- as.Date(features_date$Date,format='%m%d')-as.Date('2014-01-01')
# features_date <- data.frame(timeDiff=as.numeric(timeDiff),features_date)
# 
# # assume 364 days try local poly regression
# 
# ######Finally data look like this using inner join
# dat <- join(x=features_date,y=train_date,by=c('Date','IsHoliday','Week'),type='inner')
# head(dat[order(dat$timeDiff),],10)
# ######



#aggregate by Week
head(train$Date)
##train_date
train_date <- aggregate(x=train,by=list(train$Date),FUN=function(x)mean(x))
train_date <- train_date[,-c(2,3,4)]
names(train_date)[1] <- 'Date'
date <- as.Date(train_date[,1])
train_date$Date <- date
train_date <- data.frame(Week=ceiling(yday(date)/7),train_date)
###
##features_date
features_date <- aggregate(x=features,by=list(features$Date),FUN=function(x)mean(x))
features_date <- features_date[,-c(2,3)]
names(features_date)[1] <- 'Date'
date <- as.Date(features_date$Date)
features_date$Date<-date
features_date <- data.frame(Week=ceiling(yday(date)/7),features_date)
## Weeking  
dat <- join(x=features_date,y=train_date,by=c('Date','IsHoliday','Week'),type='inner')
head(dat[order(dat$Week),],10)



#some visuliazation
#see if tempreture is related to WeeklySales
train_stores <- aggregate(Weekly_Sales ~ Store,train,sum)
features_tem <- aggregate(Temperature ~ Store,features,mean)
temp <- join(x=train,y=features,by=c('Store','Date','IsHoliday'))
names(temp)


plot(temp$Fuel_Price,temp$Weekly_Sales,pch='.',
      xlab='FuelPrice',ylab='Weekly_Sales',main='FuelPrice vs Sales')
plot(temp$CPI,temp$Weekly_Sales,pch='.',
     xlab='CPI',ylab='Weekly_Sales',main='CPI vs Sales')

#Date vs Temperature
plot(dat[order(dat$Week),][,c(1,3)],xlab='Days from 1/1',main='Date vs Temperature',type='l',lwd=2)
plot(dat[order(dat$Week),][,c(1,13)],type='l',main='Date vs Sales')
plot(dat[order(dat$Week),][,c(3,13)],main='')
plot(dat[order(dat$Week),][,c(1,10)],type='l',main='')

## Regress without doubt!
dat$IsHoliday <- factor(dat$IsHoliday)
require(glmnet)
x.matrix <- data.matrix(dat[,-c(2,5,6,7,8,9,13)]) #delete markdown and other useless column see names(dat)
y.matrix <- dat[,13]
x.matrix <- scale(x.matrix)
x.matrix[,6] <- as.factor(x.matrix[,6])
y.matrix <- scale(y.matrix)

fit.glm <- glmnet(x=x.matrix,alpha=0,y=y.matrix,family='gaussian',lambda=1)
s0 <- predict(fit.glm,x.matrix)
#s0*1784.994+15976.7
s0*1784.994+15976.7

##kernel regression to determine dayScore
require(KernSmooth)
h <- dpill(x=dat$Week,y=dat$Weekly_Sales)  #select bandwidth
fit.lp <- locpoly(x=dat$Week,y=dat$Weekly_Sales,bandwidth=1,gridsize=53,degree=1,range.x=c(1,53))
plot(dat[order(dat$Week),][,c(1,13)],type='l',cex=0.5,main='Week vs Sales',ylab='WeeklySales')
lines(fit.lp$x,fit.lp$y,'l',col='red')
legend('topleft',legend=c('realValue','fittedVlaue'),lty=c(1,1),col=c(1,'red'))
day_score <- data.frame(timeDiff=fit.lp$x,dayScore=fit.lp$y)



####Regression with whole data set

## case1 using week score
temp <- join(x=train,y=features,by=c('Store','Date','IsHoliday'))
names(temp)
date <- as.Date(temp$Date,format='%Y-%m-%d')
timeDiff <- ceiling(yday(date)/7)
dat_reg <- data.frame(y=temp$Weekly_Sales,timeDiff=timeDiff,IsHoliday=temp$IsHoliday,
                      Temperature=temp$Temperature,Unemployment=temp$Unemployment,
                      Store=as.factor(temp$Store),Dept=as.factor(temp$Dept),CPI=temp$CPI)
dat_join <- join(x=dat_reg,y=day_score,by='timeDiff')

X <- build.x(y~dayScore + IsHoliday + Temperature + Unemployment + CPI + Store + Dept,data=dat_join)
y <- build.y(y~dayScore + IsHoliday + Temperature + Unemployment + CPI + Store + Dept ,data=dat_join)
cvfit1 = cv.glmnet(X, y, type.measure = "mse", nfolds = 20)


### case2, Week as dummy variable Week=as.factor(timeDiff),

##1 linear regression
temp <- join(x=train,y=features,by=c('Store','Date','IsHoliday'))
date <- as.Date(temp$Date,format='%Y-%m-%d')
timeDiff <- ceiling(yday(date)/7)
Week=as.factor(timeDiff)
dat2 <-data.frame(y=temp$Weekly_Sales,Week=as.factor(timeDiff),IsHoliday=temp$IsHoliday,Temperature=temp$Temperature,
                  Unemployment=temp$Unemployment,Store=as.factor(temp$Store),
                  Dept=as.factor(temp$Dept),CPI=temp$CPI)
X <- build.x(y~ Week + IsHoliday + Temperature + Unemployment + CPI + Store + Dept,data=dat2)
y <- build.y(y~Week + IsHoliday + Temperature + Unemployment + CPI + Store + Dept ,data=dat2)
require(doMC)
registerDoMC(cores = 2)
cvfit = cv.glmnet(X, y, type.measure = "mse", nfolds = 10,parallel=T)
res <-  predict(cvfit, newx = X, type = "response", s='lambda.1se')
Final_res <- cbind(train,data.frame(Sales_pred=as.vector(res)))

group <- (Final_res$Store==1)&(Final_res$Dept==1)
plot(timeDiff[group],Final_res[group,]$Weekly_Sales,pch='.',cex=3,
     main='Predict for Store1 & Dept1',xlab='Week',ylab='WeeklySales',ylim=c(0,60000))
lines(sort(timeDiff[group]),Final_res[group,]$Sales_pred[order(timeDiff[group])],col='red')

plot(timeDiff[group],Final_res[group,]$Weekly_Sales,pch='.',cex=3,
     main='Predict for Store1 & Dept1',xlab='Week',ylab='WeeklySales',ylim=c(0,60000))
lines(sort(timeDiff[group]),Final_res[group,]$Sales_pred[order(timeDiff[group])],col='red')


# 3 PCR
fit1 <-  mvr(y~.,data=data.frame(y,X),ncomp=3)
summary(fit1)




###test for case 1 
temp <- join(x=test,y=features,by=c('Store','Date','IsHoliday'))
names(temp)
date <- as.Date(temp$Date,format='%Y-%m-%d')
timeDiff <- ceiling(yday(date)/7)
dat_test <- data.frame(timeDiff=as.factor(timeDiff),IsHoliday=temp$IsHoliday,Temperature=temp$Temperature,
                      Unemployment=temp$Unemployment,Store=as.factor(temp$Store))
dat_join_test <- join(x=dat_test,y=day_score,by='timeDiff')
dat_join_test <- data.frame(y=1,dat_join_test)
X.test <- build.x(y~dayScore + IsHoliday + Temperature + Unemployment + Store,data=dat_join_test)
## predict
res <-  predict(cvfit, newx = X.test, type = "response", s = 'lambda.1se')
Final_res <- data.frame(test,WeeklySales=res)


###test for case 2
temp <- join(x=test,y=features,by=c('Store','Date','IsHoliday'))
names(temp)
date <- as.Date(temp$Date,format='%Y-%m-%d')
timeDiff <- ceiling(yday(date)/7)
dat2_test <-data.frame(y=1,Week=factor(timeDiff,levels=levels(as.factor(c(1:53)))),IsHoliday=temp$IsHoliday,Temperature=temp$Temperature,
                  Unemployment=temp$Unemployment,Store=as.factor(temp$Store),
                  Dept=as.factor(temp$Dept),CPI=temp$CPI)
X.test <- build.x(y~Week + IsHoliday + Temperature + Unemployment + CPI + Store + Dept,data=dat2_test)
res <-  predict(cvfit, newx =X.test, type = "response", s = 'lambda.1se')
Final_res <- data.frame(test,WeeklySales=res)


#not sigificant!
fit_holiday = cv.glmnet(x=cbind(1,X[,3,drop=F]), y, type.measure = "mse", nfolds = 10,parallel=T)



