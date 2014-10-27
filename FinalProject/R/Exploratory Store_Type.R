#loading
require(plyr)
require(VIM)

#read data
setwd("/Users/Carol/Desktop/2014 Spring/Applied Data Science/team_27/FinalProject/Data")
feature <- read.table('features.csv',sep=',',header=T,stringsAsFactors=F)
store <- read.table('stores.csv',sep=',',header=T,stringsAsFactors=F)
train <- read.table('train.csv',sep=',',header=T,stringsAsFactors=F)

nrow(feature)

#Exploratory analysis of dataset store
store$Type <- factor(store$Type)
num_A <- sum(store$Type=='A')
num_B <- sum(store$Type=='B')
num_C <- sum(store$Type=='C')

#One-way anova
ntype <- max(as.numeric(store$Type))
fita <- aov(Size ~ Type, data=store)
summary(fita)
layout(matrix(c(1,2,3,4),2,2))
plot(fita)

#Visualize group difference
 # what is this line for...... attach(mtcars)
ntype=3
yrange <- range(store$Size)
xrange <- c(1,max(store$Store))

layout(matrix(c(1,2,3,4),1,1))
plot(xrange, yrange, type="n", ylab="Size",xlab="Store Index")
colors <- rainbow(ntype)
linetype <- c(1:ntype)
plotchar <- seq(18,18+ntype,1)

for (i in 1:ntype) {
  type <- subset(store, as.numeric(store$Type)==i)
  lines(type$Store,type$Size,type="b",lwd=1.5,
        lty=linetype[i],col=colors[i],pch=plotchar[i])  
}

title("Size vs Type")

legend('topright', c('A','B','C'), cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Type")

#For different types, we can see siginicant differences in stores' size
#There are some unusual cases in each type, like store 33 and store 36 in type A
#store 3 and store 5 in type B


#Preprocess train_data
#Sum weekly_sales of different departments in each store, in each date

train_sum <- aggregate(x=train$Weekly_Sales,by=list(train$Store,
                                                    train$Date),FUN="sum")
names(train_sum) <- c("Store", "Date", "Week_Sales")
#pick up data for store=1 Dept=1
num <- sum(train$Store==1 & train$Dept==1)
Date <- train$Date[1:num]
#time range from 2010-02-05 to 2012-10-26
IsHoliday <- train$IsHoliday[1:num]
use <- as.data.frame(cbind(Date,IsHoliday))
names(use)
train_sum1 <- merge(train_sum, use, by="Date")

#Time Series Analysis
store1 <- train_sum1[,c(3,4)][train_sum1$Store==1,]
store1.Date <- as.Date(store1[,1])
store1 <- cbind(store1.Date, store1)
store1ts <- ts(store1)
plot.ts(store1ts, main="Store_1")
library("TTR")
store1tsSMA3 <- SMA(store1ts,n=8)
plot.ts(store1tsSMA3)
store1tsd <- decompose(store1tsSMA3)

#We can see from the plot clearly that weekly_sales fluctuate with 
#existence of holiday

#There are significant seasonal effects, as well as a slightly yearly 
#trend, for weekly_sales increased on average. Then we try decompose this
#time series into three parts, one is trend, one is seasonal, one is random
#which I haven't figured it out...



#merge store size and deal with 'Date' variable
train_sum2 <- merge(train_sum1, store, by="Store")

library(lubridate)
library(ggplot2)
library(quantmod)
train_sum2$Date <- ymd(train_sum2$Date)

#extract month and year, for sales are recorded each week 
store1_new <- train_sum2[train_sum2$Store==1,]
mday <- month(store1_new$Date)
yday <- year(store1_new$Date)
data <- data.frame(store1_new$Date, mday,yday,store1_new$Week_Sales)
names(data) <- c("Date","month","year","weekly_sale")

p <- ggplot(data, aes(factor(mday),factor(yday),z=weekly_sale))
p + stat_summary2d(fun=function(x) mean(x))+labs(x="month",y="year")+ggtitle("Store 1")

#another try, plot for store and month in 2010,2011,2012
mday <- month(train_sum2$Date)
yday <- year(train_sum2$Date)
data <- data.frame(train_sum2$Store,mday,yday,train_sum2$Week_Sales)
#year2010
data1 <- data[data$yday==2010,]
p <- ggplot(data1, aes(factor(train_sum2.Store),factor(mday),z=train_sum2.Week_Sales))
p + stat_summary2d(fun=function(x) mean(x))+labs(x="Store",y="month")+ggtitle("Year 2010")
#year2011                                          
data2 <- data[data$yday==2011,]
p <- ggplot(data2, aes(factor(train_sum2.Store),factor(mday),z=train_sum2.Week_Sales))
p + stat_summary2d(fun=function(x) mean(x))+labs(x="Store",y="month")+ggtitle("Year 2011")
#year2012
data3 <- data[data$yday==2012,]
p <- ggplot(data3, aes(factor(train_sum2.Store),factor(mday),z=train_sum2.Week_Sales))
p + stat_summary2d(fun=function(x) mean(x))+labs(x="Store",y="month")+ggtitle("Year 2012")

#linear regression

train_sumues <- merge(train_sum2,datas,by="Store")
ddayl <- day(train_sumues$Date)
mdayl <- month(train_sumues$Date)
ydayl <- year(train_sumues$Date)
datafit <- cbind(train_sumues,mdayl,ydayl,ddayl)
datafit$ydayl <- as.character(datafit$ydayl)
fit1 <- lm(Week_Sales ~ Size+mdayl+IsHoliday+Type+Group+ddayl,data=datafit)
datafit2 <- datafit
datafit2[,1] <- as.character(datafit2[,1])
fit2 <- lm(Week_Sales ~ Store+Size+mdayl+IsHoliday+Type+Group+ddayl,data=datafit2)                     

library(caret)
forTraining <- createDataPartition(datafit[,1],p=3/4)[[1]]
features4 <- feature[,c(1,2,3,4,10,11)]
datafit_plus <- join(datafit,features4,by=c('Date','Store'),type='left')

datafitn <- datafit[,-c(2,5,6,7,9)]
datafitn_plus <- datafit_plus[,-c(2,5,6,7,9)]
datafitn[,1] <- as.character(datafitn[,1])
datafitn_plus[,1] <- as.character(datafitn_plus[,1])
datafittrain <- datafitn[forTraining,]
datafittrain_plus <- datafitn_plus[forTraining,]
datafittest <- datafitn[-forTraining,]
datafittest_plus <- datafitn_plus[-forTraining,]

controlObject<-trainControl(method="repeatedcv",repeats=5, number=10)
                                                         
set.seed(1000)
linearRego1<-train(Week_Sales ~. ,data=datafittrain,method="lm",
                  trControl=controlObject)
linearRego1
summary(linearRego1)
lmPred1<-predict(linearRego1, newdata=datafittest)
lmValues1<-data.frame(obs=datafittest[,2],pred=lmPred1)
library(caret)
defaultSummary(lmValues1)
plot(datafittest[,2],ylab="Weekly_Sales",main="Linear Regression")
points(lmPred1,pch=2,col='red')


set.seed(1000)
linearRego2<-train(Week_Sales ~. ,data=datafittrain_plus,method="lm",
                   trControl=controlObject)
linearRego2
summary(linearRego2)
lmPred2 <- predict(linearRego2, newdata=datafittest_plus)
lmValues2 <- data.frame(obs=datafittest_plus[,2],pred=lmPred2)
library(caret)
defaultSummary(lmValues2)
points(lmPred2,pch=3,col='green')
legend("topright", c("True","Predicted1","Predicted2"),pch=c(1,2,3),col=c(1,2,3))


#pls model
set.seed(1000)
plsModel1<-train(Week_Sales ~. ,data=datafittrain,
                method="pls",
                tuneLength=15,
                trControl=controlObject)
plsPred1 <- predict(plsModel1,newdata=datafittest)
plsValues1 <-data.frame(obs=datafittest[,2],pred=plsPred1)
library(caret)
defaultSummary(plsValues1)
plot(datafittest[,2],ylab="Weekly_Sales",main="Partial Least Square")
points(plsPred1,pch=2,col='red')

#no difference from results obatined using linear regression, so there are no 
#correlation between these variables.

set.seed(1000)
plsModel2<-train(Week_Sales ~. ,data=datafittrain_plus,
                 method="pls",
                 tuneLength=15,
                 trControl=controlObject)
plsPred2 <- predict(plsModel2,newdata=datafittest_plus)
plsValues2 <-data.frame(obs=datafittest_plus[,2],pred=plsPred2)
library(caret)
defaultSummary(plsValues2)
points(plsPred2,pch=3,col='green')
legend("topright", c("True","Predicted1","Predicted2"),pch=c(1,2,3),col=c(1,2,3))

#knn method
set.seed(1000)
knn1<-train(Week_Sales ~. ,data=datafittrain,
                method="knn",
                tuneGrid=data.frame(.k=c(1:20)),
                 trControl=controlObject)
knnPred1 <- predict(knn1,newdata=datafittest)
knnValues1 <- data.frame(obs=datafittest[,2],pred=knnPred1)
library(caret)
defaultSummary(knnValues1)
plot(datafittest[,2],)
points(knnPred1,pch=2,col='red',ylab="Weekly_Sales",main="K nearest neighbors")


set.seed(1000)
knn2<-train(Week_Sales ~. ,data=datafittrain_plus,
            method="knn",
            tuneGrid=data.frame(.k=c(1:20)),
            trControl=controlObject)
knnPred2 <- predict(knn2,newdata=datafittest_plus)
knnValues2 <- data.frame(obs=datafittest[,2],pred=knnPred2)
library(caret)
defaultSummary(knnValues2)
points(knnPred2,pch=3,col='green')
legend("topright", c("True","Predicted1","Predicted2"),pch=c(1,2,3),col=c(1,2,3))

#random forest 
set.seed(1000)
rfModel1<-train(Week_Sales ~. ,data=datafittrain,
               method="rf",
               tuneLength=10,
               ntrees=300,
               importance=TRUE,
               trControl=controlObject)  

rfPred1 <-predict(rfModel1,newdata=datafit)
rfValues1 <-data.frame(obs=datafittest[,2],pred=rfPred1)
library(caret)
defaultSummary(rfValues1)
plot(datafittest[,2],)
points(rfPred1,pch='x',col='red')

set.seed(1000)
rfModel2 <- train(Week_Sales ~. ,data=datafittrain_plus,
                method="rf",
                tuneLength=10,
                ntrees=300,
                importance=TRUE,
                trControl=controlObject)  

rfPred2 <- predict(rfModel2,newdata=datafittest)
rfValues2 <-data.frame(obs=datafittest_plus[,2],pred=rfPred2)
library(caret)
defaultSummary(rfValues2)
points(rfPred2,pch='x',col='green')



#regression tree tune Cp
library(caret)
set.seed(1000)
rpartModel1 <- train(Week_Sales ~. ,data=datafittrain,
                  method="rpart",
                  tuneLength=30,
                  trControl=controlObject)

rpartPred1 <-predict(rpartModel1,newdata=datafittest)
rpartValues1 <-data.frame(obs=datafittest[,2],pred=rpartPred1)
library(caret)
defaultSummary(rpartValues1)
plot(datafittest[,2],ylab="Weekly_Sales",main="Regression Tree")
points(rpartPred1,pch=2,col='red')


set.seed(1000)
rpartModel2 <- train(Week_Sales ~. ,data=datafittrain_plus,
                     method="rpart",
                     tuneLength=30,
                     trControl=controlObject)
rpartPred2 <-predict(rpartModel2, newdata=datafittest_plus)
rpartValues2 <-data.frame(obs=datafittest_plus[,2],pred=rpartPred2)
library(caret)
defaultSummary(rpartValues2)
points(rpartPred2,pch=3,col='green')
legend("topright", c("True","Predicted1","Predicted2"),pch=c(1,2,3),col=c(1,2,3))






