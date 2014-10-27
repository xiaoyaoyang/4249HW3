#Time Series Analysis for Store1 using package zoo
install.package("zoo")
library(zoo)
store1 <- train_sum1[,c(1,3,4)][train_sum1$Store==1,]
store1.Date <- as.Date(store1[,1])
store1ts <- zoo(store1[,c(2,3)],store1.Date)
class(store1ts)
plot(store1ts)
store1ts <- ts(store1,)
plot.ts(store1ts, main="Store_1")

decompose(store1ts)
