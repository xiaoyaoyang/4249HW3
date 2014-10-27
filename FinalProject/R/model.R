dat<-dat[,-15]
dat<-dat[,-13]
data2<-dat[,-c(5:10)]
data2<-data2[,-12]
write.csv(data2,file="D:/CU/W4249/Final project/data_withoutmarkdown.csv") #data without markdown
leaps<-regsubsets(Weekly_Sales~timeDiff+Temperature+Fuel_Price+CPI+Unemployment+IsHoliday,data=dat)
plot(leaps, scale="adjr2") 
plot(leaps, scale="bic") 
null=lm(Weekly_Sales~1, data=dat) 
null 
full=lm(Weekly_Sales~., data=dat) 
full 
step(null,scope = list(upper=full), data=dat, direction="both")