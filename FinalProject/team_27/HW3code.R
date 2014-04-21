
setwd("D:/CU/W4249/HW3")
dat <- read.table('adult.data.txt',sep=',',header=F,stringsAsFactors=FALSE)
names(dat)

#pre cleaning
des <- read.table('adult.des.txt',header=F,sep=':')
des <- des$V1
des <- as.character(des)
des <- c(des,'y')
des[5] <- 'education_num';des[6] <- 'marital_status';des[11] <- 'capital_gain';des[12] <- 'capital_loss'
des[13] <- 'hours_per_week';des[14] <- 'native_country';
names(dat) <- des;
dat$y[dat$y==' <=50K'] <- 0
dat$y[dat$y==' >50K'] <- 1
dat$y <- as.numeric(dat$y)

sum(is.na(dat))


#Prelook
#age
density(dat$age)

#fnlwgt

#workclass 1836 ? value, 9 level.
table(as.factor(dat$workclass))

#education
table(as.factor(dat$education))

#education-num what is this?

#marital_status
table(as.factor(dat$marital))

#occupation  there are 1843 ? 
table(as.factor(dat$occupation))

#relationship,race and sex
table(as.factor(dat$relationship))
table(as.factor(dat$race))
table(as.factor(dat$sex))
#native_country 583 ? samples
table(as.factor(dat$native_country))

#delete sample containing ?
dat[dat==' ?'] <- NA
dat <- na.omit(dat)


#creating dummy variables
model.matrix(~dat$workclass-1)->dummy_workclass
model.matrix(~dat$education-1)->dummy_education
model.matrix(~dat$marital-1)->dummy_marital
model.matrix(~dat$occupation-1)->dummy_occupation
model.matrix(~dat$relationship-1)->dummy_relationship
model.matrix(~dat$race-1)->dummy_race
model.matrix(~dat$sex-1)->dummy_sex
model.matrix(~dat$native_country-1)->dummy_native

CreateDummy <- function(dat_var){
  n <- length(dat_var)
  site <- as.factor(dat_var)
  table(site)->site.t
  sort(site.t,decreasing=TRUE)->temp
  cumsum(temp)/sum(temp)->site.cdf
  names(temp)[site.cdf<0.99]->site.names
  # order number
  for (i in 1:n){
    if(!sum(dat_var[i]==site.names)) {dat_var[i] <- 'Others'}
  }
  model.matrix(~dat_var-1)->dummy_var
  return(dummy_var)
}
dummy_native <- CreateDummy(dat$native)
#build new data frame
datn <- data.frame(y=dat$y,dat$age,dummy_workclass,dat$fnlwgt,dummy_education,dat$education_num,dummy_marital,dummy_occupation,dummy_relationship,dummy_race,dummy_sex,dat$capital_gain,dat$capital_loss,dat$hours_per_week,dummy_native)

#seperate training and test
m <- dim(datn)[1];n <- dim(datn)[2]
dat_train <- datn[1:(0.5*m),]
dat_test <- datn[(0.5*m+1):m,]

#Using PCA data
princomp(datn[,-1]) -> pca
dat_pca <- data.frame(y=as.factor(datn$y),pca$scores[,1:3])
dat_train <- dat_pca[1:(0.5*m),]
dat_test <- dat_pca[(0.5*m+1):m,]

#logistic
require(glmnet)
cvfit = cv.glmnet(x=as.matrix(dat_train[,-1]),y=dat_train[,1],family = "binomial", type.measure = "class")
plot(cvfit)
pred <- predict(cvfit,as.matrix(dat_test[,-1]),type='class',s='lambda.min')
pred$class
TestError <- sum(pred!=dat_test[,1])/length(pred)
cat(TestError)


#knn
require(class)
fit2 <- knn(train=dat_train[,-1],test=dat_test[,-1],k=5,cl=dat_train[,1])
sum(fit2!=dat_test[,1])/length(dat_test[,1])

#LDA
require(MASS)
fit_lda<-lda(y~.,data=dat_train)
pred<-(predict(fit_lda,dat_test[,-1]))
pred<-pred$class
TestError <- sum(pred!=dat_test[,1])/length(pred)
cat(TestError)

#QDA
fit_qda<-qda(y~.,data=dat_train,cv=T)
pred<-(predict(fit_qda,dat_test[,-1]))
pred<-pred$class
TestError <- sum(pred!=dat_test[,1])/length(pred)
cat(TestError)

#Lasso 
fit_lasso<-cv.glmnet(x=as.matrix(dat_train[,-1]),y=as.numeric(dat_train[,1]),alpha=1,family = "gaussian")
plot(fit_lasso)
pred <- predict(fit_lasso,as.matrix(dat_test[,-1]))
pred[pred>0.5]<-1
pred[pred<=0.5]<-0
TestError <- sum(pred!=dat_test[,1])/length(pred)
cat(TestError)
table(pred,dat_test$y)

#Ridge
fit_ridge<-cv.glmnet(x=as.matrix(dat_train[,-1]),y=as.numeric(dat_train[,1]),alpha=0,family = "gaussian")
plot(fit_ridge)
pred <- predict(fit_ridge,as.matrix(dat_test[,-1]))
pred[pred>0.5]<-1
pred[pred<=0.5]<-0
TestError <- sum(pred!=dat_test[,1])/length(pred)
cat(TestError)
table(pred,dat_test$y)
