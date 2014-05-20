require(glmnet)
acs <- read.table('http://www.jaredlander.com/data/acs_ny.csv',
                  sep=',',header=T,stringsAsFactors=F)

dim(acs)
?glmnet
response ~ 
  
require(ggplot2)
ggplot(acs,aes(x=FamilyIncome)) + geom_density(fill='grey50') +
  geom_vline(xintercept=150000)

acs$Income <- (acs$FamilyIncome >=150000)

require(useful)
theFormula<-Income ~ NumBedrooms +NumChildren + NumPeople + 
  NumRooms + NumVehicles + NumWorkers + OwnRent + YearBuilt+ 
  ElectricBill + FoodStamp + HeatingFuel + Insurance + Language - 1
#what this do?
acsX <- build.x(formula=theFormula, data=acs, contrasts=FALSE)

class(acsX)
dim(acsX)

acsY <- build.y(theFormula, data=acs)

corner(acsX)
head(acsX)

acs1 <- glmnet(x=acsX,y=acsY,family='binomial',alpha=0)
plot(acs1)
acsCV1<- cv.glmnet(x=acsX,y=acsY,family='binomial',alpha=0,nfold=5)
plot(acsCV1)
acsCV1$lambda.min
acsCV1$lambda.1se

plot(acsCV1$glmnet.fit)
abline(v=c(acsCV1$lambda.min,acsCV1$lambda.1se))

acs2 <- glmnet(x=acsX,y=acsY,family='binomial',alpha=1)
acsCV2<- cv.glmnet(x=acsX,y=acsY,family='binomial',alpha=1,nfold=5)
plot(acsCV2)
acsCV2$lambda.1se-acsCV2$lambda.min
sd(acsCV2$lambda)


######paste
paste('hello',c('1','2'),'3')
person<-'David'
partySize <- 'eight'
waitTime <-25

paste('Hello ',person,', your party of ',partySize,' will be seated in ',waitTime, 'minutes.',sep='')
sprintf('Hello %s, your party of %s will be seated in %s minutes.',person,partySize,waitTime)


require(XML)
presidents <- readHTMLTable('http://www.loc.gov/rr/print/list/057_chron.html',as.data.frame=TRUE,skip.rows=1,stringsAsFactors=FALSE,which=3,
                            header=TRUE)
head(presidents)
presidents[,2]
