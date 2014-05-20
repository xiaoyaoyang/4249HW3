

#parallel processing
require(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
parLapply(cl=cl,theList,sum)
stopCluster(cl)

firstList <- list(A=matrix(1:16,4),B=matrix(1:16,2),c=1:5)
secondList <- list(A=matrix(1:16,4),B=matrix(1:16,8),c=15:1)
mapply(identical,firstList,secondList)

simplefunc <- function(x,y)
{
    NROW(x) + NROW(y)
}

mapply(simplefunc, firstList, secondList)

require(ggplot2)
data(diamonds)

head(diamonds)

aggregate(price ~ cut, diamonds, mean)
aggregate(price ~ cut + color, diamonds, mean)
aggregate(cbind(price, carat)~ cut, diamonds,mean)

require(plyr)
head(baseball)
baseball
baseball$sf[baseball$year < 1954] <- 0
baseball$hbp[is.na(baseball$hbp)] <- 0
baseball <- baseball[baseball$ab >=50,]

baseball$OBP <- with(baseball, (h + bb + hbp)/ (ab + bb + hbp + sf))
head(baseball)

obp <- function(data)
{
    c(OBP=with(data, sum(h + bb + hbp)/sum(ab + bb + hbp + sf)))
}

careerOBP <- ddply(baseball,'id',obp)
head(careerOBP,10)

aggregate(price ~ cut, diamonds,each(mean,median))

codes <- read.table('http://www.jaredlander.com/data/countryCodes.csv',
                    header=TRUE,sep=',',stringsAsFactors=FALSE)
countries <- read.table('http://www.jaredlander.com/data/GovType.csv',
                        header=TRUE,sep=',',stringsAsFactors=FALSE)

codes <- rename(codes, c(Country.name='Country'))
countryJoined <- join(x=codes, y=countries, by='Country')
head(countryJoined)
View(countryJoined)

require(reshape2)
head(airquality)

airMelt <- melt(airquality, id=c('Month','Day'),value.name='Value',
                variable.name='Metric')
head(airMelt)


airCast <- dcast(airMelt, Month + Day ~ Metric, value.var='Value')
head(airCast)
