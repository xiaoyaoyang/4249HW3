#read data
setwd("/Users/Carol/Desktop/2014 Spring/Applied Data Science/team_27/FinalProject/Data")
features <- read.csv('features2.csv')
featuresnew <- features[,c(1,3,4,10,11)]
names(featuresnew)
featuresnew$Store <- as.character(featuresnew$Store)
#Determine number of clusters
wss <- (nrow(featuresnew)-1)*sum(apply(featuresnew,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(featuresnew, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#According to the plot, the best number is 5
fit <- kmeans(featuresnew, 3)
aggregate(featuresnew,by=list(fit$cluster),FUN=mean)
mydata <- data.frame(featuresnew, fit$cluster)
library(cluster)
clusplot(mydata,fit$cluster,color=TRUE, shade=TRUE,labels=2,lines=0)

#hierarchical Agglomerative
featuresnew[,1] <- as.character(featuresnew[,1])
d <- dist(featuresnew , method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

#We want to see the classification result when there are 45 clusters
#compared to the original cluster
fit2 <- kmeans(featuresnew, 45)
aggregate(featuresnew,by=list(fit2$cluster),FUN=mean)
mydata2 <- data.frame(featuresnew, fit2$cluster)

#Store 1
S1 <- mydata2[mydata2$Store==1,]
table1 <- table(pred=S1$fit2.cluster ,true=S1[,1])
a <- as.character(levels(as.factor(S1$fit2.cluster)))
lbls <- paste(a,"(",table1,")",sep="")
b <- as.numeric(a)
pie(table1,labels=a,col=rainbow(length(b)))

#Store2
S2 <- mydata2[mydata2$Store==2,]
table2 <- table(pred=S2$fit2.cluster ,true=S2[,1])
a <- as.character(levels(as.factor(S2$fit2.cluster)))
lbls <- paste(a,"(",table2,")",sep="")
b <- as.numeric(a)
pie(table2,labels=a,col=rainbow(length(b)))

#Store3
S3 <- mydata2[mydata2$Store==3,]
table3 <- table(pred=S3$fit2.cluster ,true=S3[,1])
a <- as.character(levels(as.factor(S3$fit2.cluster)))
lbls <- paste(a,"(",table3,")",sep="")
b <- as.numeric(a)
pie(table3,labels=a,col=rainbow(length(b)))

#Store 45
S45 <- mydata2[mydata2$Store==45,]
table45 <- table(pred=S45$fit2.cluster ,true=S45[,1])
a <- as.character(levels(as.factor(S45$fit2.cluster)))
lbls <- paste(a,"(",table45,")",sep="")
b <- as.numeric(a)
pie(table45,labels=a,col=rainbow(length(b)))

for(i in 1:45){ 
  S <- mydata2[mydata2$Store==i,]
  table <- table(pred=S$fit2.cluster ,true=S[,1])
  a <- as.character(levels(as.factor(S$fit2.cluster)))
  lbls <- paste(a,"(",table,")",sep="")
  b <- as.numeric(a)
  setwd("/Users/Carol/Desktop/2014 Spring/Applied Data Science/team_27/FinalProject/Results")
  name <- paste(i,".jpg")
  jpeg(file=name)
  c <- pie(table,labels=a,col=rainbow(length(b)))
  dev.off()  
}

#Why they have different result?

#Principal Component Analysis
featuresues <- featuresnew[,-1]
correlation <- cor(featuresues)
pca <- princomp(featuresues,cor=TRUE)
summary(pca)
plot(pca)
pca$scores
library(FactoMineR)
result <- PCA(featuresues)
#The first PCA only explain 33.6% of total variance, 3 PCAs explain 87.1% of total variance
#We can only use the former 4 variables

#SVM
library(e1071)
library(rpart)
head(featuresnew)
svm.model <- svm(Store ~ ., data = featuresnew, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, featuresnew[,-1],type="class")
table(pred=svm.pred,true=featuresnew[,1])

#factor analysis
featurematrix <- as.matrix(featuresues)
result1 <- factanal(featurematrix,factors=1)
l <- result1$loadings
featuresuesm <- as.matrix(featuresues)
score <- featuresuesm%*%l
#2 factors are enough for 4 variables, but we reject the null hypothesis that one factor is sufficient.
# factor analysis may not be applicable

# plot of relationships between different variables
library(psy) 
fpca(y=1,x=c(2:5),data=featuresnew)
corfeaturesnew <- as.data.frame(cor(featuresnew,method="spearman"))
fpca(y=1,x=c(2:5),data=corfeaturesnew,input="Cor",sample.size=60)
fpca(y=1,x=c(2,3,4),data=featuresues)
corfeaturesues <- as.data.frame(cor(featuresues,method="spearman"))
fpca(y=1,x=c(2:4),data=corfeaturesues,input="Cor",sample.size=60)
#

#simplify the problem by eliminating the effect of time
#within each store, summary each predictors using mean over periods of times

features <- featuresnew
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

data=cbind(Temp,Fuel,CPI,Un)

Store <- c(1:45)
data <- cbind(data,Store)

wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

fitd <- kmeans(data, 3)
aggregate(data,by=list(fitd$cluster),FUN=mean)
mydata <- data.frame(data, fitd$cluster)
library(cluster)
clusplot(mydata,fitd$cluster,color=TRUE, shade=TRUE,labels=2,lines=0)

setwd("/Users/Carol/Desktop/2014 Spring/Applied Data Science/team_27/FinalProject/Data")
store <- read.table('stores.csv',sep=',',header=T,stringsAsFactors=F)

c1 <- mydata$Store[mydata$fitd.cluster==1]
c2 <- mydata$Store[mydata$fitd.cluster==2]
c3 <- mydata$Store[mydata$fitd.cluster==3]

d1 <- store$Store[store$Type=="A"]
d2 <- store$Store[store$Type=="B"]
d3 <- store$Store[store$Type=="C"]

#These clusters represent different aspect from types


#hierarchical Agglomerative
data[,5] <- as.character(data[,5])
d <- dist(data , method = "euclidean") # distance matrix
fit <- hclust(d, method="average") 
plot(fit) # display dendogram
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=3, border="red")
cop <- cophenetic(fit)
cor(cop,d)


data[,5] <- as.numeric(data[,5])
datas <-as.data.frame(cbind(data[,5],groups))
names(datas) <- c("Store","Group")
datas[,2] <-as.character(datas[,2])
