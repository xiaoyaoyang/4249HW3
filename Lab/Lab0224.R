wine <- read.table('http://www.jaredlander.com/data/wine.csv',sep=',',header=T)
head(wine)
require(useful)
corner(wine)
wineTrain <- wine[,-1]
wineScale <- cmdscale(d=dist(wineTrain),k=2)
dim(wineScale)
plot(wineScale)

set.seed(86264)
wineK3 <- kmeans(x=wineTrain, centers=3, nstart=10)
plot(wineK3, data=wineTrain)
