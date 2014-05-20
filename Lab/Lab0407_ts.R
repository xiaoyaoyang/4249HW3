require(WDI)
gdp <- WDI(country=c('US','CA','CN','JP','SG','IL','GB'),
           indicator=c('NY.GDP.PCAP.CD','NY.GDP.MKTP.CD'),
           start=1960,end=2011)
names(gdp) <- c('iso2c','Country','Year','PerCapGDP','GDP')

require(ggplot2)
ggplot(gdp,aes(Year,PerCapGDP, color=Country, linetype=Country)) +
    geom_line() + scale_y_continuous(label=dollar)

ggplot(gdp,aes(Year,GDP, color=Country, linetype=Country)) +
    geom_line() + scale_y_continuous(label=dollar)

us <- gdp$PerCapGDP[gdp$Country =='United States']
us





acf(us)
pacf(us)

install.packages('forecast')
require(forecast)
usBest <- auto.arima(us)
usBest
plot(usBest$residuals)
hist(usBest$residuals)
acf(us est$residuals)

predict()


require(vars)
head(gdp)

require(reshape2)
gdpCast <- dcast(Year ~ Country, data=gdp, value.var='PerCapGDP')
head(gdpCast)

gdpTS <- ts(gdpCast[,-1], start=min(gdpCast$Year), end=max(gdpCast$Year))
head(gdpTS)
plot(gdpTS,plot.type='single',col=1:8)

numDiffs <- ndiffs(gdpTS)
numDiffs
gdpDiffed <- diff(gdpTS, differences=1)
plot(gdpDiffed, plot.type='single',col=1:7)

gdpVar <- VAR(gdpDiffed, lag.max=12)
gdpVar
length(gdpVar)
length(gdpVar$varresult)

require(coefplot)
coefplot(gdpVar$varresult$United.States)
class(gdpVar$carresult$United.States)


#GARCH
require(quantmod)
att <- getSymbols('T', auto.assign=FALSE)


#require(xts)



#require(rugarch)
attSpec <- ugarchspec(variance.model=list(model='sGARCH',
                                          garchOrder=c(1,1)))


