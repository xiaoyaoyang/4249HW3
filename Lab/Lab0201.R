require(ggplot2)
data(diamonds)
head(diamonds)
ggplot(diamonds) + geom_point(aes(x=carat,y=price))
g<-ggplot(diamonds) + geom_point(aes(x=carat,y=price,color=color))
g + scale_color_brewer()
require(ggthemes)
g + theme_economist() +  scale_color_economist()
g + theme_wsj()
g + theme_tufte()
g + theme_excel()

g + facet_wrap(~color)
g + facet_grid(cut ~ clarity)

ggplot(diamonds,aes(x=price))+geom_histogram()
ggplot(diamonds,aes(x=price))+geom_histogram(aes(fill=color))
ggplot(diamonds,aes(x=price))+geom_histogram(aes(fill=color),position=position_dodge()) 
ggplot(diamonds,aes(x=price))+geom_density(fill='grey50')
ggplot(diamonds,aes(x=price))+geom_density(aes(fill=color,color=color),alpha=1/2) #alpha->transparent

ggplot(diamonds,aes(y=price,x=1))+geom_boxplot()
ggplot(diamonds,aes(y=price,x=color))+geom_boxplot()
ggplot(diamonds,aes(y=price,x=color))+geom_violin()
ggplot(diamonds,aes(y=price,x=color))+geom_violin()+geom_jitter()
ggplot(diamonds,aes(y=price,x=color))+geom_jitter()+geom_violin() # order matter!!!

g3<-ggplot(diamonds,aes(y=price,x=color))+geom_jitter()+geom_violin(alpha=1/2) 
g3 + xlab('HUE')
g3 + labs(x='Hue',y='Cost',title='')







