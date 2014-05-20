housing <- read.table('http://www.jaredlander.com/data/housing.csv',sep=',',
                      header=TRUE,stringsAsFactors=FALSE)
names(housing) <- c('Neighborhood','Class','Units','YearBuilt','SqFt','Income','IncomePerSqFt','Expense',
                    'ExpensePerSqFt','NetIncome','Value','ValuePerSqFt','Boro')
housing <- housing[housing$Units < 1000,]
head(housing)

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro,data=housing)

require(coefplot)
coefplot(house1)

mean((housing$ValuePerSqFt-house1$fitted.values)^2)

house2 <- lm(ValuePerSqFt ~ Units*SqFt + Boro ,data=housing)
house3 <- lm(ValuePerSqFt ~ Units + SqFt*Boro + Class ,data=housing)
house4 <- lm(ValuePerSqFt ~ Units + SqFt*Boro + SqFt*Class ,data=housing)
house5 <- lm(ValuePerSqFt ~ Boro + Class ,data=housing)

multiplot(house1,house2,house3,house4,house5)

mse <- function(y,yhat){
  mean((y - yhat)^2)
}

mse(y=housing$ValuePerSqFt, yhat=house1$fitted.values)
mse(y=housing$ValuePerSqFt, yhat=house2$fitted.values)
mse(y=housing$ValuePerSqFt, yhat=house3$fitted.values)
mse(y=housing$ValuePerSqFt, yhat=house4$fitted.values)
mse(y=housing$ValuePerSqFt, yhat=house5$fitted.values)

AIC(house1,house2,house3,house4,house5)
BIC(house1,house2,house3,house4,house5)
anova(house1,house2,house3,house4,house5)

house1 <- glm(ValuePerSqFt ~ Units + SqFt + Boro,data=housing)
house2 <- glm(ValuePerSqFt ~ Units*SqFt + Boro ,data=housing)
house3 <- glm(ValuePerSqFt ~ Units + SqFt*Boro + Class ,data=housing)
house4 <- glm(ValuePerSqFt ~ Units + SqFt*Boro + SqFt*Class ,data=housing)
house5 <- glm(ValuePerSqFt ~ Boro + Class ,data=housing)

require(boot)
houseCV1 <- cv.glm(housing,house1, K=5)
houseCV2 <- cv.glm(housing,house2, K=5)
houseCV3 <- cv.glm(housing,house3, K=5)
houseCV4 <- cv.glm(housing,house4, K=5)
houseCV5 <- cv.glm(housing,house5, K=5)

houseCV1$delta
houseCV2$delta
houseCV3$delta
houseCV4$delta
houseCV5$delta


#' @title cv.work
#' @description Calculate cross-validation error
#' @return A single cross-validated error
#' @param fun The model function to call on the data
#' @param k The number of folds
#' @param data The data
#' @param cost Cost function for the error
#' @param response Character vector indicating which column is the response
#' @param \dots Extra arguments for model function
cv.work <- function(fun,k=5,data,cost,response='y', ...)
{
  #generate folds
  folds <- data.frame(Fold=sample(rep(1:k, length.out=NROW(data))),
                      Row=1:NROW(data))
  error <- 0
  
  for(f in 1:max(folds$Fold))
  {
    theRows <- folds$Row[folds$Fold == f]
    
    mod <- fun(data[-theRows,],...)
    pred <- predict(mod, data[theRows,])
    
    theCost <- cost(data[theRows, response],pred)
    error <- error + theCost*(length(theRows)/NROW(data))
  }
  return(error)
}

cv.work(fun=lm,k=5,data=housing,cost=mse,response='ValuePerSqFt',formula=ValuePerSqFt~Units+Boro)

