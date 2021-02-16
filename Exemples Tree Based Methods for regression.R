library(MASS)
boston.dt=Boston
dim(boston.dt)
names(boston.dt)
?Boston
boston.dt$chas=as.factor(boston.dt$chas)
boston.dt$rad=as.factor(boston.dt$rad)
set.seed(1)
train=sample(1:506,400)

library(randomForest)
boston.rf=randomForest(medv~lstat+rm,
                       data=boston.dt,subset=train,
                       importance=T)
boston.rf

sse.train=sum((predict(boston.rf,boston.dt[train,])-
    boston.dt[train,]$medv)^2)
sst.train=sum((boston.dt[train,]$medv-
  mean(boston.dt[train,]$medv))^2)
1-sse.train/sst.train

sse.test=sum((predict(boston.rf,boston.dt[-train,])-
                 boston.dt[-train ,]$medv)^2)
sst.test=sum((boston.dt[-train,]$medv-
                 mean(boston.dt[-train,]$medv))^2)
1-sse.test/sst.test

plot(boston.rf)

varImpPlot(boston.rf)

library(rpart)
boston.rpart=rpart(medv~lstat+rm,data=boston.dt)
par(xpd=T)
plot(boston.rpart)
text(boston.rpart,use.n=T,pretty=0)

library(gbm)

boston.gbm=gbm(medv~.,data=boston.dt[train,],
               distribution='gaussian')
boston.gbm
summary(boston.gbm)

plot(boston.gbm,i=c('rm','lstat'))

