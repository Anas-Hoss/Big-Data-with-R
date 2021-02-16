library(rpart)

iris.dt=iris
set.seed(101)
train=sample(1:150,100)
#train


#fix(iris.dt)
summary(iris.dt)

iris.rpart=rpart(Species~.,data=iris.dt,subset=train)

iris.rpart

par(xpd=T)
plot(iris.rpart)
text(iris.rpart,use.n=T)

iris.pred.train=predict(iris.rpart,iris.dt[train,],type="class")
table(iris.dt[train,]$Species,iris.pred.train)
sum(diag(table(iris.dt[train,]$Species,iris.pred.train)))/100


iris.pred.test=predict(iris.rpart,iris.dt[-train,],type="class")
table(iris.dt[-train,]$Species,iris.pred.test)
sum(diag(table(iris.dt[-train,]$Species,iris.pred.test)))/50

#Construction d'un model random Forest
library("randomForest")
iris.rf=randomForest(Species~.,
                     data=iris.dt,subset=train,
                     importance=T)

print(varImpPlot(iris.rf))

iris.rf
plot(iris.rf)

?randomForest







