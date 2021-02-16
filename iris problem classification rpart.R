library(MASS)

names(iris)

Data =iris

dim(Data)

set.seed(100)

indices = sample(1:150,100)

train = Data[indices,]

test= Data[-indices,]

library(rpart)

attach(train)

model_rf=rpart(Species~.,data=train)

summary(model_rf)

par(xpd=7)

plot(model_rf)

text(model_rf,use.n=7,pretty=0)

prediction=predict(model_rf,train, type="class")

prediction

R=table(predict(model_rf,train, type="class"),train$Species)

Good_per=sum(diag(R))/sum(R)

Good_per


prediction2=predict(model_rf,test, type="class")

R2=table(predict(model_rf,test, type="class"),test$Species)

Good_per2=sum(diag(R2))/sum(R2)

Good_per2

model_rf2=rpart(Species~.,data=Data)

summary(model_rf2)

Good_per

Good_per2
