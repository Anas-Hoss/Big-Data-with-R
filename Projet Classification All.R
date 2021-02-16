library(MASS)
library(ISLR)
library(rpart)
library(class)
library(randomForest)

#Importation de la base de donnée :
default=read.csv("C:/Users/hossi/Desktop/CCdefault.csv")
default
View(default)
#Exploration de données :
names(default)
summary(default)
default$default.payment.next.month = as.factor(default$default.payment.next.month)
default$PAY_0 = as.factor(default$PAY_0)
default$PAY_2 = as.factor(default$PAY_2)
default$PAY_3 = as.factor(default$PAY_3)
default$PAY_4 = as.factor(default$PAY_4)
default$PAY_5 = as.factor(default$PAY_5)
default$PAY_6 = as.factor(default$PAY_6)
default$EDUCATION = as.factor(default$EDUCATION)
default$MARRIAGE = as.factor(default$MARRIAGE)
default$SEX = as.factor(default$SEX)
par(mfrow=c(2,2))
plot(default$default.payment.next.month~default$SEX)
plot(default$default.payment.next.month~default$EDUCATION)
plot(default$default.payment.next.month~default$MARRIAGE)
plot(default$default.payment.next.month~default$AGE)

#Logistic Regression
glm.fit=glm(default$default.payment.next.month~.,data=default,family=binomial)
print(glm.fit)
summary(glm.fit)
names(glm.fit)
coef(glm.fit)
sum(residuals(glm.fit)^2)
deviance(glm.fit)
summary(glm.fit)$coef
glm.probs=predict(glm.fit,type="response")
predict(glm.fit,type="link")[1:10]
log(glm.probs/(1-glm.probs))[1:10]
glm.logit=predict(glm.fit,type="link")[1:10]
glm.probs[1:10]
contrasts(default$default.payment.next.month)
glm.pred=ifelse(glm.probs>0.5,"1","0")
glm.pred[1:10]
glm.pred=rep("0",1250)
glm.pred[glm.probs>.5]="1"
glm.pred
table(glm.pred,default$default.payment.next.month)
table(default$default.payment.next.month,glm.pred)
prop.table(table(glm.pred,default$default.payment.next.month),2)
(812+654)/2000

#Decision Tree
set.seed(5231)
ind=sample(1:2000,1400)
default.train=default[ind,]
default.test=default[-ind,]
dim(default.train)
names(default.train)
dim(default.test)
names(default.test)
default.rpart=rpart(default.train$default.payment.next.month~.,data=default.train)
par(xpd=T)
plot(default.rpart,main="Arbre de classification")
text(default.rpart,use.n=T,pretty=0)
default.predict=predict(default.rpart,default.train,type = "class")
table(default.train$default.payment.next.month,default.predict)
mean(default.train$default.payment.next.month==default.predict)
default.predict.test=predict(default.rpart,default.test,type = "class")
table(default.test$default.payment.next.month,default.predict.test)
mean(default.test$default.payment.next.month==default.predict.test)

# Random Forest
default.rf=randomForest(default.train$default.payment.next.month~., data = default.train)
pred.train=predict(default.rf,default.train,type="class")
table(default.train$default.payment.next.month,pred.train)
mean(default.train$default.payment.next.month==pred.train)
predict.rf=predict(default.rf,default.test,type = "class")
table(default.test$default.payment.next.month,predict.rf)
mean(default.test$default.payment.next.month==predict.rf)
varImpPlot(default.rf)
