library(ISLR)
library(MASS)
library(class)

?Default
data(Default)
summary(Default)
attach(Default)
names(Default)

#Logistic Regression LR :
glm.fits=glm(default ~ student + balance+ income,family ="binomial", data=Default)
print(glm.fits)
summary (glm.fits)
names(glm.fits)
coef(glm.fits)
sum(residuals(glm.fits)^2)
deviance(glm.fits)
summary (glm.fits)$coef
summary (glm.fits)$coef [,4]
glm.probs =predict(glm.fits,type = "response")
glm.probs [1:10]
contrasts (default)
glm.pred=rep("No",10000)
glm.pred[glm.probs>.5]="Yes"
table(glm.pred,default)
mean(glm.pred==default)

#LR test :
test=1:1000
glm.fits2 =glm(default~student+balance+income,family="binomial",data=Default,subset=-test)
summary(glm.fits2)
print(glm.fits2)
glm.probs2=predict(glm.fits2,Default[test,],type="response")
glm.pred2=rep("No",1000)
glm.pred2[glm.probs2>.5]="Yes"
table(glm.pred2,default[test])
mean(glm.pred2==default[-test])


## for student+income
test=1:1000
glm.fits3 =glm(default~student+income,family="binomial",data=Default,subset=-test)
summary(glm.fits3)
print(glm.fits3)
glm.probs3=predict(glm.fits3,Default[test,],type="response")
glm.pred3=rep("No",1000)
glm.pred3[glm.probs3>.5]="Yes"
table(glm.pred3,default[test])
mean(glm.pred3==default[-test])


### for student + balance
test=1:1000
glm.fits4 =glm(default~student+balance,family="binomial",data=Default,subset=-test)
summary(glm.fits4)
print(glm.fits4)
glm.probs4=predict(glm.fits4,Default[test,],type="response")
glm.pred4=rep("No",1000)
glm.pred4[glm.probs4>.5]="Yes"
table(glm.pred4,default[test])
mean(glm.pred4==default[-test])


##############################################################################################################
#Linear Discriminant Analysis LDA :
lda.fits=lda(default~student+balance+income,subset=-test)
lda.fits
plot(lda.fits)
lda.pred=predict(lda.fits, Default[test,])
names(lda.pred)
lda.class=lda.pred$class
lda.class
default[test]
table(lda.class,default[test])
mean(lda.class==default[test])

sum(lda.pred$posterior [ ,1] >=.5)
sum(lda.pred$posterior [,1]<.5)
lda.pred$posterior [1:20 ,1]
plot(lda.class)

lda.class [1:20]
sum(lda.pred$posterior [,1]>.9)
sum(lda.pred$posterior [,1]<.1)
plot(lda.class)

############################################################################################################
#Quadratic Discriminant Analysis QDA :
qda.fits=qda(default~student+balance+income,data=Default ,subset=-test)
qda.fits
qda.pred=predict(qda.fits, Default[test,])
qda.class=qda.pred$class
qda.class
plot(qda.class)
table(qda.class,default[test])
mean(qda.class==default[test])

###########################################################################################################
#K-Nearest Neighbors KNN :

set.seed(1)
test=1:1000
train.X=cbind(student,balance,income)[-test,]
test.X=cbind(student,balance,income)[test,]
default.X=default[-test]

knn.cl=knn(train.X,test.X,default.X,k=1)
summary(knn.cl)
table(knn.cl,default[test])
mean(default[test]==knn.cl)
plot(knn.cl)

knn.cl2=knn(train.X,test.X,default.X,k=3)
summary(knn.cl2)
table(knn.cl2,default[test])
mean(default[test]==knn.cl2)
plot(knn.cl2)

knn.cl3=knn(train.X,test.X,default.X,k=5)
summary(knn.cl3)
table(knn.cl3,default[test])
mean(default[test]==knn.cl3)
plot(knn.cl3)

