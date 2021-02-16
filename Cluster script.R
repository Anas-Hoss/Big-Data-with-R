
cluster.dt=read.csv("cluster.csv",header=F)
summary(cluster.dt)
plot(cluster.dt,pch=19,xlab="x1",ylab="x2",cex=1)

?kmeans
fit=kmeans(cluster.dt, 2)

fit<-kmeans(cluster.dt, 2, nstart = 10)

class(fit)
names(fit)
#fit$centers

points(fit$centers,pch=19,col="blue",cex=2)
points(cluster.dt,col=fit$cluster,pch=19)


fit$withinss
fit$tot.withinss
fit$iter

vec.k=seq(2,10)
vex.sse=rep(0,9)

for (i in 1:9){
  fit<-kmeans(cluster.dt, vec.k[i], nstart = 10)
  vex.sse[i]=fit$tot.withinss
}

plot(vec.k,vex.sse, type='b')


##############iris
iris.dt=iris[,1:4]

apply(iris.dt,2,sd)

apply(iris.dt,2,mean)

summary(iris.dt)
iris.dt=scale(iris.dt)
fix(iris)
apply(iris.dt,2,sd)

apply(iris.dt,2,mean)


vec.k=seq(2,10)
vex.sse=rep(0,9)
vex.bss=rep(0,9)

for (i in 1:9){
  fit<-kmeans(iris.dt, vec.k[i], nstart = 10)
  vex.sse[i]=fit$tot.withinss
  vex.bss[i]=fit$betweenss }


par(mfrow=c(1,2))
plot(vec.k,vex.sse, type='b')
plot(vec.k,vex.bss,type='b',col='red')

?kmeans


fit<-kmeans(iris.dt, 3, nstart = 30,iter.max=20)

table(fit$cluster,iris[,5])
x=data.frame(iris,fit$cluster)
fix(x)

?hclust
iris.hclust=hclust (dist (iris [,1:4]),method='ward.D') 
plot (iris.hclust)


