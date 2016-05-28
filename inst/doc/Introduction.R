## ---- message=FALSE------------------------------------------------------
library(evclust)

## ------------------------------------------------------------------------
data(fourclass)
x<-fourclass[,1:2]
y<-fourclass[,3]
plot(x[,1],x[,2],pch=y,col=y,xlab=expression(x[1]),ylab=expression(x[2]))

## ------------------------------------------------------------------------
clus<-ecm(x,c=4,type='full',alpha=1,beta=2,delta=sqrt(20),disp=FALSE)

## ------------------------------------------------------------------------
summary(clus)

## ------------------------------------------------------------------------
clus<-ecm(x,c=4,type='pairs',alpha=1,beta=2,delta=sqrt(20),disp=FALSE)
summary(clus)

## ---- fig.width=6, fig.height=6------------------------------------------
clus<-ecm(x,c=4,type='pairs',alpha=1,beta=2,delta=sqrt(20),disp=FALSE)
plot(clus,x,mfrow=c(2,2),ytrue=y,approx=2)

