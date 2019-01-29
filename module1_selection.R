rm(list=ls())
bodyfat = read.csv("~/Desktop/628/1/BodyFat.csv")
attach(bodyfat)

B=495*(1/DENSITY)-450
plot(B-BODYFAT)

plot(1/DENSITY,BODYFAT) #positive correlation

bodyfat0=bodyfat[,-c(1,3)]  #remove density index
str(bodyfat0)
lm0=lm(BODYFAT~., data=bodyfat0)
summary(lm0)
plot(lm0,which=4) #39, 42, 86
abline(h=4/(252-15),lty=2) #leverages > 4/(n-p)

##data cleaning

bodyfat0[39,] #weight 363.15
lm1=lm(BODYFAT~., data=bodyfat0[-39,])
summary(lm1) #0.7391
layout(matrix(1:4,ncol=2))
plot(lm1)

bodyfat0[c(42,86),] #high leverage
bodyfat0[c(207,224),] #possible outliers

plot(lm1,which=4)
abline(h=4/(251-15),col='red',lty=2)
lm2=lm(BODYFAT~., data=bodyfat0[-c(39,42),])
summary(lm2) #0.7364
layout(matrix(1:4,ncol=2))
plot(lm2)

#86, 207, 224
library(car)
outlierTest(lm2)
# 224 is an outlier

## mallow's cp
lm2=lm(BODYFAT~., data=bodyfat0[-c(39,42,224),])
X = model.matrix(lm2)[,-1]
Y = bodyfat0[-c(39,42,224),1]
library(leaps)
library(faraway)
g=leaps(X,Y)
layout(matrix(1:1,ncol=1))
Cpplot(g)
g=leaps(X,Y,nbest=1)
layout(matrix(1:1,ncol=1))
Cpplot(g) 

#(1,3,6,7,13,14)
cp.choice=c(1,3,6,7,13,14)+1
lm3=lm(BODYFAT~., data=bodyfat0[-c(39,42,224),c(1,cp.choice)])
summary(lm3) #0.7404

#(1,2,3,4,5,6,7,8,9,11,12,13,14)
cp.choice=c(1,2,3,4,5,6,7,8,9,11,12,13,14)+1
lm4=lm(BODYFAT~., data=bodyfat0[-c(39,42,224),c(1,cp.choice)])
summary(lm4) #0.7417

## adjust R^2
g=leaps(X,Y,nbest=1,method='adjr2')
plot(g$adjr2)
(g$which)[which(g$adjr2==max(g$adjr2)),]
r2.choice=c(2,4,6:10,14:15)
lm5=lm(BODYFAT~., data=bodyfat0[-c(39,42,224),c(1,r2.choice)])
summary(lm5) #0.7433

## aic/bic
AIC(lm4) #1398.752
BIC(lm4) #1451.513
AIC(lm5) #1393.383
BIC(lm5) #1432.075

model.AIC=step(lm2,k=2)
summary(model.AIC) #0.743
aic.choice=c(2,5:10,14,15)

model.BIC=step(lm2,k=log(249))
summary(model.BIC) #0.7367
bic.choice=c(2,5,7,8,15)

base=lm(BODYFAT~1, data=bodyfat0)
aic.base=step(base,direction='both', scope = list(lower=~1,upper=lm2),trace=T)
summary(aic.base) #0.7383

bic.base=step(base,direction='both', scope = list(lower=~1,upper=lm2),trace=T,k=log(249))
summary(bic.base) #0.7308
