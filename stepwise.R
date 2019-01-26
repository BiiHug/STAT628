data = read.csv("BodyFat.csv")
data = data[,-1]
str(data)
model = lm(BODYFAT~.,data = subset(data,select = -DENSITY))
summary(model)

###data clean
###leverages > 4/(n-p)
plot(model,which = 4)
abline(h = 4/(252-16),lty = 2)
##[39,42,86] 
summary(model <- lm(BODYFAT~.,data = data[-39,-2]))
layout(matrix(1:4,ncol = 2))
plot(model)
##[42,86]
summary(model <- lm(BODYFAT~.,data = data[-c(39,42),-2]))
layout(matrix(1:4,ncol = 2))
plot(model)

##[224,207] outliers
library(car)
outlierTest(model)
##224 is an outlier

##cp
model = lm(BODYFAT~.,data = data[-c(39,42,86,207,224),-2])
X = model.matrix(model)[,-1]
Y = data[-c(39,42,86,207,224),1]
library(leaps)
library(faraway)
g = leaps(X,Y)
layout(matrix(1))
Cpplot(g)
g = leaps(X,Y,nbest = 1)
Cpplot(g)
##1,2,7,9,13,14
cp.choice = c(1,2,7,9,13,14)+2
summary(model.cp <- lm(BODYFAT~.,data = data[-c(39,42,86,207,224),c(1,cp.choice)]))

##adjusted R^2
g = leaps(X,Y,nbest = 1,method = "adjr2")
plot(g$adjr2)
(g$which)[which(g$adjr2 == max(g$adjr2)),]
r2.choice = c(3,5,7:11,15:16)
summary(model.r2 <- lm(BODYFAT~.,data = data[-c(39,42,86,207,224),c(1,r2.choice)]))

##AIC\BIC
model.AIC = step(model,k = 2)
summary(model.AIC)
model.BIC = step(model,k = log(248))
summary(model.BIC)
vif(model.BIC)
  