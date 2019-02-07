rm(list = ls())
library(car)
fat = read.csv('~/Desktop/628/1/BodyFat.csv')[,-1]

---#第一部分：数据清洗#---
attach(fat)

##首先看bodyfat结果与实际density之间的关系
B = 495*(1/DENSITY)-450
plot(B-BODYFAT)
text(1:252, B-BODYFAT, 1:252)
###96,48,76,182突出

###根据density数据修改这四项数据
detach(fat)
fat1 = fat
fat1[96,1] = 495*(1/fat1[96,2])-450
fat1[48,1] = 495*(1/fat1[48,2])-450
fat1[76,1] = 495*(1/fat1[76,2])-450
fat1[182,1] = 495*(1/fat1[182,2])-450
B = 495*(1/fat1[,2])-450
plot(B-fat1[,1])

###发现修改过后182的BODYFAT直接小于零了，倾向于认为DENSITY有可能记录出错，此时
###无法判断BODYFAT数据是否正确，于是倾向于删除四条记录
fat2 = fat[-c(96,48,76,182),]


##接下来看各项数据有没有奇怪的

summary(fat2)
###weight,bmi,neck,abdomen,hip,thigh,knee,ankle,biceps均有突出的大数据
hist(fat2$ANKLE)

###画出纵向折线图对整个数据趋势有个了解
plot(1:15,fat2[1,-c(2)], type = 'b', col=1, ylim = c(-10,350))
for(i in 2:252){
  lines(1:15,fat2[i,-c(2)], type = 'b', col=i)
}

###突出大数据应该都属于一个人，找出weight最大的大佬
which(fat2$WEIGHT == max(fat2$WEIGHT))
fat2[39,]
###自变量X的数据都很突出，但BODYFAT倒不是很突出，应该是一个奇怪的特例，故删除
fat3 = fat2[-39,]

###之前还发现height有一个极小点，折线图也可以看出来
###由bmi数据查看weight与height是否正确
plot(fat3$HEIGHT^2,fat3$WEIGHT/fat3$ADIPOSITY)
text(fat3$HEIGHT^2,fat3$WEIGHT/fat3$ADIPOSITY,1:252)
fat3[41,]

###明显判断出是height计算出错，构建出bmi的计算公式：
fat4 = fat3[-41,]
H_2 = fat4$HEIGHT^2
W_BMI = fat4$WEIGHT/fat4$ADIPOSITY
h2_lm = lm(H_2~W_BMI)
summary(h2_lm)
new_wbmi = data.frame(W_BMI = fat3$WEIGHT[41]/fat3$ADIPOSITY[41])
new_height = sqrt(predict(h2_lm, new_wbmi))
fat3$HEIGHT[41] = new_height

plot(fat3$HEIGHT^2,fat3$WEIGHT/fat3$ADIPOSITY)
text(fat3$HEIGHT^2,fat3$WEIGHT/fat3$ADIPOSITY,1:252)

###159,216?疑问点

###AGE?我倾向于不删
which(fat3$AGE == max(fat3$AGE))

###再画一次折线图看看现在的情况
plot(1:15,fat3[1,-c(2)], type = 'b', col=1, ylim = c(-10,350))
for(i in 2:252){
  lines(1:15,fat3[i,-c(2)], type = 'b', col=i)
}

### 现在做一个lm看看有没有明显脱离线性关系的点
fat3 = fat3[,-2]
fat3_lm <- lm(BODYFAT~., data = fat3)
summary(fat3_lm)
plot(fat3_lm)
fat3[86,]

vif(fat3_lm)

write.csv(fat3,'data_cleaned.csv')

### 没有什么特别异常的点了，不放心的话可以做一个outliertest

#mallow's cp
lm2=lm(BODYFAT~., data=fat3)
X = model.matrix(lm2)[,-1]
Y = fat3[,1]
library(leaps)
library(faraway)
g=leaps(X,Y)
layout(matrix(1:1,ncol=1))
Cpplot(g)
g=leaps(X,Y,nbest=1)
layout(matrix(1:1,ncol=1))
Cpplot(g)  #1 3 6 7 12 14

cp.choice=c(1,3,6,7,12,14)+1
model.cp=lm(BODYFAT~.,data=fat3[,c(1,cp.choice)])
summary(model.cp) #0.7357

#adjusted r2
g=leaps(X,Y,nbest=1,method='adjr2')
plot(g$adjr2)
(g$which)[which(g$adjr2==max(g$adjr2)),]
r2.choice=c(2,5:10,14:15)
lm5=lm(BODYFAT~., data=fat3[,c(1,r2.choice)])
summary(lm5) #0.738

AIC(model.cp)
BIC(model.cp)

model.AIC=step(lm2,k=2)
summary(model.AIC) #0.7374

model.BIC=step(lm2,k=log(247))
summary(model.BIC) #0.726

#aic
base=lm(BODYFAT~1, data=fat3)
aic.base=step(base,direction='both', scope = list(lower=~1,upper=lm2),trace=T)
summary(aic.base) #0.7319
#bic
bic.base=step(base,direction='both', scope = list(lower=~1,upper=lm2), trace=T,k=log(247))
summary(bic.base) #0.7291
confint(bic.base)

library(lars)
x=as.matrix(fat3[,r2.choice])
y=as.matrix(fat3[,1])
laa=lars(x,y,type='lar') # R-squared: 0.753 
plot(laa)
summary(laa) #ABDOMEN AGE WRIST


