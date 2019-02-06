rm(list = ls())
library(car)
fat = read.csv('/Applications/Study/UWM/628/module1/BodyFat.csv')[,-1]

# (1) clean the data:

## See if any bodyfat observations disobey the siri equation:
attach(fat)
B = 495*(1/DENSITY)-450
plot(B-BODYFAT,
     xlab = 'observations', ylab = 'B-BODYFAT(%)',
     main = "siri's equation")
text(1:252, B-BODYFAT, 1:252)
detach(fat)

### 96,48,76,182 seem to be weird
### We firstly choose to believe in the density record, and try to recalculate bodyfat using density records:

fat1 = fat
fat1[96,1] = 495*(1/fat1[96,2])-450; fat1[48,1] = 495*(1/fat1[48,2])-450
fat1[76,1] = 495*(1/fat1[76,2])-450; fat1[182,1] = 495*(1/fat1[182,2])-450
fat1[96,1]; fat1[48,1]; fat1[76,1]; fat1[182,1]

### However, after recalculation, the percent bodyfat of NO.182 is smaller than 0%, in this case we
###can't tell which one is wrong, thus we choose to delete these four points:
fat2 = fat[-c(96,48,76,182),]


## See if there is any weird observations in each variable:
summary(fat2)

### We find that most of the variables get one 'stick out' observation, besides we also find that
###they all belong to the same people, which is more obverious using this line chart:
plot(1:15,fat2[1,-c(2)], type = 'b', col=1, ylim = c(-10,350),
     xlab= 'variables', ylab = 'observed data',
     main = 'Observations from 248 people')
for(i in 2:252){
  lines(1:15,fat2[i,-c(2)], type = 'b', col=(i+2))
}

### This 'stick-out' guy turns out to be NO.39, have a close look at him:
which(fat2$WEIGHT == max(fat2$WEIGHT))
fat2[39,]

### The weird thing is that the percent body fat of this 'stick-out' guy is surprisingly not 
###so extraodinary:
fat2$BODYFAT[39]; summary(fat2$BODYFAT)

### In this case we think this 'stick-out' guy would defenitely cause huge interference on the
###model, thus we choose to delete it:
fat3 = fat2[-39,]

### From the line charts before we also find there is an extremely small point at the 'HEIGHT' 
###variable. Cause already have the variable 'ADIPOSITY'(bmi) we try to see if it obey the 
###relationship between WEIGHT, HEIGHT and ADIPOSITY:
plot(fat3$HEIGHT^2,fat3$WEIGHT/fat3$ADIPOSITY,
     xlab = 'Height^2(inches)',
     ylab = 'Weight/Adiposity',
     main = 'Linear relationship between Height^2 & Weight/Adiposity'
     )
text(fat3$HEIGHT^2,fat3$WEIGHT/fat3$ADIPOSITY,1:252)
fat3$HEIGHT[41]

### the outlier NO.41 turns out to be the extremely 'short' guy, we choose to recalculate his
###height by constructing the linear relationship between HEIGHT^2 and WEIGHT/ADIPOSITY:
fat4 = fat3[-41,]
H_2 = fat4$HEIGHT^2
W_BMI = fat4$WEIGHT/fat4$ADIPOSITY
h2_lm = lm(H_2~W_BMI)
summary(h2_lm)
new_wbmi = data.frame(W_BMI = fat3$WEIGHT[41]/fat3$ADIPOSITY[41])
new_height = sqrt(predict(h2_lm, new_wbmi))
fat3$HEIGHT[41] = new_height

### Now we draw another line charts based on fat3:
plot(1:15,fat3[1,-c(2)], type = 'b', col=1, ylim = c(-10,350),
     xlab= 'variables', ylab = 'observed data',
     main = 'Observations from 247 people')
for(i in 2:252){
  lines(1:15,fat3[i,-c(2)], type = 'b', col=i)
}

### There is no more 'stick-out'points

### We also did a simple linear regression to see if there is any observation of strong
###influence:
fat3 = fat3[,-2]
fat3_lm <- lm(BODYFAT~., data = fat3)
summary(fat3_lm)
plot(fat3_lm, which = 1); plot(fat3_lm, which = 5)
fat3[86,]; fat3[224,]

### No one seems to be extremely weird, thus fat3 becomes our final cleaned-up data set.



# (2) model building

## There're too many variables, also severe multicollinearity problem exists, which means that 
##variable selection is needed:
VIF3 <- vif(fat3_lm); VIF3


## We use the basic four methods: Mellow's Cp, Adj-R^2, AIC, and BIC to help us narrow our
##selection:

### Mellow's Cp
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

### Adj-R^2
g=leaps(X,Y,nbest=1,method='adjr2')
plot(g$adjr2)
(g$which)[which(g$adjr2==max(g$adjr2)),]
r2.choice=c(2,5:10,14:15)
lm5=lm(BODYFAT~., data=fat3[,c(1,r2.choice)])
summary(lm5) #0.738

### AIC
base=lm(BODYFAT~1, data=fat3)
aic.base=step(base,direction='both', scope = list(lower=~1,upper=lm2),trace=T)
summary(aic.base) #0.7319

### BIC
bic.base=step(base,direction='both', scope = list(lower=~1,upper=lm2), trace=T,k=log(247))
summary(bic.base) #0.7291
confint(bic.base)

## Then we use cross validation to help us judge the performance of these four 'best' models 
##based on RMSE.

### For CV, we try 1000 times for each model. Each time we select 200 observations out of 247 as the
###train model, and the remaining 47 ones as the test model. Then rebuild the linear regression
###models using the variables selected by each methods:

n_all <- nrow(fat3)
n_train <- 200
n_test <- n_all - n_train

### Mellow's Cp
RMSE1 <- c()
for(i in 1:1000){
  row_train <- sample(1:n_all, n_train)
  data_train <- fat3[row_train,]
  fat_lm_train <- lm(BODYFAT~AGE+HEIGHT+CHEST+ABDOMEN+BICEPS+WRIST, data = data_train)
  data_test <- fat3[-row_train,]
  
  sum_err2 <- 0
  for(j in 1:n_test){
    pre <- predict(fat_lm_train, data_test[j,-1])
    err <- abs(data_test[j,1] - pre)
    sum_err2 <- sum_err2 + err^2
  }
  rmse <- sqrt(sum_err2/(n_test-1))
  RMSE1[i] <- rmse
}
mean(RMSE1)

###adj-R2
RMSE2 <- c()
for(i in 1:1000){
  row_train <- sample(1:n_all, n_train)
  data_train <- fat3[row_train,]
  fat_lm_train <- lm(BODYFAT~AGE+ADIPOSITY+NECK+CHEST+ABDOMEN+HIP+THIGH+FOREARM+WRIST, data = data_train)
  data_test <- fat3[-row_train,]
  
  sum_err2 <- 0
  for(j in 1:n_test){
    pre <- predict(fat_lm_train, data_test[j,-1])
    err <- abs(data_test[j,1] - pre)
    sum_err2 <- sum_err2 + err^2
  }
  rmse <- sqrt(sum_err2/(n_test-1))
  RMSE2[i] <- rmse
}
mean(RMSE2)

###AIC
RMSE3 <- c()
for(i in 1:1000){
  row_train <- sample(1:n_all, n_train)
  data_train <- fat3[row_train,]
  fat_lm_train <- lm(BODYFAT~ABDOMEN+WEIGHT+WRIST+BICEPS, data = data_train)
  data_test <- fat3[-row_train,]
  
  sum_err2 <- 0
  for(j in 1:n_test){
    pre <- predict(fat_lm_train, data_test[j,-1])
    err <- abs(data_test[j,1] - pre)
    sum_err2 <- sum_err2 + err^2
  }
  rmse <- sqrt(sum_err2/(n_test-1))
  RMSE3[i] <- rmse
}
mean(RMSE3)

###BIC
RMSE4 <- c()
for(i in 1:1000){
  row_train <- sample(1:n_all, n_train)
  data_train <- fat3[row_train,]
  fat_lm_train <- lm(BODYFAT~ABDOMEN+WEIGHT+WRIST, data = data_train)
  data_test <- fat3[-row_train,]
  
  sum_err2 <- 0
  for(j in 1:n_test){
    pre <- predict(fat_lm_train, data_test[j,-1])
    err <- abs(data_test[j,1] - pre)
    sum_err2 <- sum_err2 + err^2
  }
  rmse <- sqrt(sum_err2/(n_test-1))
  RMSE4[i] <- rmse
}
mean(RMSE4)

### We find that these four models have very similar performance (0.01% of bodyfat really 
###doesn't matter for normal people). The model select by BIC methods have fewest variables,
###thus we choose it as our basic model.

fat3_lm_bic <- lm(BODYFAT~ABDOMEN+WEIGHT+WRIST, data = fat3)
summary(fat3_lm_bic)

## The BIC method choose these three variables: WEIGHT, ABDOMEN, and WRIST. Based on the 
##reasons listed below, we choose to delete the variable WRIST:

### 1. The variable ABDOMEN is defenitely of decisive importance of the whole dataset, all
###the other variables are just playing the role of adjusting, optimizing:

#### (1) ABDOMEN appears in all four selected models, with broad range and large coefficient:
#### For example, in the modle selected by BIC: 
fat3_lm_bic$coefficients[2]*(quantile(fat3$ABDOMEN,3/4)-quantile(fat3$ABDOMEN,1/4))
fat3_lm_bic$coefficients[3]*(quantile(fat3$WEIGHT,3/4)-quantile(fat3$WEIGHT,1/4))
fat3_lm_bic$coefficients[4]*(quantile(fat3$WRIST,3/4)-quantile(fat3$WRIST,1/4))

#### (2) Doing a single simple regression BODYFAT~ABDOMEN, The R^2 could be around 0.7, which
####is extremely high for a single variable. (And no other single variable in this dataset could
####do this):
fat3_lm_a <- lm(BODYFAT~ABDOMEN, data = fat3)
summary(fat3_lm_a)

### 2. Our model is built for the front users rather for business prediction or acdemical
###research. Wrist is hard to explain, besides user is hard to understand the relationship 
###between wrist circumference and body fat. In one word wrist is less informative and 
###intutively understandable to users.

### 3. The linear model with variables ABDOMEN+WEIGHT gets relatively larger R^2 than the 
###one with ABDOMEN+WRIST:
fat3_lm_wa <- lm(BODYFAT~WEIGHT+ABDOMEN, data = fat3)
summary(fat3_lm_wa)

fat3_lm_wra <- lm(BODYFAT~WRIST+ABDOMEN, data = fat3)
summary(fat3_lm_wra)

### 4. The linear model with variables ABDOMEN+WEIGHT still performed well in CV based on RMSE:

RMSE5 <- c()
for(i in 1:1000){
  row_train <- sample(1:n_all, n_train)
  data_train <- fat3[row_train,]
  fat_lm_train <- lm(BODYFAT~WEIGHT+ABDOMEN, data = data_train)
  data_test <- fat3[-row_train,]
  
  sum_err2 <- 0
  for(j in 1:n_test){
    pre <- predict(fat_lm_train, data_test[j,-1])
    err <- abs(data_test[j,1] - pre)
    sum_err2 <- sum_err2 + err^2
  }
  rmse <- sqrt(sum_err2/(n_test-1))
  RMSE5[i] <- rmse
}
mean(RMSE5)



# (3) model explanation

## Here we got our final model:
fat3_lm_wa <- lm(BODYFAT~WEIGHT+ABDOMEN, data = fat3)
summary(fat3_lm_wa)

### The model indicates that, in general cases, one's percent body fat is determined by the 
##abdomen circumference. The larger your abdomen circumference is, you'll get higher percentage
##of body fat.
### Besides, for people with the same abdomen circumference, the heavier people will get a 
##lower percent of body fat. So if you have the same abdomen circumference as a classical
##body builder, you should take care of yourself right now.

### The coefficients of the model are all significant. This two-variable model gets an R^2 of 
###0.72, which is really high.

### We also find there's acturally a linear relationship between weight and abdomen(but not 
###enough to cause a colliearity problem), which indicates that if you lose your weight by 
###your effort, it's highly likely that your abdomen circumference would get smaller at the
###same time.
plot(fat3$WEIGHT, fat3$ABDOMEN,
     xlab = 'Weight(lbs)', ylab = 'Abdomen circumference(cm)',
     main = 'Linear relationship between Weight & Abdomen circumference')




# (4) model diagnostic
par(mfrow = c(2,2))
plot(fat3_lm_wa)
vif(fat3_lm_wa)
shapiro.test(fat3_lm_wa$residuals)
ncvTest(fat3_lm_wa)
