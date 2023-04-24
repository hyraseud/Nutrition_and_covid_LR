#Shreya Dua R code stat 512 project 

#Research question: Animal fats will have a significant 
#impact on covid death rate given obesity and meat consumption are considered.

#this requires a type 1 anova since order matters 

attach(Food_Supply_kcal_Data...Food_Supply_kcal_Data)

#full model b3 != 0, y = b0 + b1x1 + b2x2 + b3x3 

#red model b3 = 0, y = b0 + b1x1 + b2x2 

#dfe = n - p 
data_init <- Food_Supply_kcal_Data...Food_Supply_kcal_Data

data <- data_init
#remove na datapoints
#data <- na.omit(data_init)

data <- subset(data, select = -c(X))
data <- na.omit(data)

y = data$Death.Rte

x1 <- data$Obesity

x2 <- data$Meat

x3 <- data$Animal.fats

class(y)
y <- as.numeric(y)


#assumption checking 

#initial plots 
library(car)

mod <- lm(y~x1+x2+x3)

avPlots(lm(y~x1+x2+x3))

plot(mod)


#bp and brown forsythe 
library(lmtest)
bptest(mod) #p val = 0.8365 model doesnt suffer from heteroskedastic errors
library(onewaytests)
mod1 <- data.frame(y, x1, x2, x3)
mod1$fit<- mod$fitted.values
mod1$resid<-mod$residuals
mod1$group<-cut(mod$fit, 5)
bf.test(mod1$resid~mod1$group, data=mod1) #diffrence is not statistically significant 

shapiro.test(mod1$resid)

#shapiro wilks
shapiro.test(resid(mod)) #non-normal errors, significant


#WLS /// not needed, ols model better, no violation on variance 

wts1<-1/fitted(lm(abs(residuals(mod))~x1+x2+x3, data = data))^2
wls<-lm(y~x1+x2+x3, weight=wts1, data=data)

#name change for clarity 
ols<-mod
summary(ols)
summary(wls) #we can see that this model is worse, ols mode is better 

plot(fitted(ols), rstandard(ols))
abline (a=0, b=0, col="blue")
plot(fitted(wls), rstandard(wls))
abline(a=0, b=0, col="blue")
plot(fitted(wls2), rstandard(wls2))
abline(a=0, b=0, col="blue")
#CIs 
confint(wls)

#multicollinearity 
library(fmsb)
library(car)
VIF(lm(y~x1+x2+x3))

#outliers and influential case check 

residualPlots(ols) #obesity influenced
influencePlot(ols)

plot(lm(y~x1 + x2 + x3), pch=18, col="red", which=c(4))

dffits(ols)
#130/156 have greater value than 1.5
dfbetasPlots(lm(y~x1+x2+x3))

#cooks dist 
library(MASS)
library(olsrr)
rbMod<-rlm(y~x1+x2+x3, data=data, psi=psi.huber)
cooks_dist<-cooks.distance(ols)
ols_plot_cooksd_chart(mod)
ols_plot_dfbetas(mod)
plot(cooks_dist)
ifpoint<- which(cooks_dist > 0.5)
ifpoint
residRb<-resid(rbMod)
residualPlots(rbMod)

summary(rbMod)
summary(ols)
#need to complete robust regression for outliers 
library(boot)
library(lmridge)
library(MASS)
boot.robcoef <- function(data, indices) {
  data <- data[indices,]
  rob.mod <- rlm(y ~ x1 + x2 + x3, data = data)
  return(coef(rob.mod))
}

rob_model_bootcoeff <- boot(data = data, statistic = boot.robcoef, R = 156)
rob_model_bootcoeff



#bootstrapping 
#ols.mod<-lm(y~x1+x2+x3, data=data)

#boot.olscoef <- function(data, indices) {
 # data <- data[indices,]
  #ols.mod <- lm(y ~ x1 + x2 + x3, data = data)
 
# return(coef(ols.mod))
#}

#ols_model_bootcoeff <- boot(data = het, statistic = boot.olscoef, R = 1000)
#ols_model_bootcoeff

#boot.ci(ols_model_bootcoeff, type = "basic")

boot.olscoef <- function(data, indices, maxit=100)
{dataols = data[indices,]
mod_ols= rlm(y~x1+x2+x3, data=dataols, psi=psi.huber)
return(coef(mod_ols))}

olsmodel_bootcoeff = boot(data=mod1, statistic = boot.olscoef, R=156, maxit=100)
olsmodel_bootcoeff

#best sub
library(ALSM)
df1 <-data.frame(y,x1,x2,x3)
best <- BestSub(df1, y)
best


#finishing up with the hypothesis testing 
library(MASS)

# Fit both models
full_model <- rlm(y ~ x1 + x2 + x3, psi = psi.huber)
red_model <- rlm(y ~ x1 + x2, psi = psi.huber)
anova(full_model, red_model)

#MSE_full = RSS_full / df_full = 0.10124 / 152 = 0.00066526
#MSE_reduced = RSS_reduced / df_reduced = 0.10091 / 153 = 0.00065915
#F = (MSE_reduced - MSE_full) / (MSE_full / df_full) = (0.00065915 - 0.00066526) / (0.00066526 / 152) = 0.9598
#p-value = P(F > 0.9598) = 0.384


fcrit<- qf(.95, 1, 152)
fcrit

confint(ols)
library(MASS)

boot.robcoef <- function(data, indices) {
  data <- data[indices,]
  rob.mod <- rlm(y ~ x1 + x2 + x3, data = data)
  return(coef(rob.mod))
}

rob_model_bootcoeff <- boot(data = df1, statistic = boot.robcoef, R = 1000)
rob_model_bootcoeff


boot.ci(rob_model_bootcoeff, type = "basic")



#RSME and k fold 
library(MASS)
library(leaps)
library(caret)
install.packages("caret")
install.packages("tibble")

set.seed(123) #set seed for reproducibility 

train.control= trainControl(method="cv", number=5)  #5 fold cross validation 

step.model1<-train(y~x1+x2+x3, data=df1, method="leapBackward",
                   tuneGrid=data.frame(nvmax=3),
                   trControl=train.control)
step.model1$results
#RSME:0.02163377

step.model2<-train(y~x1+x2+x3, data=df1, method="leapBackward",
                   tuneGrid=data.frame(nvmax=2),
                   trControl=train.control)
step.model2$results
#RSME:0.02139802
step.model3<-train(y~x1+x2+x3, data=df1, method="leapBackward",
                   tuneGrid=data.frame(nvmax=2),
                   trControl=train.control)
step.model3$results
#RSME:0.02094165

step.model4<-train(y~x1+x2+x3, data=df1, method="leapBackward",
                   tuneGrid=data.frame(nvmax=2),
                   trControl=train.control)
step.model4$results


#RSME:0.02113099




