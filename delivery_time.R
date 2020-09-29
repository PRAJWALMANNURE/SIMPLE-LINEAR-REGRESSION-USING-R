#load the data set
library(readr)
delivery_time <- read.csv(file.choose())
View(delivery_time)
dt <- delivery_time$Delivery.Time
st <- delivery_time$Sorting.Time

####EXPLORATORY DATA ANALYSIS####
summary(delivery_time)

#normality-test
shapiro.test(dt) # p-value = 0.8963, data is normal
shapiro.test(st) # p-value = 0.1881, data is normal

#box-plot
boxplot(dt,horizontal = T) # there are no outliers
boxplot(st,horizontal = T) # there are no outliers

#density-plot
library(ggpubr)
ggdensity(dt) # partially follows bell-shape, partially follows positive skewness.
ggdensity(st) # follows negative kurtosis.

#histogram
hist(dt) # follows positive skewness
hist(st) # follows neagative kurtosis

#scatter-plot
plot(st,dt) # there is a moderate relationship
scatter.smooth(st,dt)

#correlation
cor(st,dt) # 0.8259973, we can say there is moderate relationship between variable.

###SIMPLE LINEAR REGRESSION MODEL###
reg <- lm(dt~st)
summary(reg)
pred <- predict(reg)

library(ggplot2)
ggplot(data = delivery_time,aes(x=st,y=dt))+
  geom_point(color='blue')+
  geom_line(color='red',data = delivery_time,aes(x=st,y=pred))

##results##
#Residuals:
#Min      1Q  Median      3Q     Max 
#-5.1729 -2.0298 -0.0298  0.8741  6.6722 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   6.5827     1.7217   3.823  0.00115 ** 
#  st            1.6490     0.2582   6.387 3.98e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 2.935 on 19 degrees of freedom
#Multiple R-squared:  0.6823,	Adjusted R-squared:  0.6655 
#F-statistic:  40.8 on 1 and 19 DF,  p-value: 3.983e-06


#LOGARTHEMIC MODEL###
plot(log(st),dt)
scatter.smooth(log(st),dt)
cor(log(st),dt) # 0.8259973 relationship is moderate

reg1 <- lm(dt~log(st))
summary(reg1)
log_pred <- predict(reg1)

reg1$residuals
sqrt(sum(reg1$residuals^2)/nrow(delivery_time))  #RMSE value is 2.733171

confint(reg1,level=0.95)
predict(reg1,interval="confidence")

library(ggplot2)
ggplot(data = delivery_time,aes(x=log(st),y=dt))+
  geom_point(color='blue')+
  geom_line(color='red',data = delivery_time,aes(x=log(st),y=log_pred))
##RESULTS##
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-4.0829 -2.0133 -0.1965  0.9351  7.0171 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    1.160      2.455   0.472    0.642    
#log(st)        9.043      1.373   6.587 2.64e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 2.873 on 19 degrees of freedom
#Multiple R-squared:  0.6954,	Adjusted R-squared:  0.6794 
#F-statistic: 43.39 on 1 and 19 DF,  p-value: 2.642e-06





###EXPONENTIAL MODEL###
plot(st,log(dt))
scatter.smooth(st,log(dt)) # it shows moderate realtionship

cor(st,log(dt)) #0.8431773. as value is not greater than 0.85,it have moderate realtionship

log_reg <- lm(log(dt)~st)

summary(log_reg)
exp_pred <- predict(log_reg)

log_reg$residuals
sqrt(sum(log_reg$residuals^2)/nrow(delivery_time))  #RMSE is value 0.1669628

confint(log_reg,level=0.95)
predict(log_reg,interval="confidence")

ggplot(data = delivery_time,aes(x=st,y=log(dt)))+
  geom_point(color='blue')+
  geom_line(color='red',data = delivery_time,aes(x=st,y=exp_pred))
##result##
#Residuals:
#Min       1Q   Median       3Q      Max 
#-0.29209 -0.13364  0.02065  0.08421  0.41892 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.12137    0.10297  20.601 1.86e-14 ***
#  st           0.10555    0.01544   6.836 1.59e-06 ***
# ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.1755 on 19 degrees of freedom
#Multiple R-squared:  0.7109,	Adjusted R-squared:  0.6957 
#F-statistic: 46.73 on 1 and 19 DF,  p-value: 1.593e-06



###POLINOMIAN MODEL WITH TWO DEGREE(QUADRATIC)

plot(st*st,dt) # moderate relationship
scatter.smooth(st*st,dt)

cor(st*st,dt) # 0.7939063, we can see a moderate relationship

reg2degree <- lm(log(dt) ~ st + I(st*st))

summary(reg2degree)
polypred <- predict(reg2degree)
reg2degree$residuals
sqrt(sum(reg2degree$residuals^2)/nrow(delivery_time))  #RMSE is value 0.1505874

confint(log_reg,level=0.95)
predict(log_reg,interval="confidence")

ggplot(data = delivery_time,aes(x=st+I(st^2),y=log(dt)))+
  geom_point(color='blue')+
  geom_line(color='red',data = delivery_time,aes(x=st+I(st^2),y=polypred))

##RESULTS##
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.21194 -0.11776 -0.03034  0.10550  0.35975 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.69970    0.22843   7.441 6.77e-07 ***
#  st           0.26592    0.08022   3.315  0.00385 ** 
#  I(st * st)  -0.01284    0.00632  -2.032  0.05722 .  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.1627 on 18 degrees of freedom
#Multiple R-squared:  0.7649,	Adjusted R-squared:  0.7387 
#F-statistic: 29.28 on 2 and 18 DF,  p-value: 2.197e-06


# POLYNOMIAL MODEL WITH DEGREEE 3

reg3degree<-lm(log(dt)~st + I(st*st) + I(st*st*st))

summary(reg3degree)

logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)


reg3degree$residuals
sqrt(sum(reg3degree$residuals^2)/nrow(delivery_time))  #RMSE is value 0.1450416

confint(reg3degree,level=0.95)
predict(reg3degree,interval="confidence")

# visualization
ggplot(data = delivery_time, aes(x = st + I(st^2) + I(st^3), y = dt)) + 
  geom_point(color='blue') +
  geom_line(color='red',data =delivery_time, aes(x=st+I(st^2)+I(st^3), y=expy3))

##results##
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.23291 -0.08697 -0.00472  0.09220  0.31701 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
#(Intercept)      1.071553   0.590824   1.814   0.0874 .
#st               0.649456   0.342562   1.896   0.0751 .
#I(st * st)      -0.080891   0.059452  -1.361   0.1914  
#I(st * st * st)  0.003636   0.003159   1.151   0.2656  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.1612 on 17 degrees of freedom
#Multiple R-squared:  0.7819,	Adjusted R-squared:  0.7434 
#F-statistic: 20.31 on 3 and 17 DF,  p-value: 7.372e-06



## we can conclude that the polynomial model with degree 3 is a better fit model as its rmse value is low as compared to other models