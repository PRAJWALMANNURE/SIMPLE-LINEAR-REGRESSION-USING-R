#load data set
library(readr)
emp_data <- read.csv(file.choose())
View(emp_data)
attach(emp_data)

##EXPLORATORY DATA ANALYSIS###
summary(emp_data)

#nomality test
shapiro.test(Salary_hike) #p-value = 0.5018, data is normal as p-value > 0.05
shapiro.test(Churn_out_rate) #p-value = 0.7342, data is normal as p-value > 0.05

boxplot(Salary_hike, horizontal = T) # there are no outliers
boxplot(Churn_out_rate,horizontal = T) # there are no outliers

hist(Salary_hike) # positive skewness with negative kurtosis
hist(Churn_out_rate) # positive skewness with negative kurtosis

#scatter-plot 
plot(Salary_hike,Churn_out_rate) # we can say there is strong negative realtionship
scatter.smooth(Salary_hike,Churn_out_rate)

#correlation 
cor(Salary_hike,Churn_out_rate) # -0.9117216  there is a strong negative correlation

##SIMPLE LINEAR REGRESSION##
reg <- lm(Churn_out_rate~Salary_hike)
summary(reg)

slpred <- predict(reg)

reg$residuals
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_data)) #RMSE value is 3.997528

sqrt(mean(reg$residuals^2))

confint(reg,level = 0.95)
predict(reg,interval = 'confidence')

#ggplot for adding regression line
library(ggplot2)

ggplot(data = emp_data,aes(x=Salary_hike,y=Churn_out_rate))+
         geom_point(color='blue') +
         geom_line(color='red',data = emp_data,aes(x=Salary_hike,y=slpred))


###RESUTS##
#Residuals:
#  Min     1Q Median     3Q    Max 
#-3.804 -3.059 -1.819  2.430  8.072 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 244.36491   27.35194   8.934 1.96e-05 ***
#  Salary_hike  -0.10154    0.01618  -6.277 0.000239 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 4.469 on 8 degrees of freedom
#Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101 
#F-statistic:  39.4 on 1 and 8 DF,  p-value: 0.0002386


#Logarthemic Model
plot(Salary_hike,Churn_out_rate)
plot(log(Salary_hike),Churn_out_rate)
scatter.smooth(log(Salary_hike),Churn_out_rate)

cor(log(Salary_hike),Churn_out_rate) # -0.9212077 strong negative correlation

reg1 <- lm(Churn_out_rate~log(Salary_hike))
summary(reg1)
logpred<- predict(reg1)

sum(reg1$residuals)

sqrt(sum(reg1$residuals^2)/nrow(emp_data)) #RMSE value is 3.786004

confint(reg1,level = 0.95)
predict(reg1,interval = 'confidence')

library(ggplot2)
ggplot(data = emp_data,aes(x=log(Salary_hike),y=Churn_out_rate))+
  geom_point(color='blue')+
  geom_line(color='red',data = emp_data,aes(x=log(Salary_hike),y=logpred))

#####RESULTS####
#Residuals:
#  Min     1Q Median     3Q    Max 
#-3.678 -2.851 -1.794  2.275  7.624 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        1381.5      195.4   7.070 0.000105 ***
#  log(Salary_hike)   -176.1       26.3  -6.697 0.000153 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 4.233 on 8 degrees of freedom
#Multiple R-squared:  0.8486,	Adjusted R-squared:  0.8297 
#F-statistic: 44.85 on 1 and 8 DF,  p-value: 0.0001532



#Exponential Model

plot(Salary_hike,log(Churn_out_rate)) # strong neagative realationship
scatter.smooth(Salary_hike,log(Churn_out_rate))
scatter.smooth(Salary_hike,log(Churn_out_rate))

cor(Salary_hike,log(Churn_out_rate)) #-0.9346361 there is a strong neagative correlation

reg2 <- lm(log(Churn_out_rate)~Salary_hike)
summary(reg2)

reg2$residuals
sqrt(mean(reg2$residuals^2))

logchr <- predict(reg2)
co <- exp(logchr)

error <- Churn_out_rate-co
error 
sqrt(sum(error^2)/nrow(emp_data)) # RMSE value is 3.541549

confint(reg2,level = 0.95)
predict(reg2,interval = 'confidence')

library(ggplot2)
ggplot(data = emp_data,aes(x=Salary_hike,y=log(Churn_out_rate)))+
  geom_point(color='blue')+
  geom_line(color='red',data = emp_data,aes(x=Salary_hike,y=logchr))
##RESULTS##
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.04825 -0.03519 -0.01909  0.02942  0.08970 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.6383000  0.3175983  20.902 2.88e-08 ***
#  Salary_hike -0.0013963  0.0001878  -7.434 7.38e-05 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.0519 on 8 degrees of freedom
#Multiple R-squared:  0.8735,	Adjusted R-squared:  0.8577 
#F-statistic: 55.26 on 1 and 8 DF,  p-value: 7.377e-05


#Polynomial Model with degree two
plot(Salary_hike*Salary_hike,Churn_out_rate)
plot(Salary_hike*Salary_hike,log(Churn_out_rate))
scatter.smooth(Salary_hike*Salary_hike,log(Churn_out_rate))

cor(Salary_hike*Salary_hike,log(Churn_out_rate)) # -0.925803 strong neagative correlation

reg3 <- lm(log(Churn_out_rate)~Salary_hike+I(Salary_hike*Salary_hike))
summary(reg3)

predy <- predict(reg3)
expy <- exp(predy)

err <- Churn_out_rate-expy
sqrt(sum(err^2)/nrow(emp_data)) # RMSE value is  1.32679

confint(reg3,level = 0.95)
predict(reg3,interval = 'confidence')

library(ggplot2)
ggplot(data = emp_data,aes(x=Salary_hike+I(Salary_hike^2),y=log(Churn_out_rate)))+
    geom_point(color='blue')+
    geom_line(color='red',data = emp_data,aes(x=Salary_hike+I(Salary_hike^2)),y=predy)
####RESULTS###
#Residuals:
#  Min        1Q    Median        3Q       Max 
#-0.027877 -0.014280  0.002735  0.012608  0.027882 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                   2.318e+01  2.415e+00   9.597  2.8e-05 ***
#  Salary_hike                  -2.068e-02  2.813e-03  -7.351 0.000156 ***
#  I(Salary_hike * Salary_hike)  5.605e-06  8.175e-07   6.857 0.000241 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.01997 on 7 degrees of freedom
#Multiple R-squared:  0.9836,	Adjusted R-squared:  0.9789 
#F-statistic: 210.1 on 2 and 7 DF,  p-value: 5.634e-07


#############################################################################
#HENCE THE POLYNOMIAL MODEL IS HAVING R SQUARED VALUE 98.36% HIGHER COMPARE TO OTHER MODELS,
#WITH LESS SIGNIFICANCE VALUE = 0.0000000563
#POLYNOMIAL MODEL IS BEST FIT MODEL
#############################################################################

