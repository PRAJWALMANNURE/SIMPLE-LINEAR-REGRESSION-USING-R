#Load data file
library(readr)
salary_data <- read.csv(file.choose())
View(salary_data)
attach(salary_data)

#EXPLORATORY DATA ANALYSIS
summary(salary_data)

#normality-test
shapiro.test(YearsExperience) #p-value = 0.1034 data is normal.
shapiro.test(Salary) # p-value = 0.01516, data isn't normal 

boxplot(YearsExperience,horizontal = T) # there are no outliers in the data
boxplot(Salary,horizontal = T) # there are no outliers in the data

hist(YearsExperience) # positive skewness 
hist(Salary) 

library(ggpubr)
ggdensity(YearsExperience) # positive skewness
ggdensity(Salary) # positive skewness

#Scatter plot
plot(YearsExperience,Salary) #strong positive realtion
scatter.smooth(YearsExperience,Salary)

cor(YearsExperience,Salary) # 0.9782416 strong postive correlation

#Simple Linear Regression Model
reg <- lm(Salary~YearsExperience)
summary(reg)
pred <- predict(reg)

sqrt(sum(reg$residuals^2)/nrow(salary_data))#RMSE value is 5592.044

confint(reg,level = 0.95)
predict(reg,interval = 'confidence')

library(ggplot2)

ggplot(data = salary_data,aes(x=YearsExperience,y=Salary))+
   geom_point(color= 'blue')+
  geom_line(color='red', data=salary_data, aes(x=YearsExperience,y=pred))


##Results###
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7958.0 -4088.5  -459.9  3372.6 11448.0 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      25792.2     2273.1   11.35 5.51e-12 ***
#  YearsExperience   9450.0      378.8   24.95  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 5788 on 28 degrees of freedom
#Multiple R-squared:  0.957,	Adjusted R-squared:  0.9554 
#F-statistic: 622.5 on 1 and 28 DF,  p-value: < 2.2e-16

#Logarthemic Model
plot(log(YearsExperience),Salary) # strong positive raltionship
scatter.smooth(log(YearsExperience),Salary)

cor(log(YearsExperience),Salary) # 0.9240611 strong positive correlation

reg2 <- lm(Salary~log(YearsExperience))
summary(reg2)

#R squared value has decresed,to Multiple R-squared:  0.8539,

############################################################################
#conclusion, Simple linear regression model is a better fit model with 95.7%
#with less significance value p-value: < 2.2e-16.
#hence model1 is the best prediction model
#########################################################################