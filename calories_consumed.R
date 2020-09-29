#load data calories consumed data file
library(readr)
cal_consumed <- read.csv(file.choose())
View(cal_consumed)
wg <- cal_consumed$Weight.gained..grams.
cc <- cal_consumed$Calories.Consumed

###EXPLORATORY DATA ANALYSIS###
summary(cal_consumed)
attach(cal_consumed)

#HISTOGRAM
hist(wg)  # data is positively skewed
hist(cc)  # data is positively skewed

#BOX-PLOT
boxplot(wg,horizontal = T) # there are no outliers
boxplot(cc,horizontal = T) # there are no outliers 

# density-plot to check normality
library(ggpubr)
ggdensity(wg) # there is no bell shaped curve, its positively skewed curve with long tail
ggdensity(cc) # positively skewed curve 

# scatter-plot
plot(cc,wg)
scatter.smooth(cc,wg) # it has strong linear relationship, as most data points are near to line 

# corelation coeffiecient
cor(cc,wg) # [1] 0.946991, there is a good relationship 

### Simple Linear Regression Model###
reg <- lm(wg~cc)
summary(reg)

pred <- predict(reg)

sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(cal_consumed))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = cal_consumed, aes(x =cc, y = wg)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal_consumed, aes(x=cc, y=pred))

### Results###
#Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8882 
# hence the model is a better model

