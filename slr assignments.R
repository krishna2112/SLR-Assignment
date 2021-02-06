1.
x <- read.csv(file.choose())
View(x)

attach(x)

summary(x)

var(x)
sd(x$Weight.gained..grams.,na.rm = F)
sd(x$Calories.Consumed)

install.packages("moments")
library(moments)

skewness(x)
hist(x$Weight.gained..grams.)
hist(x$Calories.Consumed)

kurtosis(x)
hist(x$Weight.gained..grams.)
hist(x$Calories.Consumed)

boxplot(x)


plot(x$Weight.gained..grams.,x$Calories.Consumed)
abline(plot(x$Weight.gained..grams.,x$Calories.Consumed))

qqplot(x$Weight.gained..grams.,x$Calories.Consumed)

boxplot(x$Weight.gained..grams.,x$Calories.Consumed)

cor(x$Weight.gained..grams.,x$Calories.Consumed)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(x,histogram = TRUE,method = "pearson")

library(ggplot2)
ggplot(data = x,aes(x=x$Weight.gained..grams.,y=x$Calories.Consumed))+geom_point(color="blue")


reg1 <- lm(Weight.gained..grams.~Calories.Consumed)
summary(reg1)


reg2 <- lm(Weight.gained..grams.~log(Calories.Consumed))
summary(reg2)

reg3 <- lm(log(Weight.gained..grams.)~Calories.Consumed)
summary(reg3)

confint(reg3,level = 0.95)
pred <- predict(reg3,interval = "predict")
finalvalues <- exp(pred)
View(finalvalues)
vv <- as.data.frame(finalvalues)
write.table(vv,"slr1.csv",row.names = F)
getwd()


2.
x <- read.csv(file.choose())
View(x)

summary(x)


library(moments)

skewness(x)
hist(x$Sorting.Time)
hist(x$Delivery.Time)

kurtosis(x)
hist(x$Delivery.Time)
hist(x$Sorting.Time)

attach(x)

x1 <- plot(x$Delivery.Time,x$Sorting.Time)
x2 <- plot(Delivery.Time,log(Sorting.Time))
x3 <- plot(log(Delivery.Time),Sorting.Time)



qqplot(x$Delivery.Time,x$Sorting.Time)

boxplot(x)

library(PerformanceAnalytics)
chart.Correlation(x,histogram = T,method = "pearson")


library(ggplot2)
ggplot(data = x,aes(x=x$Delivery.Time,y=x$Sorting.Time))+geom_point(color="orange")+geom_line(data = x)
ggplot(data = x,aes(x=x$Delivery.Time,y=x$Sorting.Time))+geom_point(color="orange")+geom_area(data = x)
ggplot(data = x,aes(x=x$Delivery.Time,y=x$Sorting.Time))+geom_point(color="orange")+geom_bar(x=x$Delivery.Time,y=x$Sorting.Time)

library(psych)
pairs.panels(x)

cor(x$Delivery.Time,x$Sorting.Time)
cor(Delivery.Time,log(Sorting.Time)) 
cor(log(Delivery.Time),Sorting.Time) 


 reg1 <- lm(Delivery.Time~Sorting.Time)
 summary(reg1)
 
reg2 <- lm(Delivery.Time~log(Sorting.Time))
summary(reg2)

reg3 <- lm(log(Delivery.Time)~Sorting.Time)
 summary(reg3)
 
 
confint(reg3,level = 0.95)
pred2 <- predict(reg3,interval = "predict")
finalpred2 <- exp(pred2)
View(finalpred2)
write.csv(finalpred2,"Slr_2.csv",row.names = FALSE)
getwd()



3.
x <- read.csv(file.choose())
View(x)
summary(x)
library(moments)

skewness(x)

kurtosis(x)
 
attach(x)

plot(x)
plot(Salary_hike,log(Churn_out_rate))
plot(log(Salary_hike),Churn_out_rate)

boxplot(x)
boxplot(x$Salary_hike,horizontal = T)
boxplot(x$Churn_out_rate,horizontal = T)


library(psych)
pairs.panels(x)

library(ggplot2)
ggplot(data = x, aes(x=Salary_hike,y=Churn_out_rate))+geom_point(color="blue")
ggplot(data = x,aes(x=Salary_hike,y=Churn_out_rate))+geom_point(color="orange")+geom_line(data = x)

library(PerformanceAnalytics)
chart.Boxplot(x)
chart.QQPlot(x)
chart.Correlation(x)

cor(x)
cor(Salary_hike,log(Churn_out_rate))
cor(log(Salary_hike),Churn_out_rate)


reg1 <- lm(Salary_hike~Churn_out_rate)

summary(reg1)
confint(reg1,level = 0.95)
pred3 <- predict(reg1,interval = "predict")
View(pred3)
write.csv(pred3,"pred3.csv")
getwd()

4.
x <- read.csv(file.choose())
View(x)
attach(x)

summary(x)

var(x)
sd(x$YearsExperience)
sd(x$Salary)

library(moments)

skewness(x)
kurtosis(x)


plot(x)

hist(x$YearsExperience)
hist(x$Salary)

boxplot(x)

library(PerformanceAnalytics)
chart.Correlation(x)
chart.QQPlot(x)

qqplot(x$YearsExperience,x$Salary)

library(ggplot2)
ggplot(data = x,aes(x=YearsExperience,y=Salary))+geom_point(color="red")+geom_line(color="blue")

cor(x)

reg4 <- lm(x$Salary~x$YearsExperience)
summary(reg4)
confint(reg4,level=0.95)
pred4 <- predict(reg4,interval = "predict")
write.csv(pred4,"pred4.csv")
getwd()
