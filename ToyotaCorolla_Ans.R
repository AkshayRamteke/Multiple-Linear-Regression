library(e1071)
library(car)
Data <- read.csv("E:\\Assignment\\5_Multi Linear regression\\ToyotaCorolla.csv")
Corolla<-Data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

attach(Corolla)


# Find Correlation between input and output
pairs(Corolla)

# Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Corolla)

##Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(Corolla))

## Building linear regression model
model <- lm(Price ~ ., data = Corolla)
summary(model)

# cc and Doors are influence to each other, predict the model based on individual records
model.carcc <- lm(Price ~ cc)
summary(model.carcc) # Its significat to output


model.cardoor <- lm(Price ~ Doors)
summary(model.cardoor) # It's also significatnt

## Build model with cc and Doors
model.car <- lm(Price ~ cc + Doors)
summary(model.car) # Both are significant to each other

# Find out the influencial record
influence.measures(model.car)

# ploting influential measures
influenceIndexPlot(model.car)

influencePlot(model.car)

# Delete influentails records and build the model
model1 <- lm(Price ~ ., data = Corolla[-c(81),])
summary(model1)

vif(model1)

avPlots(model1)

finalmodel <- lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight, data = Corolla[-c(81),])
summary(finalmodel)

## Evaluate model LINE assumptions 
plot(finalmodel)

hist(residuals(finalmodel)) # close to normal distribution

#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(finalmodel)







