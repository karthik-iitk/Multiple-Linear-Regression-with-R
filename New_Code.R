library(tidyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(caTools)
library(varhandle)
library(dummies)
library(GGally)
library(corrplot)
library(VIF)
library(lmtest)
library(sandwich)
library(robustbase)

dataset<-read.csv("ToyotaCorolla.csv")

plot(dataset$Age , dataset$Price , abline(lm(formula = dataset$Price ~ dataset$Age ) , col = 'red') , col = 'blue' )
plot(dataset$KM , dataset$Price , abline(lm(formula = dataset$Price ~ dataset$KM ) , col = 'red') , col = 'blue' )
plot(dataset$HP , dataset$Price , abline(lm(formula = dataset$Price ~ dataset$HP ) , col = 'red') , col = 'blue' )
plot(dataset$Automatic , dataset$Price , abline(lm(formula = dataset$Price ~ dataset$Automatic ) , col = 'red') , col = 'blue' )
plot(dataset$CC , dataset$Price , abline(lm(formula = dataset$Price ~ dataset$CC ) , col = 'red') , col = 'blue' )
plot(dataset$Weight , dataset$Price , abline(lm(formula = dataset$Price ~ dataset$Weight ) , col = 'red') , col = 'blue' )

dummy_FuelType <- model.matrix(~FuelType,data=dataset)
dataset <- cbind(dataset %>% dplyr::select(-FuelType),dummy_FuelType[,-1])
dataset$FuelTypeDiesel <- NULL

m <- cor(dataset,dataset)
corrplot(m , method = 'color')
corrplot(m , method = 'number')

m1=lm(Price~Age,dataset)
summary(m1)
plot(m1)

m2=lm(Price~KM,dataset)
summary(m2)
plot(m2)

m3=lm(Price~HP,dataset)
summary(m3)
plot(m3)

m4=lm(Price~CC,dataset)
summary(m4)
plot(m4)

m5=lm(Price~Weight,dataset)
summary(m5)
plot(m5)


m6=lm(Price~.,dataset)
summary(m6)

m7=lm(Price~.-FuelTypePetrol,dataset)
summary(m7)

                                                                                                                                                                                                                    

m8=lm(Price~.-FuelTypePetrol-MetColor,dataset)
summary(m8)

m9=lm(Price~.-FuelTypePetrol-MetColor-Automatic,dataset)
summary(m9)

m10=lm(Price~.-FuelTypePetrol-MetColor-Automatic-Doors,dataset)
summary(m10)
plot(m10)

m11=lmrob(Price~.-FuelTypePetrol-MetColor-Automatic-Doors,dataset)
summary(m11)
plot(m11)

m12=lm(log(Price)~.-FuelTypePetrol-MetColor-Automatic-Doors+log(KM),dataset)
summary(m12)
plot(m12)

m13=lm(log(Price)~.-FuelTypePetrol-MetColor-Automatic-Doors+log(KM)-CC,dataset)
summary(m13)
plot(m13)

pairs(~log(Price)+Age+log(KM)+HP+Weight,data = dataset,main="Scatterplot Combinations")
