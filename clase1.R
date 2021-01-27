setwd("C:/Users/rsf94/Google Drive/MAESTRÍA ITAM/2do semestre/eco_computacional/clase1")
library(readxl)
library(car)
library(tseries)
library(stargazer)

ruta <- "C:/Users/rsf94/Google Drive/MAESTRÍA ITAM/2do semestre/eco_computacional/clase1/consumo.xls"
datos <- read_xls(ruta, sheet="Hoja1")

attach(datos) # para no poner signo de $ para seleccionar columnas
View(datos)

vars <- data.frame(consumption, income, wealth, interest)
summary(vars)
boxplot(vars, col="orange", main="Diagrama de caja")

plot(income, consumption, type="p", col="red",
     main="Diagrama de dispersión ingreso vs consumo")
abline(lm(consumption~income, data=datos),col="blue")

plot(wealth, consumption, type="p", col="red",
     main="Diagrama de dispersión")
abline(lm(consumption~wealth, data=datos),col="blue")

plot(interest, consumption, type="p", col="red",
     main="Diagrama de dispersión")
abline(lm(consumption~interest, data=datos),col="blue")

n<- length(consumption)
k <- 3
reg1 <- lm(consumption ~ income + wealth + interest,data=datos)

summary(reg1)

ygorro <- reg1$fitted.values
ugorro <- reg1$residuals
sigma2 <- sum(ugorro^2)/(n-k-1)

linearHypothesis(reg1,"income=0")
linearHypothesis(reg1,"wealth=0")
linearHypothesis(reg1,"interest=0")

linearHypothesis(reg1,c("income=0","wealth=0","interest=0"))

confint(reg1)

(fest <- summary(reg1)$fstatistic)

aic <- AIC(reg1)

bic <- BIC(reg1)

r2 <- summary(reg1)$r.squared
r2

r2aj <- summary(reg1)$adj.r.squared
r2aj

y <- consumption
(data.frame(y,ygorro,ugorro))

obs <- 1:n
plot(obs,y,type="l",main="Gráfico de bondad de ajuste")
lines(obs,ygorro,type="l",col="blue")

anova <- aov(reg1)
summary(anova)

jarque.bera.test(ugorro)

vardep <- data.frame(income, wealth,interest)
library(corrplot)
install.packages("corrplot")

M <- round (cor(vardep),4)
corrplot(cor(vardep),method="number")

aux1m <- lm(income~wealth+interest,data=datos)
aux2m <- lm(wealth~income+interest,data=datos)
aux3m <- lm(interest~income+wealth,data=datos)

stargazer(aux1m,aux2m,aux3m, type="text")


plot(obs,ugorro,type="l",col="red")
abline(h=0,col="blue")

ugorro2 <- ugorro^2
plot(ygorro,ugorro2,type="p",col="red",main="Método gráfico")
abline(h=0, col="blue")


