# 2.2 ATE y estimación MCO

library(Matching)
library(stargazer)
library(devtools)
library(RCT)
library(usethis)

data(lalonde)

attach(lalonde)

mean(re78[treat==1]) - mean(re78[treat==0])

# Prueba de Neyman
# se vió clase pasada


reg1 <- lm(re78~treat)
stargazer(reg1,type="text")

c(mean(married[treat==1]),mean(married[treat==0]))

t.test(
  x = married[treat==1],
  y = married[treat==0]
  
)

mlp <- lm(treat~age+educ+black)
stargazer(mlp,type="text")

variables <- data.frame(age,educ,black,hisp,married,nodegr,treat)

tabla <- balance_table(variables,treatment="treat")

stargazer(as.data.frame(tabla),type="text", summary=FALSE)
