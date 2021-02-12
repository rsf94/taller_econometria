# 2.2 ATE y estimación MCO

library(Matching)
library(stargazer)
data(lalonde)

attach(lalonde)

mean(re78[treat==1]) - mean(re78[treat==0])

# Prueba de Neyman
# se vió clase pasada


reg1 <- lm(re78~treat)
stargazer(reg1,type="text")

c(mean(age[treat==1]),mean(age[treat==0]))

t.test(
  x = age[treat==1],
  y = age[treat==0]
)