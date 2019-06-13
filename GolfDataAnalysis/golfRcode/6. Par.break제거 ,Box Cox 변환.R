
df <- read.csv("train.csv", header = TRUE, sep = ",")
dt <- read.csv("test_set.csv", header = TRUE, sep = ",")

library(MASS)

c = boxcox(Ave.Stroke~ Ave.putting+Ave.birdie+GIR+Par.save+Recovery.rate,data=df)
lamda = c$x[which(c$y==max(c$y))]
m2 <- lm(Ave.Stroke^(lamda)~ Ave.putting+Ave.birdie+GIR+Par.save+Recovery.rate,data=df)
summary(m2) #Par.break제거,Box cox변환
require(car)
vif(m2)>10 #다중공선성검사
a <- 100 - mean(abs(residuals(m2))/(df[,2])^(lamda))*100 #트레이닝셋의 평균정답률 
a
dt <- read.csv("test_set.csv", header = TRUE, sep = ",")
pred <- predict(m2, dt)
b <- 100 - mean(abs(dt[,2]^(lamda)-pred)/dt[,2]^(lamda))*100 #테스트셋의 평균정답률
b