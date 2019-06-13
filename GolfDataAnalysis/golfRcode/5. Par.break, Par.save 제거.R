
df <- read.csv("train.csv", header = TRUE, sep = ",")
dt <- read.csv("test_set.csv", header = TRUE, sep = ",")

m2 <- lm(Ave.Stroke~ Ave.putting+Ave.birdie+GIR+Recovery.rate,data=df)
summary(m2) #Par.break, Par.save제거

require(car)
vif(m2)>10 #다중공선성검사

a <- 100 - mean(abs(residuals(m2))/(df[,2]))*100 #트레이닝셋의 평균정답률 
a

dt <- read.csv("test_set.csv", header = TRUE, sep = ",")
pred <- predict(m2, dt)
b <- 100 - mean(abs(dt[,2]-pred)/dt[,2])*100 #테스트셋의 평균정답률
b
