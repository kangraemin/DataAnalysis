
df <- read.csv("train.csv", header = TRUE, sep = ",")
dt <- read.csv("test_set.csv", header = TRUE, sep = ",")

m2 <- lm(Ave.Stroke~ Ave.putting+Ave.birdie+GIR+Par.save+Par.break+Recovery.rate,data=df)
summary(m2)
df.pca <- prcomp(df[,3:8],center=T,scale=T) #PCA수행
summary(df.pca)
screeplot(df.pca,type="lines",pch=1, main = "scree plot") #주성분채택(PC1,PC2,PC3채택)
PRC <- as.matrix(df[,3:8]) %*% df.pca$rotation
train1 <- cbind(df[,2],as.data.frame(PRC))
colnames(train1)[1] <- "Ave.Stroke"
fit1<-lm(Ave.Stroke~PC1+PC2+PC3, data = train1) #PCA회귀분석, 이떄 PC2,3은 유효하지 않는 결과 도출
summary(fit1)
a <- 100 - mean(abs(residuals(fit1))/df[,2])*100 #트레이닝셋의 평균정답률
a
PRC <- as.matrix(dt[,3:8]) %*% df.pca$rotation
train2 <- cbind(dt[,2],as.data.frame(PRC))
colnames(train1)[1] <- "Ave.Stroke"
pred <- predict(fit1, train2)
b <- 100 - mean(abs(dt[,2]-pred)/dt[,2])*100 #테스트셋의 평균정답률
b