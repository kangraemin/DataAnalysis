#LDA(DiscriminantAnalysis)

library(MASS)
d.a<-read.csv("inflatedtrainset.csv")
d.t<-read.csv("testset.csv")
fit <- lda(a ~ b+c+d+e+f+g+h+i , data=d.a)
pred = predict(fit, d.t)
pred_a = pred$class
table(pred_a,d.t$a)
ct <- table(pred_a,d.t$a)
sum(diag(prop.table(ct)))