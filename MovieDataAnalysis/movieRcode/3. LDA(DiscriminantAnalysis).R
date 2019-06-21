#LDA(DiscriminantAnalysis)

library(MASS)
d.a<-read.csv("trainset.csv")
d.t<-read.csv("testset.csv")
fit <- lda(a ~ b+c+d+e+f+g+h+i , data=d.a)
pred = predict(fit, d.t)
pred_a = pred$class
table(pred_a,d.t$a)
cfm <- table(pred_a,d.t$a)
sum(diag(prop.table(cfm)))
confusionMatrix(pred_a, d.t$a)
prob= (sum(cfm[2,1])+sum(cfm[1:2,3])+sum(cfm[1:3,4])+sum(cfm[1:4,5])) / sum(cfm[1:5,1:5])
prob
