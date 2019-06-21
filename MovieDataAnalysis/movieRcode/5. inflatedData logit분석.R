# logit

library(caret)
library(nnet)
d.a<-read.csv("inflatedtrainset.csv")
d.t<-read.csv("testset.csv")
m <- multinom(a~.,data=d.a)
multinom(formula = a ~.,data=d.a)
predicted <- predict(m, newdata=d.t)
cfm <- xtabs(~predicted + d.t$a)
sum ( predicted == d.t $ a ) / NROW ( predicted )
imp <- varImp(m)
rownames(imp)[order(imp$Overall, decreasing="True")][1:5]
confusionMatrix(predicted, d.t$a)
prob= (sum(cfm[2,1])+sum(cfm[1:2,3])+sum(cfm[1:3,4])+sum(cfm[1:4,5])) / sum(cfm[1:5,1:5])
prob