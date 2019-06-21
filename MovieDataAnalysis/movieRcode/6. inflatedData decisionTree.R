#decisionTree

library(rpart)
library(caret)
d.a<-read.csv("inflatedtrainset.csv")
d.t<-read.csv("testset.csv")
m.rp <- rpart(a~b+c+d+e+f+g+h+i, data=d.a, method = "class" )
pred.y <- predict(m.rp, d.t[,-1], type = "class")
cfm <- table(d.t$a, pred.y)
train.err <- (sum(cfm)-sum(diag(cfm)))/sum(cfm)
cfm
train.err
(1-train.err)*100
library(party)
#dtree<-ctree(a~. , d.a)
#plot(dtree)
imp <- varImp(m.rp)
rownames(imp)[order(imp$Overall, decreasing="True")][1:5]
confusionMatrix(pred.y, d.t$a)
prob= (sum(cfm[2,1])+sum(cfm[1:2,3])+sum(cfm[1:3,4])+sum(cfm[1:4,5])) / sum(cfm[1:5,1:5])
prob