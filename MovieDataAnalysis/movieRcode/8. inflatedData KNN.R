#K_Nearest_Neighbors

d.a<-read.csv("inflatedtrainset.csv")
d.t<-read.csv("testset.csv")
library(class)
training_data<-d.a[,-1]
test_data<-d.t[,-1]
class <- d.a[,1]
m1 <-knn(training_data,test_data,class,13)
target<-d.t[,1]
table(m1,target)
cfm <- table(m1,target)
sum(diag(prop.table(cfm)))
confusionMatrix(m1,target)
prob= (sum(cfm[2,1])+sum(cfm[1:2,3])+sum(cfm[1:3,4])+sum(cfm[1:4,5])) / sum(cfm[1:5,1:5])
prob