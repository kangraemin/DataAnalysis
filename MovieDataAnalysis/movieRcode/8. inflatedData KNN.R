#K_Nearest_Neighbors

d.a<-read.csv("inflatedtrainset.csv")
d.t<-read.csv("testset.csv")
library(class)
training_data<-d.a[,-1]
test_data<-d.t[,-1]
class <- d.a[,1]
m1 <-knn(training_data,test_data,class,13)
target<-d.t[,1]
table(target,m1)
ct <- table(target,m1)
sum(diag(prop.table(ct)))