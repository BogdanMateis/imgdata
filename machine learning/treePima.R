library(rpart)
library(datasets)
library(e1071)
pima<-read.table("D:/pima.data",header=FALSE,dec=".",sep=",")
classColumn<- 9
index<-1:nrow(pima)
testindex<-sample(index,trunc(length(index)/3))
testset<-pima[testindex,]
trainset<-pima[-testindex,]
rpart.model<- rpart(V9~.,trainset,method="class")
plot(rpart.model, margin = 0.05); text(rpart.model, use.n = TRUE,
                                       cex = 0.8)
pred<- predict(rpart.model,testset[,-classColumn],type="class")
confusion<-table(pred,testset[,classColumn])
classAgreement(confusion)$diag