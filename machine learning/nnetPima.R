library(nnet)
library(datasets)
pima<-read.table("D:/pima.data",header=FALSE,dec=".",sep=",")
classColumn<- 9
index<-1:nrow(pima)
testindex<-sample(index,trunc(length(index)/3))
testset<-pima[testindex,]
trainset<-pima[-testindex,]
pima.nn<-nnet(V9~.,trainset,size=4,rang=0.5,maxit=200)
out_train<-class.ind(trainset[,classColumn])
out_test<-class.ind(testset[,classColumn])
test.cl <- function(true, pred){
  true <- max.col(true)
  pred <- max.col(pred)
  table(true, pred)
  return (table(true,pred))
}
tab2<-test.cl(out_train,predict(pima.nn,trainset[,-9]))
tab1<-test.cl(out_test,predict(pima.nn,testset[,-9]))
classAgreement(tab2)$diag

