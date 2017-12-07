library(e1071)
library(mlbench)
data<-read.csv("D:/tips.csv",header=TRUE);
classColumn<- 3;
index<- 1:nrow(data);
testindex<- sample(index, trunc(length(index)/5));
testset<- data[testindex,];
trainset<-data[-testindex,];
svm.model<- svm(tip~.,data=trainset,kernel="radial",epsilon=0);
svm.pred<-predict(svm.model,testset[,-classColumn],na.action=na.omit)
contab <- table(pred=svm.pred, true = testset[,classColumn])
summary(svm.pred)
RMSE <- sqrt(mean((svm.pred - testset[, classColumn])^2))

cor.test(svm.pred,testset[,classColumn],method="spearman")
cor.test(svm.pred,testset[,classColumn],method="pearson")