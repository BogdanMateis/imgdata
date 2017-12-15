library("mlbench")
library(rpart)
library(datasets)
library("e1071")
library(ROCR) # pentru ROC
library(stats) 
library(fmsb)
data(BreastCancer)
dat<-na.omit(BreastCancer)
dat<-dat[,-1]
classColumn<-10
repeats<-30;
meanaccuracy<-function(dat,repeats){
index<-1:nrow(dat)
accuraciesTree<-vector(mode="numeric",length=repeats)
accuraciesSVM<-vector(mode="numeric",length=repeats)
for(i in 1:repeats){
  #random subsampling 1/3 data
testindex<-sample(index,trunc(length(index)/3))
testset<-dat[testindex,]
trainset<-dat[-testindex,]

#learning
svm.model<-svm(Class~., data = trainset, kernel = "linear", cost = 1, probability = TRUE)
rpart.model<- rpart(Class~.,trainset,method="class")
#predicting
predTree<- predict(rpart.model,testset[,-classColumn],type="class")
predSVM<- predict(svm.model,testset[,-classColumn],type="class")



#matricea de confuzie
confusionTree<-table(predTree,testset[,classColumn])
confusionSVM<-table(predSVM,testset[,classColumn])

accuraciesTree[i]<-classAgreement(confusionTree)$diag
accuraciesSVM[i]<-classAgreement(confusionSVM)$diag
}
#acuratetea medie, deviatia standard
print(mean(accuraciesTree))
print(mean(accuraciesSVM))
print(sqrt(var(accuraciesTree)))
print(sqrt(var(accuraciesSVM)))
Kappa.test(predTree,testset[,classColumn])
Kappa.test(predSVM,testset[,classColumn])
#wilcox
wilcox.test(accuraciesSVM,accuraciesTree,paired=TRUE)
}

#curba ROC
probSVM<-predict(svm.model,testset[,-classColumn],probability=TRUE)
svm.predictii<-attr(probSVM,"probabilities")[,1]
reale<-testset[classColumn][,1]
etichete<-as.integer(reale)-1
pred<-prediction(as.vector(probSVM),etichete)