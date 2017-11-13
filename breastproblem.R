library("e1071")
library("mlbench")
data(BreastCancer)
dat<- BreastCancer
repeats <-100
classColumn <-10;
accuracies <- vector(mode="numeric",length=10)
dat<- dat[,-1];
index <- 1:nrow(dat)
for(i in 1:repeats){
  testindex <- sample(index, trunc(length(index)/4)) # 25%

  testset <- dat[testindex, ]
 # clase <- testset[,c("Class")]
  trainset <- dat[-testindex, ]
  svm.model <- svm(Class ~ ., data = trainset, kernel =
                     "sigmoid", cost = 1,na.action=na.omit)
  svm.pred <- predict(svm.model, testset[, -classColumn],na.action=na.omit)
  pred = svm.pred;
   dif <-nrow(testset) - length(pred) #egalizez lista de pred cu cea din testset cu o diferenta de length
  testset<- testset[-(1:dif),];
  contab <- table(pred, true = testset[,classColumn])
  accuracies[i] <- classAgreement(contab)$diag
}
dim(pred)


testset<- testset[-1,]
is.numeric(testset)
print(mean(accuracies))
