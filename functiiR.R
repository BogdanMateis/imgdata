library(e1071)

makemodel<- function(file,repeats)
  {
  accuracies<-vector(mode="numeric",length=repeats);
  for(i in 1:repeats){
  data <- read.table(file,header=TRUE);
  noOfSamples <- nrow(data);
  random <- sample (1:noOfSamples,replace=FALSE);
  noOfTrainingSamples <- round(0.66* noOfSamples);
  randomTrain <- random[1:noOfTrainingSamples];
  randomTest <- random[(noOfTrainingSamples +1):noOfSamples];
  train <- data[randomTrain,];
  test <- data[randomTest,];
  claseTest<- train[,c("tip")];
  train <- subset(train, select=- tip);
  svm_model <- svm(train,claseTest);
  clase <- test[,c("tip")];
  test <- subset(test, select=- tip);
  pred <- predict(svm_model,test);
  accuracies[i] <- classAgreement(table(pred,clase))$diag;
  }
  return(mean(accuracies));
}
mergearrays<- function (a, b)
{
  m <- cbind(a,b);
  return (m);  
}

fileName <- "D:/date.txt" #calea catre fisierul cu datele
makemodel(fileName, 2)


makemodelArray<- function(dataset,repeats)
{
  accuracies<-vector(mode="numeric",length=repeats);
  for(i in 1:repeats){
    data <- dataset
    noOfSamples <- nrow(data);
    random <- sample (1:noOfSamples,replace=FALSE);
    noOfTrainingSamples <- round(0.66* noOfSamples);
    randomTrain <- random[1:noOfTrainingSamples];
    randomTest <- random[(noOfTrainingSamples +1):noOfSamples];
    train <- data[randomTrain,];
    test <- data[randomTest,];
    claseTest<- train[,c("tip")];
    train <- subset(train, select=- tip);
    svm_model <- svm(train,claseTest);
    clase <- test[,c("tip")];
    test <- subset(test, select=- tip);
    pred <- predict(svm_model,test);
    accuracies[i] <- classAgreement(table(pred,clase))$diag;
  }
  return(mean(accuracies));
}

file1<- read.table("d:/programming/r/data/thresh211scan1ms.txt",header=TRUE)
file2<- read.table("d:/programming/r/data/thresh215scan1.txt",header=TRUE)
file3<- read.table("d:/programming/r/data/thresh160scan0ms.txt",header=TRUE)
file4<- read.table("d:/programming/r/data/thresh160scan0.txt",header=TRUE)
file5<- read.table("d:/programming/r/data/thresh180scan0.txt",header=TRUE)
file6<- read.table("d:/programming/r/data/thresh180scan0ms",header=TRUE)
file7<-  read.table("d:/programming/r/data/thresh200scan0ms",header=TRUE)
file8<-  read.table("d:/programming/r/data/thresh200scan0.txt",header=TRUE)