makemodel<- function(file)
  {
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
  return(table(pred,clase));
  
}
