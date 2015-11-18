library(e1071)
data <-
  read.csv(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"
  )

i = 1
sum = 0
count = 0
kernels = c('','linear','sigmoid','radial','polynomial')
defaultFlag = 0
for (ker in kernels) {
  for (i  in 1:10)
  {
    floorVal<-floor(0.9*nrow(data))
    splitData <- sample(nrow(data), size = floorVal)
    trainData <- data[splitData,]
    testData <- data[-splitData,]
    
    if (defaultFlag == 0) {
      modelSVM <-
        svm(as.factor(trainData[,9]) ~ ., data = trainData[,1:8])
      predicted <- predict(modelSVM,testData[,1:8])
      
      
    }
    else{
      modelSVM <-
        svm(as.factor(trainData[,9]) ~ ., data = trainData[,1:8],kernal = ker)
      predicted <- predict(modelSVM,testData[,1:8])
      
    }
    
    table(predicted,testData[,9])
    predictedValue <-
      sum(predicted == testData$X1) / length(predicted)
    sum = sum + predictedValue * 100
    count = count + 1
  }
  
  average = sum / count
  if (defaultFlag == 0) {
    cat("Average accuracy of 10 experiments with Default Kernel is =",average,"%\n")
    defaultFlag = 1
  }
  else{
    cat("Average accuracy of 10 experiments =",average,"% for kernel =",ker,"\n")
  }
  
  
}
