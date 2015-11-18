library(e1071)

#for command line arguments
args <- commandArgs(TRUE)

#Read data file

data <-  read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data")

count = 0

sum = 0

for (i  in 1:10)
  #10 iterations
  
{
  floorVal<-floor(0.9*nrow(data))
  splitData <- sample(nrow(data), size = floorVal)
  trainData <- data[splitData,]
  testData <- data[-splitData,]
  
  #prepare naive model on triaingin data
  naiveModel <-
    naiveBayes(as.factor(trainData[,9]) ~ ., data = trainData[,1:8])
  predictedClassLabels <- predict(naiveModel,testData)
  
  predictedValue <-
    sum(predictedClassLabels == testData$X1) / length(predictedClassLabels)
  cat("Iterartion ",i,": Accuracy is",predictedValue * 100,"% \n")
  sum = sum + predictedValue * 100
  count = count + 1
}

average = sum / count
cat("Average accuracy = ",average,"%")