library(class)
#for command line arguments
args <- commandArgs(TRUE)
#Read data file

data <-  read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data")

kValues = c(3,5,7,9,11)

i = 1
count = 0
sum = 0

for (kVal in kValues)
{
  for (i  in 1:10)
  {
    floorVal<-floor(0.9*nrow(data))
    splitData <- sample(nrow(data), size = floorVal)
    trainData <- data[splitData,]
    testData <- data[-splitData,]
    
    predictedClassLabels <-
      knn(trainData[,1:8], testData[,1:8],trainData$X1, k = kVal)
    
    predictedValue <-
      sum(predictedClassLabels == testData$X1) / length(predictedClassLabels)

    sum = sum + predictedValue * 100
    count = count + 1
  }
  average = sum / count
  cat("Average accuracy of 10 experiments =",average,"% when k is", kVal, "\n")
}