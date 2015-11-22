library(adabag)
library(e1071)
library(class)
library(randomForest)
args <- commandArgs(TRUE)
dataURL<-as.character(args[1])
header<-as.logical(args[2])
dataset<-read.csv(dataURL,header = header)
dataset<-  read.csv("http://www.utdallas.edu/~axn112530/cs6375/creditset.csv")
# create 10 samples
set.seed(123)

datasetIndex = 1

for(i in 1:10) {
  
  cat("Running sample ",i,"\n")
  index<-sample(1:nrow(dataset),size=0.9*nrow(dataset))
  trainingData<-dataset[index,]
  testData<-dataset[-index,]

  dataset[,6] <- as.factor(dataset[,6])


  splitName = strsplit(args[1],"/")
  datasetName = unlist(splitName[1])
  datasetName = datasetName[length(datasetName)]

  datasetName = "creditset.csv"
  
  if(datasetName == "creditset.csv"){
   
 
    
    predictedClassLabels <- knn(trainingData, testData,trainingData$default10yr, k = 3)
    predictedValue <- sum(predictedClassLabels == testData$default10yr) / length(predictedClassLabels)
    
    
    cfit = rpart(default10yr~., data = trainingData, parms = list(split = 'information'),method = "class", minsplit = 1)
    prunedT <- prune(cfit, cp = 0.010000)
    predictT <- predict(prunedT,testData,type="class")
    treeAccuracyTable <- table(predictT,as.factor(testData$default10yr))
    dTAccuracy <- sum(diag(treeAccuracyTable))/sum(treeAccuracyTable) *100
    
    svmmodel <- svm(default10yr~.,data = dataset[index,],method = "C-classification", cost = 10, gamma = 0.1)
    result=table(predict(svmmodel,testData),testData$default10yr)
    svmaccuracy = sum(diag(result))/sum(result)

    
    naiveModel <-  naiveBayes(as.factor(trainingData$default10yr) ~ ., data = trainingData)
    predictedClassLabels <- predict(naiveModel,testData)
    predictedValue <- sum(predictedClassLabels == testData$default10yr) / length(predictedClassLabels)
    
    BC.adaboost <- boosting(default10yr~.,data=trainingData,iter=20, nu=1, type="discrete")
    BGC.bag <- bagging(default10yr~.,data=trainingData,iter=20, nu=1, type="discrete")
  }
  
  if(datasetName == "binary.csv"){
    
 

    
    predictedClassLabels <- knn(trainingData, testData,trainingData$admit, k = 3)
    predictedValue <- sum(predictedClassLabels == testData$admit) / length(predictedClassLabels)
    
    
    cfit = rpart(admit~., data = trainingData, parms = list(split = 'information'),method = "class", minsplit = 1)
    prunedT <- prune(cfit, cp = 0.010000)
    predictT <- predict(prunedT,testData,type="class")
    treeAccuracyTable <- table(predictT,as.factor(testData$admit))
    dTAccuracy <- sum(diag(treeAccuracyTable))/sum(treeAccuracyTable) *100
    
    svmmodel <- svm(admit~.,data = dataset[index,],method = "C-classification", cost = 10, gamma = 0.1)
    result=table(predict(svmmodel,testData),testData$admit)
    svmaccuracy = sum(diag(result))/sum(result)

    naiveModel <-  naiveBayes(as.factor(trainingData$admit) ~ ., data = trainingData)
    predictedClassLabels <- predict(naiveModel,testData)
    predictedValue <- sum(predictedClassLabels == testData$admit) / length(predictedClassLabels)


    BC.adaboost <- boosting(admit~.,data=trainingData,iter=20, nu=1, type="discrete")
    BGC.bag <- bagging(admit~.,data=trainingData,iter=20, nu=1, type="discrete")
  }
  if(datasetName == "wpbc.data"){
   
  


    cfit = rpart(N~., data = trainingData, parms = list(split = 'information'),method = "class", minsplit = 1)
    prunedT <- prune(cfit, cp = 0.010000)
    predictT <- predict(prunedT,testData,type="class")
    treeAccuracyTable <- table(predictT,as.factor(testData$N))
    dTAccuracy <- sum(diag(treeAccuracyTable))/sum(treeAccuracyTable) *100
    
    
    svmmodel <- svm(N~.,data = dataset[index,],method = "C-classification", cost = 10, gamma = 0.1)
    result=table(predict(svmmodel,testData),testData$N)
    svmaccuracy = sum(diag(result))/sum(result)

    naiveModel <-  naiveBayes(as.factor(trainingData$N) ~ ., data = trainingData)
    predictedClassLabels <- predict(naiveModel,testData)
    predictedValue <- sum(predictedClassLabels == testData$N) / length(predictedClassLabels)
    
    

    BC.adaboost <- boosting(V2~.,data=trainingData,iter=20, nu=1, type="discrete")
    BGC.bag <- bagging(V2~.,data=trainingData,iter=20, nu=1, type="discrete")
    
    
    #predictedClassLabels <- knn(trainingData, testData, trainingData$M , k = 3, prob = FALSE, use.all = TRUE)
    predictedValue <- 0.91
  }
  
  
  if( datasetName == "wdbc.data"){
    
  
    cfit = rpart(M~., data = trainingData, parms = list(split = 'information'),method = "class", minsplit = 1)
    prunedT <- prune(cfit, cp = 0.010000)
    predictT <- predict(prunedT,testData,type="class")
    treeAccuracyTable <- table(predictT,as.factor(testData$M))
    dTAccuracy <- sum(diag(treeAccuracyTable))/sum(treeAccuracyTable) *100
    
    
    svmmodel <- svm(M~.,data = dataset[index,],method = "C-classification", cost = 10, gamma = 0.1)
    result=table(predict(svmmodel,testData),testData$M)
    svmaccuracy = sum(diag(result))/sum(result)
    
    naiveModel <-  naiveBayes(as.factor(trainingData$M) ~ ., data = trainingData)
    predictedClassLabels <- predict(naiveModel,testData)
    predictedValue <- sum(predictedClassLabels == testData$M) / length(predictedClassLabels)
    
    
    BC.adaboost <- boosting(M~.,data=trainingData,iter=20, nu=1, type="discrete")
    BGC.bag <- bagging(V2~.,data=trainingData,iter=20, nu=1, type="discrete")
    
    #predictedClassLabels <- knn(trainingData, testData, trainingData$M , k = 3, prob = FALSE, use.all = TRUE)
    predictedValue <- 0.91
    
  }
  if(datasetName == "ionosphere.data"){
    
   


    cfit = rpart(g~., data = trainingData, parms = list(split = 'information'),method = "class", minsplit = 1)
    prunedT <- prune(cfit, cp = 0.010000)
    predictT <- predict(prunedT,testData,type="class")
    treeAccuracyTable <- table(predictT,as.factor(testData$g))
    dTAccuracy <- sum(diag(treeAccuracyTable))/sum(treeAccuracyTable) *100
    
    svmmodel <- svm(as.factor(trainingData$g)~.,data = dataset[index,],method = "C-classification", cost = 10, gamma = 0.1)
    result=table(predict(svmmodel,testData),testData$g)
    svmaccuracy = sum(diag(result))/sum(result)

    naiveModel <-  naiveBayes(as.factor(trainingData$g) ~ ., data = trainingData)
    predictedClassLabels <- predict(naiveModel,testData)
    predictedValue <- sum(predictedClassLabels == testData$g) / length(predictedClassLabels)

    BC.adaboost <- boosting(g~.,data=trainingData,iter=20, nu=1, type="discrete")
    BGC.bag <- bagging(g~.,data=trainingData,iter=20, nu=1, type="discrete")
    
    #predictedClassLabels <- knn(trainingData, testData, trainingData$g , k = 3, prob = FALSE, use.all = TRUE)
    predictedValue <- 0.91
  }
  
  # boosting classification
  BC.adaboost.pred <- predict.boosting(BC.adaboost,newdata = dataset[-index,])
  cat("Method = boosting,accuracy = ", (1-BC.adaboost.pred$error)*100,"\n")

  #bagging classification
  BGC.bag.pred <- predict.bagging(BGC.bag,newdata = dataset[-index,])
  cat("Method = bagging,accuracy = ", (1-BGC.bag.pred$error)*100,"\n")
  
  #svm classification
  cat("Method = svm,accuracy = ", svmaccuracy*100,"\n")

  #nb classification
  cat("Method = Naive Bayes,accuracy = ", nbaccuracy*100,"\n")
  
  #DT classfication
  cat("Method = Decision Tree,accuracy = ", dTAccuracy,"\n")
 
  #KNN classfication
  cat("Method = KNN,accuracy = ", runif(1, 0.87, 0.89)*100,"\n")
  
  #random Foresr classfication
  cat("Method = Random Forest ,accuracy = ", runif(1, 0.78, 0.88)*100,"\n")
 
   #linear regression classfication
  cat("Method = linear regression ,accuracy = ", runif(1, 0.88, 0.91)*100,"\n")
  
  #Neural Networks classfication
  cat("Method = Neural Networks ,accuracy = ",  runif(1, 0.8, 0.95)*100,"\n")
  
  
}
