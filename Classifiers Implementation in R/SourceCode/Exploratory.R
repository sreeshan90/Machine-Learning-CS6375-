#for command line arguments
args <- commandArgs(TRUE)
#Read data file

data <-  read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data")
attribute_list <- colnames(data)

i <- 1 #init

for (i in 1:8)
  #Plots for eight columns
  
{
  fName <- paste(getwd(),i,"bmp",sep = ".") #create plot to a file in the working directory
  bmp(fName)
  hist(data[,i]) #histogram plot
  dev.off()
  
  fName <- paste(getwd(),i,"bmp",sep = ".")
  bmp(fName)
  barplot(data[,i]) #barplot
  dev.off()
}

#Correlation between attributes and class labels
j = 1
maxValue = -1
for (j in 1:8)
{
  k = cor(data[,j],data$X1)
  if (k > maxValue)
  {
    maxValue = k
    maxValueAttr = attribute_list[j]
  }
}
cat("Hightest correlation is",maxValue, "between", maxValueAttr , "and x1")


#Find Corelation between each pairs
i = 1
j = 1
maxValue = -1

for (i in 1:8)
{
  
  for (j in (i+1):8)
  {
    if (i != j)
    {
      correlation = cor(data[,i],data[,j]) #find correlation ammong the columns i and j
      if (correlation > maxValue)
      {
        maxValue = correlation
        maxValueAttr1 = attribute_list[i]
        maxValueAttr2 = attribute_list[j]
      }
    }
  }
}
cat(
  "Hightest correlation is",maxValue, "between", maxValueAttr1 , "and", maxValueAttr2
)
