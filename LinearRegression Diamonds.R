data <- iris 
library(ggplot2)
data(diamonds)
View(diamonds)

#Training and testing set
library(caTools)
sample.split(diamonds$price, SplitRatio = 0.80)->splitDF
subset(diamonds, splitDF==T)->trainingSet
subset(diamonds,splitDF==F)->testSet

#Model train and test
lm(price~.,data = trainingSet)->trainedModel
predict(trainedModel, testSet)->finalResult
cbind(actual = testSet$price, predicted = finalResult)->finalData
as.data.frame(finalData)->finalData

#confusion matrix
(finalData$actual - finalData$predicted)->errorRate
cbind(finalData,errorRate)->finalData
errorMean<-sqrt(mean(finalData$errorRate^2))
errorMean

#new prediction
newdata<-data.frame(carat=0.23,cut='Good',color='E',clarity='VS1', depth=56.9, table=65.0, x=4.06, y=4.07, z=2.31)
predict(trainedModel, newdata)
