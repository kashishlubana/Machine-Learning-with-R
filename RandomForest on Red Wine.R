install.packages("randomForest")
library(randomForest)
wineQualityReds <-read.csv("C:/Users/Kashish/Desktop/R/wineQualityReds.csv")

summary(wineQualityReds)
#Change quality to factors
wineQualityReds$quality = as.factor(wineQualityReds$quality)

library(caTools) 
set.seed(20)
sample.split(wineQualityReds$quality, SplitRatio = 0.80)->splitDF1
subset(wineQualityReds, splitDF1==TRUE)->trainingSet1
subset(wineQualityReds,splitDF1==FALSE)->testSet1
View(splitDF1)
rf <- randomForest(quality ~., data=trainingSet, mtry=4, ntree=2001, importance=TRUE)

rf

plot(rf)
View(testSet1)
View(trainingSet1)
result <- data.frame(testSet1$quality, predict(rf, testSet1[,1:11], type = "response"))
