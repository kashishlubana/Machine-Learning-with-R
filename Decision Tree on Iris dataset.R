library(datasets)
library(ggplot2)
library(caTools)
library(rpart)

data("iris")
View(iris)

sample.split(iris$Species, SplitRatio = 0.80) -> splitDF
subset(iris, splitDF==TRUE)-> trainingset
subset(iris, splitDF==FALSE)-> testset
rpart(Species~.,  trainingset) -> trainedModel
predict(trainedModel, testset, type = "class") -> finalResult
table(testset$Species, finalResult)

newData<- data.frame(Sepal.Length = 5.0, Sepal.Width = 3.3, Petal.Length= 1.2, Petal.Width = 1.0)

predict(trainedModel, newData, type="class")









