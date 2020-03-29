dis_data <- read.table("C:/Users/Kashish/Desktop/R/DishonestInternet.txt", quote="\"", comment.char="")

str(dis_data)
summary(dis_data)
#dis_data1 <- as.data.frame(cbind(as.numeric(dis_data$V1),as.numeric(dis_data$V2),as.numeric(dis_data$V3),as.numeric(dis_data$V4))) #as.numeric(dis_data$V5) 
View(dis_data1)
dis_data1 <- cbind(dis_data1,dis_data$V5)
str(dis_data1)
View(dis_data1)
#dis_data1$`dis_data$V5` = factor(dis_data1$`dis_data$V5`, levels = c("trustworthy","untrustworthy"))
split = sample.split(dis_data$V5, SplitRatio = 0.7)
training_set = subset(dis_data, split == TRUE)
test_set = subset(dis_data, split == FALSE)
dim(training_set)
dim(dis_data1$V5)
library(class)
classifier_knn = knn(train = training_set[, -5],
                     test = test_set[, -5],
                     cl = training_set$V5,
                     k = 5,
                     prob = TRUE)

table <- table(test_set[, 5], classifier_knn)
View(table)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table)
confusionMatrix(test_set[, 5], classifier_knn)
View(classifier_knn)
