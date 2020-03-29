dis_data <- read.table("C:/Users/Kashish/Desktop/R/Dishonestusers.txt", quote="\"", comment.char="")
summary(dis_data)
str(dis_data)

# Splitting the dataset into the Training set and Test set
library(caTools)
split = sample.split(dis_data$V5, SplitRatio = 0.8)
training_set = subset(dis_data, split == TRUE)
test_set = subset(dis_data, split == FALSE)

#Decison Tree
library(rpart)
classifier = rpart(formula = V5 ~ .,
                   data = training_set)
result = predict(classifier,test_set,type = "class")

#Confusion Matrix
table(test_set$V5,result)
library(e1071)
confusionMatrix(test_set$V5,result)


#Logistic Regression
classifier = glm(formula = V5 ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-5])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 5], y_pred > 0.5)

#kNN
summary(dis_data)
library(class)
dis_data$V1 = factor(dis_data$V1, levels = c("CT_range_1","CT_range_2","CT_range_3","CT_range_4"))
dis_data$V2 = factor(dis_data$V2, levels = c("CU_range_1","CU_range_3","CU_range_4","CU_range_5"))
dis_data$V3 = factor(dis_data$V3, levels = c("LT_range_1","LT_range_2","LT_range_3","LT_range_4"))
dis_data$V4 = factor(dis_data$V4, levels = c("sport","ECommerce","holiday","game"))
dis_data$V5 = factor(dis_data$V5, levels = c("trustworthy","untrustworthy"))

classifier_knn = knn(train = training_set[, -5 ],
                     test = test_set[, -5 ],
                     cl = training_set[, 5 ],
                     k = 5)


table(test_set[, 5], classifier_knn)
