library(caTools)
dis_data <-  read.table("G:/R/Dishonestusers.txt", quote="\"", comment.char="")
dis_data1 <- as.data.frame(cbind(as.numeric(dis_data$V1),as.numeric(dis_data$V2),as.numeric(dis_data$V3),as.numeric(dis_data$V4))) #,dis_data$V5))
dis_data1 <- cbind(dis_data1,dis_data$V5)
str(dis_data1)
View(dis_data1)
dis_data1$`dis_data$V5` = factor(dis_data1$`dis_data$V5`, levels = c("trustworthy","untrustworthy"))
split = sample.split(dis_data1$`dis_data$V5`, SplitRatio = 0.7)
training_set = subset(dis_data1, split == TRUE)
test_set = subset(dis_data1, split == FALSE)

classifier = glm(formula =  `dis_data$V5`~ .,
                 family = binomial,
                 data = training_set)
summary(classifier)


# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-5])
y_pred = as.data.frame( ifelse(prob_pred > 0.5, 1, 0))
summary(prob_pred)
str(y_pred)
# Making the Confusion Matrix
library(caret)
library(e1071)
cm <- table(test_set[, 5], y_pred > 0.5)
View(y_pred)
cm
library(pROC)
plot(roc(test_set[, 5], prob_pred, direction="<"),
     col="yellow", lwd=3, main="The turtle finds its way")
