

# load the dataset Spam
Spam = read.csv("C:/Users/kashi/Desktop/data_exam_2/Spam.csv", header=TRUE,sep = ',')
head(Spam)

#Split data and check accuracy
library(caTools)

split = sample.split(Spam$yesno, SplitRatio = 0.8)
training_set = subset(Spam, split == TRUE)
test_set = subset(Spam, split == FALSE)


fit.lr <- glm(yesno ~ ., data=training_set, family="binomial")
summary(fit.lr) 


prob_pred = predict(fit.lr, type = 'response', newdata = test_set[-7])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

cm = table(test_set[, 7], y_pred)
cm[1]/sum(cm)


# use LDA

library(klaR)
library(MASS)
fit.gw <- greedy.wilks(yesno ~ . , data=training_set)
fit.gw

fit.lda <- lda(fit.gw$formula, data=training_set, CV=TRUE)


# decision tree

library(rpart)
classifier = rpart(formula = yesno ~ .,
                   data = training_set,method = "class")
result = predict(classifier,test_set,type = "class")
cm <- table(test_set$yesno,result)
cm[1]/sum(cm)

#Plotting the tree for yesno
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(classifier)


# Answer for 2 question
#Predictors differs



# Answer for 3rd question
#REccomend to use logistic Regresstion