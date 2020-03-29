students <- read.csv("C:/Users/kashi/Desktop/data_exam_2/students.csv")



## VIsualizations for the data


boxplot(students$SCORE)
boxplot(students$HOURS)
boxplot(students$ANXIETY)
boxplot(students$A_POINTS)

require(ggplot2)
p <- ggplot(students, aes(SCORE, HOURS))
p + geom_boxplot()





# Correlation: from -1 to 1, 1 means high relation b/w variables
cor(students$SCORE, students$HOURS)  # calculate correlation between score and hours

# This comes out to be .82 

cor(students$SCORE, students$ANXIETY)  # calculate correlation between score and anxiety
# This is .56

cor(students$SCORE, students$A_POINTS)   # calculate correlation between score and a_points

#This is 0.87

## We can clearly see that hours and A_points has the highest effect on predicting score


# Building multiple Linear Model
linearMod <- lm(SCORE ~ A_POINTS + HOURS, data=students)  # build linear regression model on full data
print(linearMod)

# Linear Regression Diagnostics: 
summary(linearMod)


# Predicting Linear Models: stepwise
# Create the training (development) and test (validation) data samples from original data.
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(students), 0.8*nrow(students))  # row indices for training data
trainingData <- students[trainingRowIndex, ]  # model training data
testData  <- students[-trainingRowIndex, ]   # test data



# Develop the model on the training data and use it to predict the Murder on test data

lmMod <- lm(SCORE ~ HOURS + A_POINTS, data=trainingData)  # build the model
ScorePred <- predict(lmMod, testData)  # predict Murder


# Review diagnostic measures
summary (lmMod) 


# Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$SCORE, predicted=ScorePred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)


min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy


# Here we can see that our model is fairly able to predict the murder with an accuracy of 92%




#### Predicting the new scores



newRow <- data.frame(SCORE ="",HOURS=35,ANXIETY=77,A_POINTS=22)

newRow2 <- data.frame(SCORE ="",HOURS=55,ANXIETY=15,A_POINTS=26)
students <- rbind(students,newRow)
students <- rbind(students,newRow2)


studentsnew <- rbind(newRow,newRow2)

z <- predict(lmMod, newdata=studentsnew)
z

##SCORES Predicted are 55.76 for Student1 and 73.25 for student2
