data(cars)

# Scatter Plot
scatter.smooth(x=data1$speed, y=cars$dist, main="Dist ~ Speed") 

# BoxPlot: Check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns to show below two plots in 1 screen
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

# Density plot: Check if the response variable is close to normality
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns to show below two plots in 1 screen
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")


# Correlation: from -1 to 1, 1 means high relation b/w variables
cor(cars$speed, cars$dist)  # calculate correlation between speed and distance 

 
# Building Linear Model
linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)


# Linear Regression Diagnostics: 
summary(linearMod)




# Predicting Linear Models: stepwise
# Create the training (development) and test (validation) data samples from original data.
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Develop the model on the training data and use it to predict the distance on test data

lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

# Review diagnostic measures
summary (lmMod) 


# Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

# k- Fold Cross validation
library(DAAG)
cvResults <- suppressWarnings(CVlm(df=cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  # => 251.2783 mean squared error

