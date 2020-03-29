mydata <- read.csv("./datasets/USArrests.csv")

str(mydata)
summary(mydata)
#Remove mising values
mydata <- na.omit(mydata)


head(mydata, n=10)

newdata<- as.data.frame(mydata)
plot(y = newdata$Murder, x = newdata$Assault, main = "Murder Rate vs. Assault Rate, US")

state.names = row.names(newdata)
barplot(newdata$Murder, names.arg = state.names, las = 2, ylab = "Murder Rate ", 
        main = "Murder Rate in the United States ")


# Correlation: from -1 to 1, 1 means high relation b/w variables
cor(newdata$Murder, newdata$Assault)  # calculate correlation between murder and assault

# This comes out to be .80, 

cor(newdata$Murder, newdata$Rape)  # calculate correlation between murder and rape
# This is .56

cor(newdata$Murder, newdata$UrbanPop)  # calculate correlation between murder and UrbanPop

#This is 0.069

## We can clearly see that assault has the highest effect on predicting murder



# Building Linear Model
linearMod <- lm(Murder ~ Assault, data=newdata)  # build linear regression model on full data
print(linearMod)


# Linear Regression Diagnostics: 
summary(linearMod)


# Predicting Linear Models: stepwise
# Create the training (development) and test (validation) data samples from original data.
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(newdata), 0.8*nrow(newdata))  # row indices for training data
trainingData <- newdata[trainingRowIndex, ]  # model training data
testData  <- newdata[-trainingRowIndex, ]   # test data



# Develop the model on the training data and use it to predict the Murder on test data

lmMod <- lm(Murder ~ Assault, data=trainingData)  # build the model
MurderPred <- predict(lmMod, testData)  # predict Murder


# Review diagnostic measures
summary (lmMod) 


# Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$Murder, predicted=MurderPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)


min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy


# Here we can see that our model is fairly able to predict the murder with an accuracy of 75%




#### A way to standardize all the variables is by using scale function####



newdata1<-scale(newdata)

head(newdata1)


