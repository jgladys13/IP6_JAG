# Importing the dataset
rm(list = ls())

#Displaying the count of null values per column
dataset=read.csv('heart.data.csv')


# Missing data
colSums(is.na(dataset))
plot(dataset$biking)
dataset$biking[is.na(dataset$biking)]<-mean(dataset$biking,na.rm = TRUE)

colSums(is.na(dataset))
plot(dataset$smoking)
dataset$smoking[is.na(dataset$smoking)]<-mean(dataset$smoking,na.rm = TRUE)

colSums(is.na(dataset))
plot(dataset$heart.disease)
dataset$heart.disease[is.na(dataset$heart.disease)]<-mean(dataset$heart.disease,na.rm = TRUE)
#na. rm = TRUE to exclude missing values

#Create multiple copies of the dataset with no missing data
colSums(is.na(dataset))

##################################################################
## Multiple Linear Regression

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
split=sample.split(dataset$heart.disease,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

# Fitting Multiple Linear Regression to the Training set
Regressor_MLR<-lm(formula = heart.disease~.,data = training_set)

y_pred=predict(Regressor_MLR,newdata = testing_set)
# Predicting the Validation set results
library(caret)
mse(actual, predicted)
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
new <- data.frame(biking=45.09, smoking=21.39)
predict(regressor, newdata = new)


#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))

########################################################
#Support Vector Regressor
# Splitting the dataset into the Training set and Test set

# Fitting SVR to the dataset
library(e1071)

# Predicting the Validation set results
regressor_SVR=svm(formula=heart.disease~.,data=training_set,
                  type='eps-regression',
                  kernel='radial')
y_pred=predict(regressor_SVR,newdata = testing_set)
library(caret)
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
new <- data.frame( )
predict(regressor_SVR,newdata = new)
#RMSE
sqrt(mean((dataset$y_test-y_pred)^2))

########################################################
#Decision Tree Regressor
# Splitting the dataset into the Training set and Test set

# Fitting to the dataset
library(rpart)
regressor_DT=rpart(formula=heart.disease~.,data = training_set)

# Predicting the Validation set results
y_pred=predict(regressor_DT,newdata = testing_set)
library(caret)
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)
#new <- data.frame( )

#RMSE
sqrt(mean((dataset$y_test-y_pred)^2))
predict(regressor_DT,newdata = new)
########################################################
#Random Forest Regressor
# Splitting the dataset into the Training set and Test set

# Fitting to the dataset
library(randomForest)
regressor_RF=randomForest(x=training_set[,1:3],
                          y=training_set$heart.disease,
                          ntree = 20)
# Predicting the Validation set results
y_pred=predict(regressor_RF,newdata = testing_set)
library(caret)
RMSE(testing_set$heart.disease,y_pred)
R2(testing_set$heart.disease,y_pred)

#new <- data.frame( )
new<-data.frame(biking=45.09, smoking=21.39)
predict(regressor_RF,newdata = new)
#RMSE
#sqrt(mean((dataset$y_test-y_pred)^2))

