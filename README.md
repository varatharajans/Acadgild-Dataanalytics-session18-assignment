# Acadgild-Dataanalytics-session18-assignment
DATA ANALYTICS WITH R, EXCEL AND TABLEAU SESSION 18 ASSIGNMENT 

                                               Session 18 Assignment
                                             Weight Lifting Excercise




5. Problem Statement 
1. Use the below given data set 
DataSet 
2. Perform the below given activities: 
a. Create classification model using different decision trees. 
b. Verify model goodness of fit. 
c. Apply all the model validation techniques. 
d. Make conclusions


 

setwd("C:/Users/Seshan/Desktop/sv R related/acadgild/assignments/session 18 Assign/session18")
library(readr)
Weight_lift <- read.csv("Weight lift.csv")
View(Weight_lift)
data1<-Weight_lift
Weight_lift
# load libraries
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(lattice)
library(rattle)
summary(data1)
library(C50)
#install.package('devtools') # Only needed if you dont have this installed.
library(devtools)
install_github('adam-m-mcelhinney/helpRFunctions')
library(helpRFunctions)
names(data)
dim(data)
library(caret)
library(zoo)
library(plyr)
data<-na.exclude(data1)
is.na(data)
which(is.na(data))
sum(is.na(data))
colSums(is.na(data))
#data[is.na(data)] <- mean(data, na.rm = TRUE)
str(data)
summary(data)
pairs(data[8:15])
# enable multi-core processing
library(doParallel)
#cl <- makeCluster(detectCores())

registerDoParallel()
set.seed(12345)
dataTrain<-data[1:800,]
dataTest<-data[805:4024,]
head(dataTrain)
head(dataTest)
indexNA <- as.vector(sapply(dataTrain[,1:152],function(x) {length(which(is.na(x)))!=0}))
dataTrain <- dataTrain[,!indexNA]
dataTrain<-na.exclude(dataTrain)






library(C50)
head(dataTrain)
head(dataTest)
#------------
library(tree)
fit <-tree(classe~.,data=dataTrain[,-1])
summary(fit)
#fit
plot(fit)
text(fit)
pred <-predict(fit,dataTest[,-1],type='class') 
confusionMatrix(pred,dataTest$classe)

#----
library(rpart)
library(rpart.plot)
fit1 <- rpart(classe~.,data=dataTrain[,-1]) 
fit1
summary(fit1)
# make predictions 
pred <- predict(fit1,dataTest[,-1],type='class')
confusionMatrix(pred,dataTest$classe)
rpart.plot::rpart.plot(fit1)

#------------
# load libraries
library(caret)
library(rpart)

# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model<- train(classe~., data=dataTrain, trControl=train_control, method="rpart")
model
# make predictions
predictions<- predict(model,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix

#---------------
# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model<- train(classe~., data=churnTrain, trControl=train_control, method="C5.0")
model
# make predictions
predictions<- predict(model,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix

#---------------# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model<- train(classe~., data=churnTrain, trControl=train_control, method="bstTree")
model
# make predictions
predictions<- predict(model,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix


#---------------
# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model<- train(classe~., data=dataTrain, trControl=train_control, method="C5.0Cost")
model
# make predictions
predictions<- predict(model,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix

#---------------
# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model<- train(classe~., data=dataTrain, trControl=train_control, method="C5.0Rules")
model
# make predictions
predictions<- predict(model,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix

#---------------
# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model<- train(classe~., data=dataTrain, trControl=train_control, method="C5.0Tree")
model
# make predictions
predictions<- predict(model,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix


#---------------
# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model<- train(classe~., data=dataTrain, trControl=train_control, method="ctree")
model
# make predictions
predictions<- predict(model,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix
#---------------
# define training control
train_control<- trainControl(method="cv", number=10)

# train the model 
model<- train(classe~., data=dataTrain, trControl=train_control, method="ctree2")
model
# make predictions
predictions<- predict(model,dataTest)

# append predictions
pred<- cbind(dataTest,predictions)

# summarize results
confusionMatrix<- confusionMatrix(pred$predictions,pred$classe)
confusionMatrix
