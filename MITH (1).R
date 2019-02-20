################################# MITH#############################################
  rm(list=ls())
##### Setting the Working Directory
  setwd("~/Insofe/Decision Modeling/MITH/MITH_Data")
  dir()

#### Reading the data files
  traindata <- read.csv("train.csv",header = T) 
  testdata <- read.csv("test.csv",header = T)
  buildingownership<- read.csv("buildingownership.csv",header = T)
  buildingstructure<- read.csv("buildingstructure.csv",header = T)
####### Visualization of the data
  str(traindata)
  summary(traindata)
  str(testdata)
  summary(testdata)
  str(buildingownership)
  str(buildingstructure)
  summary(buildingstructure)
  
####### Type Conversions

#### Train Data
  library(dplyr)
  traindata_cat<-select(traindata,starts_with("h")) 
  names(traindata_cat)
  traindata_cat <- data.frame(apply(traindata_cat,2,as.factor))
  traindata_catg<-subset(traindata,select = c(1,2,3,13))
  names(traindata_catg)
  traindata1<-data.frame(c(traindata_catg,traindata_cat))
  str(traindata1)
  names(traindata1)
  names(testdata1)
#### Test Data
  library(dplyr)
  testdata_cat<-select(testdata,starts_with("h")) 
  names(testdata_cat)
  testdata_cat <- data.frame(apply(testdata_cat,2,as.factor))
  testdata_catg<-subset(testdata,select = c(1:4))
  names(testdata_catg)
  testdata1<-data.frame(c(testdata_catg,testdata_cat))
  str(testdata1)
  rm(testdata_cat,testdata_catg)

#### Buildiing Ownership Data
  buildingownership_cat <- select(buildingownership,starts_with("h"))
  names(buildingownership_cat)
  buildingownership_catg <- select(buildingownership,1:5)
  names(buildingownership_catg)
  buildingownership_cat <- data.frame(apply(buildingownership_cat,2,as.factor))
  buildingownership1<-data.frame(c(buildingownership_catg,buildingownership_cat))
  names(buildingownership)
  names(buildingownership1)
  str(buildingownership1)
  rm(buildingownership_cat,buildingownership_catg)
####  Building structure Data
  
  buildingstructure_cat <- select(buildingstructure,starts_with("h"),c(26,27))
  names(buildingstructure_cat)
  buildingstructure_catg <- select(buildingstructure,1:14,28,29)
  names(buildingstructure_catg)
  buildingstructure_cat <- data.frame(apply(buildingstructure_cat,2,as.factor))
  buildingstructure1<-data.frame(c(buildingstructure_catg,buildingstructure_cat))
  names(buildingstructure)
  names(buildingstructure1)
  str(buildingstructure1)
  rm(buildingstructure_cat,buildingstructure_catg)
  
#### Checking For Null Values  
  sum(is.na(traindata1))
  sum(is.na(testdata1))
  table(traindata1$has_repair_started)
  table(testdata1$has_repair_started)
  sum(is.na(buildingownership1))
  sum(is.na(buildingstructure1))
  table(traindata1$has_repair_started)
  table(testdata1$has_repair_started)
  str(traindata1)

  
### Imputing Missing Values
  library(DMwR)
  traindata1 <- centralImputation(traindata1)  
  table(traindata1$has_repair_started)
  names(testdata1)
## Removing Target As it has all missing Values
  testdata2 <- subset(testdata1,select=-2)
  names(testdata2)
  testdata3 <- subset(testdata1,select=2)
  testdata2 <- centralImputation(testdata2)  
  sum(is.na(testdata2))  
  table(testdata2$has_repair_started)
  testdata1 <-data.frame(c(testdata2,testdata3)) 
  names(testdata1)  
  
### Data Processing
  names(testdata1)
  traindata3 <- subset(traindata1,select=-2)
  traindata2 <- subset(traindata1,select=2)  
  traindata1 <- data.frame(c(traindata3,traindata2))
  names(traindata1)  
  library(caret)
  nearZeroVar(buildingownership1)
  nearZeroVar(buildingstructure1)  
  
## The zero variance attributes in Building Ownership
###  5  8  9 10 11 12 13 14 15 16
## The zero variance attributes in Building Structure
###  14 19 20 25 26 27 28

  
## Removing Zero Variance attributes in both Building OwnerShip And Building Structure
  buildingownership2 <- subset(buildingownership1,select=-c(5,8,9,10,11,12,13,14,15,16)) 
  buildingstructure2 <- subset(buildingstructure1,select= -c(14,19,20,25,26,27,28))  
  names(buildingownership2)  
  names(buildingstructure2)  
  
## Merging Of the Datasets
  mergeddata <- merge(buildingownership2,buildingstructure2,by.x = c("building_id"),
                      by.y = c("building_id"),all.x = T)
  train_merge <- merge(traindata1,mergeddata,by.x = c("building_id"),
                       by.y = c("building_id"),all.x = T)  
  test_merge <- merge(testdata1,mergeddata,by.x = c("building_id"),
                         by.y = c("building_id"),all.x = T)
  names(test_merge)
  names(train_merge)
  str(train_merge)
  str(test_merge)    
  summary(train_merge)  
  summary(test_merge)
  ## Removing target from the dataset   
  test_merge1<- subset(test_merge,select=-c(13))
  test_merge2<- subset(test_merge,select=c(13))
  train_merge1<- subset(train_merge,select=-c(13))
  train_merge2<- subset(train_merge,select=c(13))
  
## Imputation Of missing Values in the new datasets
  train <- centralImputation(train_merge1)
  test <- centralImputation(test_merge1)  
  test1 <- data.frame(c(test,test_merge2))
  names(test1)
  train1<- data.frame(c(train,train_merge2))
  names(train1)  
  sum(is.na(train1))
  sum(is.na(test1))  
  
## Converting the levels of the target to numbers
  train1$damage_grade <- ifelse(train1$damage_grade == "High",yes = 3,ifelse(train1$damage_grade=="Medium",yes = 2,no=1))
## Checking for Class Imbalance in the data 
  prop.table(table(train1$damage_grade))
  train1$damage_grade <-  as.factor(train1$damage_grade)
  str(train1)
## Removing Varaibles Which are not needed for prediction
  names(train1)
  train2 <- subset(train1,select = -c(13,14,15,18,19,20))
  names(test1)
  test2 <- subset(test1,select = -c(13,14,15,18,19,20))
  names(train2)  
  names(test2)
## Writing the dataframes to the disk
  write.csv(train2,"trainfinal.csv",row.names = FALSE)
  write.csv(test2,"testfinal.csv",row.names = FALSE)
## Removing the columns not necessary for prediction
  names(train2)
  train3 <- subset(train2,select = -c(1,2,3,24,32))
  test3<- subset(test2,select = -c(1,2,3,24,32))
  str(train3)
### Building Random Forest  
  library(randomForest)
  model_rf <- randomForest(damage_grade ~ .,data=train3,ntree = 100,mtry = 5) 
  
  importance(model_rf)
  varImpPlot(model_rf)
  plot(model_rf)
  
  # Predict on the train data
  preds_train_rf <- predict(model_rf)
  confusionMatrix(preds_train_rf, train3$damage_grade)
  
  ## Predicting on the testdata
  preds_test_rf <- predict(model_rf,newdata = test3)
  preds_test_rf<- ifelse(preds_test_rf== 3,yes= "High",ifelse(preds_test_rf== 2,yes = "Medium",no="Low" ))
  sub_data_1 <- data.frame(test2[1],preds_test_rf)  
  str(sub_data_1)
  ## Writing the submission file into csv
  write.csv(sub_data_1,"submission.csv",row.names = FALSE)
  ## Writing the train and test to the dataframe
  write.csv(train3,"traindata.csv",row.names = F)
  write.csv(test3,"testdata.csv",row.names = F)
  
### Building Random Forest  
  library(randomForest)
  model_rf1<- randomForest(damage_grade ~ .,data=train3,ntree = 200,mtry =5) 
  
  importance(model_rf1)
  varImpPlot(model_rf1)
  plot(model_rf1)
  
  # Predict on the train data
  preds_train_rf1 <- predict(model_rf1)
  confusionMatrix(preds_train_rf1, train3$damage_grade)
  
  ## Predicting on the testdata
  preds_test_rf1 <- predict(model_rf1,newdata = test3)
  preds_test_rf2<- ifelse(preds_test_rf2== 3,yes= "High",ifelse(preds_test_rf2== 2,yes = "Medium",no="Low" ))
  sub_data_1 <- data.frame(test2[1],preds_test_rf)  
  str(sub_data_1)
  ## Writing the submission file into csv
  write.csv(sub_data_1,"submission2.csv",row.names = FALSE)
  
  
  ### Building Random Forest  
  library(randomForest)
  model_rf3<- randomForest(damage_grade ~ .,data=train3,ntree = 200,mtry =10) 
  
  importance(model_rf3)
  varImpPlot(model_rf3)
  plot(model_rf3)
  
  # Predict on the train data
  preds_train_rf3 <- predict(model_rf3)
  confusionMatrix(preds_train_rf3, train3$damage_grade)
  
  ## Predicting on the testdata
  preds_test_rf3 <- predict(model_rf3,newdata = test3)
  preds_test_rf3<- ifelse(preds_test_rf3== 3,yes= "High",ifelse(preds_test_rf3== 2,yes = "Medium",no="Low" ))
  sub_data_3 <- data.frame(test2[1],preds_test_rf3)  
  str(sub_data_3)
  ## Writing the submission file into csv
  write.csv(sub_data_3,"submission3.csv",row.names = FALSE)

  