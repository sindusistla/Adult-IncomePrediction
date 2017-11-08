# Random Forest Classifer

library(randomForest)
library(caTools)

# Make categorical Variables as factors
Adult_train_filter$WorkClass <-as.factor(Adult_train_filter$WorkClass)
Adult_train_filter$Marital_Status <- as.factor(Adult_train_filter$Marital_Status)
Adult_train_filter$Occupation <- as.factor(Adult_train_filter$Occupation)
Adult_train_filter$Relationship <- as.factor(Adult_train_filter$Relationship)
Adult_train_filter$Race <- as.factor(Adult_train_filter$Race)
Adult_train_filter$Sex <- as.factor(Adult_train_filter$Sex)
Adult_train_filter$Income <-factor(Adult_train_filter$Income,levels = c("<=50K",">50K"),labels = c(0,1))

# Test Data 
Adult_test_filter$WorkClass <-as.factor(Adult_test_filter$WorkClass)
Adult_test_filter$Marital_Status <- as.factor(Adult_test_filter$Marital_Status)
Adult_test_filter$Occupation <- as.factor(Adult_test_filter$Occupation)
Adult_test_filter$Relationship <- as.factor(Adult_test_filter$Relationship)
Adult_test_filter$Race <- as.factor(Adult_test_filter$Race)
Adult_test_filter$Sex <- as.factor(Adult_test_filter$Sex)
Adult_test_filter$Income <-factor(Adult_test_filter$Income,levels = c("<=50K.",">50K."),labels = c(0,1))

# Divide Test and validation data 
Rf_sample <- sample.split(Adult_test_filter$Income, SplitRatio = .01)
Adult_Validation_Rf <- subset(Adult_test_filter, Rf_sample == TRUE)
Adult_test_filter  <- subset(Adult_test_filter, Rf_sample == FALSE)

dim(Adult_Validation_Rf)
dim(Adult_test_filter)

RandomF_model03 <- randomForest(x=Adult_train_filter[,1:12],y=Adult_train_filter$Income,ntree = 10,
                             importance = TRUE,keep.forest = TRUE)
rf_pred <- predict(RandomF_model03,newdata =Adult_test_filter[,1:12] )
rf_ConfMat <- table(Adult_test_filter$Income,rf_pred)
accuracyRF <- (rf_ConfMat[1,1]+rf_ConfMat[2,2])/(rf_ConfMat[1,1]+rf_ConfMat[2,2]+rf_ConfMat[1,2]+rf_ConfMat[2,1])

precisio_rf <- rf_ConfMat[1,1] / (rf_ConfMat[1,1]+rf_ConfMat[1,2])
precisio1_rf <- rf_ConfMat[2,2] /(rf_ConfMat[2,2]+rf_ConfMat[2,1])

recall0_rf <- rf_ConfMat[1,1] / (rf_ConfMat[1,1]+rf_ConfMat[2,1])
recall1_rf <- rf_ConfMat[2,2] / (rf_ConfMat[2,2]+rf_ConfMat[1,2])

# Validation of the Model 
Rf_Validation <- predict(RandomF_model03,newdata = Adult_Validation_Rf[,1:12])
rftab<-table(Adult_Validation_Rf$Income,Rf_Validation)

accuracy_validation <- (rftab[1,1]+rftab[2,2])/(rftab[1,1]+rftab[2,2]+rftab[1,2]+rftab[2,1])

#Precission of class 0
precisio_rf <- rftab[1,1] / (rftab[1,1]+rftab[1,2])
precisio1_rf <- rftab[2,2] /(rftab[2,2]+rftab[2,1])

recall0_rf <- rftab[1,1] / (rftab[1,1]+rftab[2,1])
recall1_rf <- rftab[2,2] / (rftab[2,2]+rftab[1,2])


Adult_AllData_Rf <- rbind(Adult_train_filter,Adult_test_filter)


# N fold cross validation 
Datafolds=createFolds(Adult_AllData_Rf$Income,k=10)
Rf_cv=lapply(Datafolds,function(x){
  training_fold=Adult_AllData_Rf[-x,]
  test_fold=Adult_AllData_Rf[x,]
  Rf_fold <- randomForest(x=training_fold[,1:12],y=training_fold$Income,ntree = 10,
                          importance = TRUE,keep.forest = TRUE)
  predict_fold_RF <- predict(Rf_fold,newdata =test_fold[,1:12] )
  confusionTable <- table(test_fold$Income,predict_fold_RF)
  foldaccuracy=(confusionTable[1,1]+confusionTable[2,2])/(confusionTable[1,1]+confusionTable[2,2]+confusionTable[1,2]+confusionTable[2,1]) 
  return(foldaccuracy)
})

print(Rf_cv)

meanfold <- mean(as.numeric(Rf_cv))

