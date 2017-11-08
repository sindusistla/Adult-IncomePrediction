# Enemsble Model

library(xgboost)

# we need to convert all the categorical variables to Numerical by Onehot encoding 
Adult_train_filter$Income <-factor(Adult_train_filter$Income,levels = c("<=50K",">50K"),labels = c(0,1))
categoricalVars <- c("WorkClass","Marital_Status","Occupation","Relationship","Race","Sex")
dummies <- dummyVars(~ WorkClass + Marital_Status  + Occupation + Relationship + Race + Sex, data = Adult_train_filter)
Adult_train_filter_ohe <- as.data.frame(predict(dummies, newdata =Adult_train_filter ))
Adult_train_filter_combined <- cbind(Adult_train_filter[,-c(which(colnames(Adult_train_filter) %in% categoricalVars))],Adult_train_filter_ohe)


# Test Data 
Adult_test_filter$Income <-factor(Adult_test_filter$Income,levels = c("<=50K.",">50K."),labels = c(0,1))
categoricalVars <- c("WorkClass","Marital_Status","Occupation","Relationship","Race","Sex")
dummies <- dummyVars(~ WorkClass + Marital_Status  + Occupation + Relationship + Race + Sex, data = Adult_test_filter)
Adult_test_filter_ohe <- as.data.frame(predict(dummies, newdata =Adult_test_filter ))
Adult_test_filter_combined <- cbind(Adult_test_filter[,-c(which(colnames(Adult_test_filter) %in% categoricalVars))],Adult_test_filter_ohe)


Adult_AllData_enese <- rbind(Adult_train_filter_combined,Adult_test_filter_combined)
# N fold cross validation 
Datafolds=createFolds(Adult_AllData_enese$Income,k=10)
emsembel=lapply(Datafolds,function(x){
  training_fold=Adult_AllData_enese[-x,]
  test_fold=Adult_AllData_enese[x,]
  testIncome<-test_fold$Income
  #print(testIncome)
  Xgboost_Model04_fold <- xgboost(data=as.matrix(training_fold[,-4]),
                                  label=as.matrix(training_fold$Income),
                                  nrounds=20,objective ="binary:logistic"
  )
  predict_fold <- predict(Xgboost_Model04_fold,newdata =as.matrix(test_fold[,-4]),ntreelimit=1)
  predictfoldClass <- ifelse(predict_fold > 0.5,1,0)
  print(dim(as.matrix(testIncome)))
  print(dim(as.matrix(predictfoldClass)))
  confusionTable <- table(testIncome,predictfoldClass)
  foldaccuracy=(confusionTable[1,1]+confusionTable[2,2])/(confusionTable[1,1]+confusionTable[2,2]+confusionTable[1,2]+confusionTable[2,1]) 
  return(foldaccuracy)
})



print(emsembel)

meanensemble <- mean(as.numeric(emsembel))
