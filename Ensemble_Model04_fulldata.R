# Enemsble Model

library(xgboost)
library(caret)
library(class)
library(caTools)
library(stats)

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
  Xgboost_Model04_fold <- xgboost(data=as.matrix(training_fold[,-7]),
                                  label=as.matrix(training_fold$Income),
                                  nrounds=100,objective ="binary:logistic",nthread=5
                                  )
  predict_fold <- predict(Xgboost_Model04_fold,newdata =as.matrix(test_fold[,-7]),ntreelimit=1)
  predictfoldClass <- ifelse(predict_fold > 0.5,1,0)
  print(dim(as.matrix(testIncome)))
  print(dim(as.matrix(predictfoldClass)))
  confusionTable <- table(testIncome,predictfoldClass)
  foldaccuracy=(confusionTable[1,1]+confusionTable[2,2])/(confusionTable[1,1]+confusionTable[2,2]+confusionTable[1,2]+confusionTable[2,1]) 
  return(foldaccuracy)
})



print(emsembel)

meanensemble <- mean(as.numeric(emsembel))


# XGBoost 

Xgboost_Model04 <- xgboost(data=as.matrix(Adult_train_filter_combined[,-7]),
                                label=as.matrix(Adult_train_filter_combined$Income),
                                nrounds=10,objective ="binary:logistic",nthread=5
)
predict_boost <- predict(Xgboost_Model04,newdata =as.matrix(Adult_test_filter_combined[,-7]),ntreelimit=1)

confTabBoost <- table(Adult_test_filter_combined$Income,predict_boost)
foldaccuracy=(confTabBoost[1,1]+confTabBoost[2,2])/(confTabBoost[1,1]+confTabBoost[2,2]+confTabBoost[1,2]+confTabBoost[2,1]) 


precisio0_en <- confTabBoost[1,1] / (confTabBoost[1,1]+confTabBoost[1,2])
precisio1_en <- confTabBoost[2,2] /(confTabBoost[2,2]+confTabBoost[2,1])

recall0_en <- confTabBoost[1,1] / (confTabBoost[1,1]+confTabBoost[2,1])
recall1_en <- confTabBoost[2,2] / (confTabBoost[2,2]+confTabBoost[1,2])
