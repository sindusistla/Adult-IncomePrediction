library(caret)
library(class)
library(caTools)
library(stats)


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
logReg_sample <- sample.split(Adult_test_filter$Income, SplitRatio = .01)
Adult_Validation_logReg <- subset(Adult_test_filter, logReg_sample == TRUE)
Adult_test_filter  <- subset(Adult_test_filter, logReg_sample == FALSE)

dim(Adult_Validation_logReg)
dim(Adult_test_filter)

# Logistic Regression 
logReg_Model02 <- glm(Income ~ ., data = Adult_train_filter, family = binomial('logit'))
logReg_predict <- predict(logReg_Model02,type="response",newdata =Adult_test_filter[,1:12] )
logReg_Class <- ifelse(logReg_predict > 0.5,1,0)
logReg_confMat <- table(Adult_test_filter$Income,logReg_Class)

# Accuracy 
accuracyNaiv <- (logReg_confMat[1,1]+logReg_confMat[2,2])/(logReg_confMat[1,1]+logReg_confMat[2,2]+logReg_confMat[1,2]+logReg_confMat[2,1])

precisio0_lg <- logReg_confMat[1,1] / (logReg_confMat[1,1]+logReg_confMat[1,2])
precisio1_lg <- logReg_confMat[2,2] /(logReg_confMat[2,2]+logReg_confMat[2,1])

recall0_lg <- logReg_confMat[1,1] / (logReg_confMat[1,1]+logReg_confMat[2,1])
recall1_lg <- logReg_confMat[2,2] / (logReg_confMat[2,2]+logReg_confMat[1,2])

# Validation of the Model 
logReg_Validation <- predict(logReg_Model02,type="response",newdata = Adult_Validation_logReg[,1:12])
logReg_Validation_Class <- ifelse(logReg_Validation > 0.5,1,0)
logtb<- table(Adult_Validation_logReg$Income,logReg_Validation_Class)


# Calculate Precission and Recall 
#Precission of class 0
precisio0 <- logtb[1,1] / (logtb[1,1]+logtb[1,2])
precisio1 <- logtb[2,2] /(logtb[2,2]+logtb[2,1])

recall0 <- logtb[1,1] / (logtb[1,1]+logtb[2,1])
recall1 <- logtb[2,2] / (logtb[2,2]+logtb[1,2])
# Combine training and testing 

Adult_AllData <- rbind(Adult_train_filter,Adult_test_filter)
dim(Adult_AllData)

precisionLt_0 <- list()
# N fold cross validation 
Datafolds=createFolds(Adult_AllData$Income,k=10)
logregmean=lapply(Datafolds,function(x){
  training_fold=Adult_AllData[-x,]
  test_fold=Adult_AllData[x,]
  mfold <- glm(Income ~ ., data = training_fold, family = binomial('logit'))
  predict_fold <- predict(mfold,type="response",newdata =test_fold[,1:12] )
  predictfoldClass <- ifelse(predict_fold > 0.5,1,0)
  confusionTable <- table(test_fold$Income,predictfoldClass)
  foldaccuracy=(confusionTable[1,1]+confusionTable[2,2])/(confusionTable[1,1]+confusionTable[2,2]+confusionTable[1,2]+confusionTable[2,1]) 
  return(foldaccuracy)
})

print(logregmean)

meanfold_reg <- mean(as.numeric(logregmean))
