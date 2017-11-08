# Naive Baise 

library(naivebayes)

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


nB_Model01 <- naive_bayes(Adult_train_filter[,1:12],Adult_train_filter$Income)
nB_predict <- predict(nB_Model01,Adult_test_filter[,1:12])

NB_confusionMat <- table(Adult_test_filter$Income,nB_predict)

# Accuracy 
accuracyNaiv <- (NB_confusionMat[1,1]+NB_confusionMat[2,2])/(NB_confusionMat[1,1]+NB_confusionMat[2,2]+NB_confusionMat[1,2]+NB_confusionMat[2,1])

# Precision tp / tp+fp
precisio0_nb <- NB_confusionMat[1,1] / (NB_confusionMat[1,1]+NB_confusionMat[1,2])
precisio1_nb <- NB_confusionMat[2,2] /(NB_confusionMat[2,2]+NB_confusionMat[2,1])

recall0_nb <- NB_confusionMat[1,1] / (NB_confusionMat[1,1]+NB_confusionMat[2,1])
recall1_nb <- NB_confusionMat[2,2] / (NB_confusionMat[2,2]+NB_confusionMat[1,2])
