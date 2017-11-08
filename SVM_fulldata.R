library(e1071)

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


classifier =svm(formula=Income~.,data=Adult_train_filter,type="C-classification",kernel="linear")
pred <- predict(classifier,Adult_test_filter[,-13])

tabsvm <- table(Adult_test_filter$Income,pred) 

accuracysvm <- (tabsvm[1,1]+tabsvm[2,2])/(tabsvm[1,1]+tabsvm[2,2]+tabsvm[1,2]+tabsvm[2,1])

# Precision tp / tp+fp
precisio0_svm <- tabsvm[1,1] / (tabsvm[1,1]+tabsvm[1,2])
precisio1_svm <- tabsvm[2,2] /(tabsvm[2,2]+tabsvm[2,1])

recall0_svm <- tabsvm[1,1] / (tabsvm[1,1]+tabsvm[2,1])
recall1_svm <- tabsvm[2,2] / (tabsvm[2,2]+tabsvm[1,2])

