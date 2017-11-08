library(caret)
library(class)
library(caTools)
library(stats)
library(naivebayes)
library(randomForest)


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


# Consider 3 weak learners for the modeling 

# Naive Bayes 
nB_Model_majority <- naive_bayes(Adult_train_filter[,1:12],Adult_train_filter$Income)

# Randomforest
RandomF_model_majority <- randomForest(x=Adult_train_filter[,1:12],y=Adult_train_filter$Income,ntree = 10,
                                importance = TRUE,keep.forest = TRUE)
# logistic Regression 
logReg_Model_majority <- glm(Income ~ ., data = Adult_train_filter, family = binomial('logit'))


# for these weak learners we need to ensemble with Majority voting 
# Predict using the three algorithms 
nB_predict_majority <- as.matrix(predict(nB_Model_majority,Adult_test_filter[,1:12]))
logreg_Predict_majority <- predict(logReg_Model_majority,type="response",newdata =Adult_test_filter[,1:12] )
logreg_Predict_majority_class <- as.matrix(ifelse(logreg_Predict_majority>0.5,1,0))
rf_pred_majority <- as.matrix(predict(RandomF_model_majority,newdata =Adult_test_filter[,1:12] ))
# form a matrix 
All_prediction_Matrix <- cbind(nB_predict_majority,logreg_Predict_majority_class,rf_pred_majority)
# Calculate the majority 

head(All_prediction_Matrix)
Majority_class <- as.factor(ifelse(nB_predict_majority==1 & logreg_Predict_majority_class== 1,1,
                                   ifelse(rf_pred_majority==1 & logreg_Predict_majority_class==1,1,
                                  ifelse(rf_pred_majority==1 & nB_predict_majority==1,1,0))))

head(Majority_class)
# Achieved the votes 

majoritytab<- table(Adult_test_filter$Income,Majority_class)

accuracy_maj <- (majoritytab[1,1]+majoritytab[2,2])/(majoritytab[1,1]+majoritytab[2,2]+majoritytab[1,2]+majoritytab[2,1])

# Calculate Precission and Recall 
#Precission of class 0
precisio0_maj <- majoritytab[1,1] / (majoritytab[1,1]+majoritytab[1,2])
precisio1_maj <- majoritytab[2,2] /(majoritytab[2,2]+majoritytab[2,1])

recall0_maj <- majoritytab[1,1] / (majoritytab[1,1]+majoritytab[2,1])
recall1_maj <- majoritytab[2,2] / (majoritytab[2,2]+majoritytab[1,2])


