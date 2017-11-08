# DownSampling

library(caret)

# We got training and testing 
sumClass1 <- sum(Adult_train_filter$Income==">50K")
sumClass2 <- sum(Adult_train_filter$Income=="<=50K")

# class 1: 7508 , Class 2: 22654
# Data is imbalanced
Adult_test_filter$Income <-factor(Adult_test_filter$Income,levels = c("<=50K",">50K"),labels = c(0,1))
Adult_train_filter$Income <- as.factor(Adult_train_filter$Income)
levels(Adult_train_filter$Income) <- make.names(levels(factor(Adult_train_filter$Income)))
levels(Adult_test_filter$Income) <- make.names(levels(factor(Adult_test_filter$Income)))


nmin <- sumClass1

ctrl <- trainControl(method = "cv",classProbs = TRUE,summaryFunction = twoClassSummary)

rfDownSample <- train(Income~., data=Adult_train_filter,method="rf",
                      ntree=1500,tunelength=5,metric="ROC",trControl=ctrl,
                      strata=Adult_train_filter$Income,sampsize=rep(nmin,2))

# before predicting convert the labels in test data 

Adult_test_filter$Income[Adult_test_filter$Income == "<=50K."]= "<=50K"
Adult_test_filter$Income[Adult_test_filter$Income == ">50K."]= ">50K"


downProbs <-predict(rfDownSample,Adult_test_filter,type="prob")[,1]
downProbs_Class <- ifelse(downProbs > 0.5,1,0)
tabD <- table (Adult_test_filter$Income,downProbs_Class)
table()


precisio_rfd <- tabD[1,1] / (tabD[1,1]+tabD[1,2])
precisio1_rfd <- tabD[2,2] /(tabD[2,2]+tabD[2,1])

recall0_rf <- tabD[1,1] / (tabD[1,1]+tabD[2,1])
recall1_rf <- tabD[2,2] / (tabD[2,2]+tabD[1,2])

