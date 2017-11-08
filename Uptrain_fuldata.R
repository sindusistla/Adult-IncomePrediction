# We got data 
library(caret)

dim(Adult_train_filter)
dim(Adult_test_filter)

set.seed(670)
dim(Adult_train_filter)
colnames(Adult_test_filter)[13] <- "Class" 
colnames(Adult_train_filter)[13] <- "Class" 
Adult_test_filter$Class[Adult_test_filter$Class == "<=50K."]= "<=50K"
Adult_test_filter$Class[Adult_test_filter$Class == ">50K."]= ">50K"


Adult_test_filter$Class <- as.factor(Adult_test_filter$Class)
Adult_train_filter$Class <- as.factor(Adult_train_filter$Class)

up_train <- upSample(x=Adult_train_filter[,-ncol(Adult_train_filter)],y=Adult_train_filter$Class)

table(up_train$Class)

levels(up_train$Class) <- make.names(levels(factor(up_train$Class)))


ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     ## new option here:
                     sampling = "up")

up_outside <- train(Class ~ ., data = up_train, 
                    method = "treebag",
                    nbagg = 50,
                    metric = "ROC",
                    trControl = ctrl)

# Preprocess the test data 
# now predict the values 
# X..50K -> 1
# X.50K -> 0
levels(Adult_test_filter$Class) <- make.names(levels(factor(Adult_test_filter$Class)))
dim(up_train)
dim(Adult_test_filter)
Adult_test_filter <- as.data.frame(Adult_test_filter)
predict_values <- predict(up_outside, Adult_test_filter[,-13],type='prob')[,"X..50K"]
predict_values_class <- ifelse(predict_values>0.5,"X..50K","X.50K")

table(predict_values_class)

table(Adult_test_filter$Class)

uptraintab<- table(Adult_test_filter$Class,predict_values_class)

uptrainaccuracy <- (uptraintab[1,1]+uptraintab[2,2])/(uptraintab[1,1]+uptraintab[2,2]+uptraintab[1,2]+uptraintab[2,1])

precisio_rf <- uptraintab[1,1] / (uptraintab[1,1]+uptraintab[1,2])
precisio1_rf <- uptraintab[2,2] /(uptraintab[2,2]+uptraintab[2,1])

recall0_rf <- uptraintab[1,1] / (uptraintab[1,1]+uptraintab[2,1])
recall1_rf <- uptraintab[2,2] / (uptraintab[2,2]+uptraintab[1,2])

