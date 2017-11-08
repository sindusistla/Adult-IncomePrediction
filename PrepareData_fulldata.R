
# Import the data 
#Ad_Data_Raw <- read.csv(file="C:/Users/sindu/Google Drive/Oakland University1/Information Retrieval/Project/Adult_Data.txt",header = FALSE,sep=",",strip.white = TRUE)
Ad_Data_Raw <- Adult_Data
dim(Ad_Data_Raw)

Ad_Data_Test <- Adult_Data_test
#Ad_Data_Test <- read.csv(file="C:/Users/sindu/Google Drive/Oakland University1/Information Retrieval/Project/Adult_Data_test.txt",header = FALSE,sep=",",strip.white = TRUE)

dim(Ad_Data_Test)

# Add column names 
colnames(Ad_Data_Raw) <- c("Age","WorkClass","Fnlwgt","Education","Education_num",
                                "Marital_Status","Occupation","Relationship","Race","Sex",
                                "Capital_gain","Capital_loss","Hours_per_week","Native_Country","Income")
colnames(Ad_Data_Test) <- c("Age","WorkClass","Fnlwgt","Education","Education_num",
                           "Marital_Status","Occupation","Relationship","Race","Sex",
                           "Capital_gain","Capital_loss","Hours_per_week","Native_Country","Income")

# Missing Values
table(Ad_Data_Raw$WorkClass)
table(Ad_Data_Raw$Occupation)
table(Ad_Data_Raw$Native_Country)

table(Ad_Data_Test$WorkClass)
table(Ad_Data_Test$Occupation)
table(Ad_Data_Test$Native_Country)

# Calculate the percentage of missing values 
colSums(Ad_Data_Raw=="?")
Ad_Data_Raw[Ad_Data_Raw=="?"] <- NA
Ad_Data_Train_nonulls <- na.omit(Ad_Data_Raw)
dim(Ad_Data_Train_nonulls)

# Calculate Missing values in Test Data
Ad_Data_Test[Ad_Data_Test=="?"] <- NA
Ad_Data_Test_nonulls <- na.omit(Ad_Data_Test)
dim(Ad_Data_Test_nonulls)

# Check the distribution of classes 
colSums(as.matrix(Ad_Data_Raw$Income) == "<=50K")
colSums(as.matrix(Ad_Data_Raw$Income) == ">50K")

colSums(as.matrix(Ad_Data_Test$Income) == "<=50K.")
colSums(as.matrix(Ad_Data_Test$Income) == ">50K.")

# Prepare the data by grouping 
# Eliminate the 
Adult_train_filter <- Ad_Data_Train_nonulls[,c(1,2,3,5,6,7,8,9,10,11,12,13,15)]
Adult_test_filter <- Ad_Data_Test_nonulls[,c(1,2,3,5,6,7,8,9,10,11,12,13,15)]

