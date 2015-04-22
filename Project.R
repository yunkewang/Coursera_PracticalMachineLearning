library(caret)
library(ggplot2)
library(kernlab)
library(randomForest)

# Please add both training and testing csv files to the current working directory
# before running the script

# read the csv file from current working directory for training 
data_training <- read.csv("pml-training.csv", na.strings= c("NA",""," "))

# clean the data by removing columns with NAs etc
data_training_NAs <- apply(data_training, 2, function(x) {sum(is.na(x))})
data_training_clean <- data_training[,which(data_training_NAs == 0)]

# remove identifier columns such as name, timestamps etc
data_training_clean <- data_training_clean[8:length(data_training_clean)]

# split the cleaned testing data into training and cross validation
inTrain <- createDataPartition(y = data_training_clean$classe, p = 0.7, list = FALSE)
training <- data_training_clean[inTrain, ]
crossval <- data_training_clean[-inTrain, ]