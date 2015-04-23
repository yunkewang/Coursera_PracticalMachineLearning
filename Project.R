library(caret)
library(e1071)
library(ggplot2)
library(reshape)
library(ggthemes)
library(kernlab)
library(randomForest)

# Please add both training and testing csv files to the current working directory
# before running the script

# read the csv file from current working directory for training 
pml_training <- read.csv("pml-training.csv", na.strings = c("NA",""," "))

# clean the data by removing columns with NAs etc
pml_training_NAs <- apply(data_training, 2, function(x) {sum(is.na(x))})
pml_training_clean <- pml_training[,which(pml_training_NAs == 0)]

# remove non-relevant columns such as user name and timestamps
pml_training_clean <- pml_training_clean[,-(1:7)]

# split the cleaned data into training and cross validation
inTrain <- createDataPartition(y = pml_training_clean$classe, p = 0.75, list = FALSE)
training <- pml_training_clean[inTrain, ]
crossval <- pml_training_clean[-inTrain, ]

# plot a correlation matrix
corMat <- cor(training[, -length(training)])
melted_cormat <- melt(corMat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=X1, y=X2, fill=value)) + geom_tile()

# fit a random forest model to predict the classe
model <- randomForest(classe ~ ., data = training)
