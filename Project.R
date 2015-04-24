library(caret)
library(ggplot2)
library(reshape)
library(kernlab)
library(randomForest)

# Please add both training and testing csv files to the current working directory
# before running the script

# read the csv file from current working directory for training 
pml_training <- read.csv("pml-training.csv", na.strings = c("NA",""," "))

# clean the data by removing columns with missing values
pml_training_NAs <- apply(data_training, 2, function(x) {sum(is.na(x))})
pml_training_clean <- pml_training[,which(pml_training_NAs == 0)]

# remove non-relevant columns such as user name and timestamps
pml_training_clean <- pml_training_clean[,-(1:7)]

# split the cleaned testing data into training and cross validation
inTrain <- createDataPartition(y = pml_training_clean$classe, p = 0.75, list = FALSE)
training <- pml_training_clean[inTrain, ]
crossval <- pml_training_clean[-inTrain, ]

# plot a correlation matrix
correlationMatrix <- cor(training[, -length(training)])
melted_cormat <- melt(correlationMatrix)
head(melted_cormat, 10L)
ggplot(data = melted_cormat, aes(x=X1, y=X2, fill=value)) + geom_tile()

# fit a model to predict the classe using everything else as a predictor
model <- randomForest(classe ~ ., data = training)

# crossvalidate the model using the remaining 25% of original training data
predictCrossVal <- predict(model, crossval)
confusionMatrix(crossval$classe, predictCrossVal)

# apply the same process to prepare test data
pml_test <- read.csv("pml-testing.csv", na.strings = c("NA",""," "))
pml_test_NAs <- apply(pml_test, 2, function(x) {sum(is.na(x))})
pml_test_clean <- pml_test[,which(data_test_NAs == 0)]
pml_test_clean <- pml_test_clean[,-(1:7)]

# predict the classes of the test data set
predictTest <- predict(model, pml_test_clean)