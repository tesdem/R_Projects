#set working directory
path <- "C:/Users/Tesfaye D/Desktop/Projects/R/RandomForest"
setwd(path)

install.packages("caret", dependencies = TRUE)
install.packages("randomForest")
library(caret)
library(randomForest)
library (data.table)
library (plyr)
library (stringr)
train <- read.table('train.csv', sep = ",", header = TRUE)
test <- read.table('test.csv', sep = ",", header = TRUE)

head(train)
head(test)

table(train[,c('Survived', 'Pclass')])
install.packages("fields")
library(fields)
bplot.xy(train$Survived, train$Age)

summary(train$Age)
bplot.xy(train$Survived, train$Fare)
summary(train$Fare)

str(train)
# Converting 'Survived' to a factor
train$Survived <- factor(train$Survived)
str(train)
# Set a random seed
set.seed(51)
# Training using 'random forest' algorithm
model <- train(Survived ~ Pclass + Sex + SibSp +
                 Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
               data = train, # Use the train data frame as the training data
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trainControl(method = 'cv'), # Use cross-validation
                                        number = 5) # Use 5 folds for cross-validation

model

summary(test)
test$Fare <- ifelse(is.na(test$Fare), mean(test$Fare, na.rm = TRUE), test$Fare)
test$Survived <- predict(model, newdata = test)
test$Survived
