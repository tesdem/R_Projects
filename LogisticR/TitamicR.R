#set working directory
path <- "C:/Users/Tesfaye D/Desktop/Projects/R/LogisticR"
setwd(path)

#load libraries and data
library (data.table)
library (plyr)
library (stringr)
train <- fread("train.csv",na.strings = c(""," ",NA,"NA"))
test <- fread("test.csv",na.strings = c(""," ",NA,"NA"))

head(train)
head(test)
str(train)

#check missing values
colSums(is.na(train))
colSums(is.na(test))

#Quick Data Exploration
summary(train$Age)
summary(test$Age)

train[,.N/nrow(train),Pclass]
test[,.N/nrow(test),Pclass]

train [,.N/nrow(train),Sex]
test [,.N/nrow(test),Sex]

train [,.N/nrow(train),SibSp]
test [,.N/nrow(test),SibSp]

train [,.N/nrow(train),Parch]
test [,.N/nrow(test),Parch] #extra 9

summary(train$Fare)
summary(test$Fare)

train [,.N/nrow(train),Cabin]
test [,.N/nrow(test),Cabin]

train [,.N/nrow(train),Embarked] 
test [,.N/nrow(test),Embarked]

#combine data
alldata <- rbind(train,test,fill=TRUE)

#New Variables
#Extract passengers title
alldata [,title := strsplit(Name,split = "[,.]")]
alldata [,title := ldply(.data = title,.fun = function(x) x[2])]
alldata [,title := str_trim(title,side = "left")]

#combine titles
alldata [,title := replace(title, which(title %in% c("Capt","Col","Don","Jonkheer","Major","Rev","Sir")), "Mr"),by=title]
alldata [,title := replace(title, which(title %in% c("Lady","Mlle","Mme","Ms","the Countess","Dr","Dona")),"Mrs"),by=title]

#ticket binary coding
alldata [,abs_col := strsplit(x = Ticket,split = " ")]
alldata [,abs_col := ldply(.data = abs_col,.fun = function(x)length(x))]
alldata [,abs_col := ifelse(abs_col > 1,1,0)]

#Impute Age with Median
for(i in "Age")
  set(alldata,i = which(is.na(alldata[[i]])),j=i,value = median(alldata$Age,na.rm = T))

#Remove rows containing NA from Embarked
alldata <- alldata[!is.na(Embarked)]

#Impute Fare with Median
for(i in "Fare")
  set(alldata,i = which(is.na(alldata[[i]])),j=i,value = median(alldata$Fare,na.rm = T))

#Replace missing values in Cabin with "Miss"
alldata [is.na(Cabin),Cabin := "Miss"]

#Log Transform Fare
alldata$Fare <- log(alldata$Fare + 1)

#Impute Parch 9 to 0
alldata [Parch == 9L, Parch := 0]

#Collect train and test
train <- alldata[!(is.na(Survived))]
train [,Survived := as.factor(Survived)]

test <- alldata[is.na(Survived)]
test [,Survived := NULL]

#Logistic Regression
model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = train[,-c("PassengerId","Name","Ticket")])
summary(model)

#run anova
anova(model, test = 'Chisq')

#another model for AIC comparison
model2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare + title, data = train,family = binomial(link="logit"))
summary(model2)

#compare two models
anova(model,model2,test = "Chisq")

#partition and create training, testing data
install.packages("caret")
library(caret)
split <- createDataPartition(y = train$Survived,p = 0.6,list = FALSE)

new_train <- train[split] 
new_test <- train[-split]

#model training and prediction
log_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Fare + title, data = new_train[,-c("PassengerId","Name","Ticket")],family = binomial(link="logit"))
log_predict <- predict(log_model,newdata = new_test,type = "response")
log_predict <- ifelse(log_predict > 0.5,1,0)

#plot ROC 
install.packages("ROCR","Metrics")
library(ROCR) 
library(Metrics)
pr <- prediction(log_predict,new_test$Survived)
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
plot(perf) > auc(new_test$Survived,log_predict) #0.76343

#imporove AUC
log_predict <- predict(log_model,newdata = new_test,type = "response")
log_predict <- ifelse(log_predict > 0.6,1,0)

pr <- prediction(log_predict,new_test$Survived)
perf <- performance(pr,measure = "tpr",x.measure = "fpr")
plot(perf)
auc(new_test$Survived,log_predict)
