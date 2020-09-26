
#set working directory
path <- "C:/Users/Tesfaye D/Desktop/Projects/R/Regression"
setwd(path)

#load data and check data
mydata <- read.csv("airfoil_self_noise.csv")
str(mydata)

#check missing values
colSums(is.na(mydata))

#check correlation
cor(mydata)

#fit linear regression model
regmodel <- lm(Sound_pressure_level ~ ., data = mydata)
summary(regmodel)

#check residual distn

#set graphic output
par(mfrow=c(2,2))

#create residual plots
plot (regmodel)

regmodel <- update(regmodel, log(Sound_pressure_level)~.)
summary(regmodel)

plot (regmodel)

#sample slicing
set.seed(1)
d <- sample ( x = nrow(mydata), size = nrow(mydata)*0.7)
train <- mydata[d,] #1052 rows 
test <- mydata[-d,] #451 rows

#train model
regmodel <- lm (log(Sound_pressure_level)~.,data = train)
summary(regmodel)

#test model
regpred <- predict(regmodel, test)

#convert back to original value
regpred <- exp(regpred)

install.packages("Metrics")
library(Metrics)
rmse(actual = test$Sound_pressure_level,predicted = regpred)

#save the output of boxplot
d <- boxplot(train$Displacement,varwidth = T,outline = T,border = T,plot = T)
d$out #enlist outlier observations
