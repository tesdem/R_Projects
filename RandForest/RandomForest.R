path <- "C:/Users/Tesfaye D/Desktop/Projects/R/RandomForest"
setwd(path)

library(randomForest)
require(caTools)

data <- read.csv(
  "processed.cleveland.data",
  header=FALSE
)

dim(data)

names(data) <- c("age", "sex", "cp", "trestbps", "choi", "fbs", 
                 "restecg", "thalach", "exang", "oldpeak", "slope", 
                 "ca", "thai", "num")
head(data)
data$num[data$num > 1] <- 1

summary(data)
sapply(data, class)

data <- transform(
  data,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  choi=as.integer(choi),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thai=as.factor(thai),
  num=as.factor(num)
)
sapply(data, class)

summary(data)
data[ data == "?"] <- NA
colSums(is.na(data))

data$thai[which(is.na(data$thai))] <- as.factor("3.0")
data <- data[!(data$ca %in% c(NA)),]
colSums(is.na(data))

summary(data)
data$ca <- factor(data$ca)
data$thai <- factor(data$thai)
summary(data)

sample = sample.split(data$num, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

rf <- randomForest(
  num ~ .,
  data=train
)

rf

pred = predict(rf, newdata=test[-14])
pred
cm = table(test[,14], pred)
cm
