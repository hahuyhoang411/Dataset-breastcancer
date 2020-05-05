data = read.csv("https://raw.githubusercontent.com/pnhuy/bioinfo/master/datasets/breast_data/breast-data.csv")
attach(data)

library(dplyr)
library(caret)
library(caTools)

data$Class= factor (data$Class, labels= c(0,1) , levels= c("no-recurrence-events", "recurrence-events"))
data$deg.malig= factor (data$deg.malig, labels= c(0,1,2) , levels= c("1", "2", "3"))
data$irradiat= factor (data$irradiat, labels= c(0,1) , levels= c("no", "yes"))

#LOGISTIC REGRESSION

log_data <- data[c(1,7,10)]

#shuffling
set.seed(1000)
shuf_ind <-sample(1:nrow(log_data))
log_data <-log_data[shuf_ind,]

#splitting data
library(caTools)
## Warning: package 'caTools' was built under R version 3.6.1
set.seed(1234)
split <-sample.split(log_data$irradiat,SplitRatio =0.8 )

#training set
training_set <-subset(log_data,split==T)

#test set
test_set <-subset(log_data,split==F)

View(training_set)
View(test_set)

#generalised linear model
classifier <-glm(formula=irradiat~. ,family = binomial(),data=training_set)
summary(classifier)
