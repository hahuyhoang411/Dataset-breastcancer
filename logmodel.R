data = read.csv("https://raw.githubusercontent.com/pnhuy/bioinfo/master/datasets/breast_cancer/breast-cancer.csv")

attach(data)


library(dplyr)
library(caret)
library(caTools)

data$Class= factor (data$Class, labels= c(0,1) , levels= c("no-recurrence-events", "recurrence-events"))
data$deg.malig= factor (data$deg.malig, labels= c(0,1,2) , levels= c("1", "2", "3"))
data$irradiat= factor (data$irradiat, labels= c(0,1) , levels= c("no", "yes"))

#LOGISTIC REGRESSION

log_data <- data[c(1,7,10)]

#splitting data
set.seed(123)
split <-sample.split(log_data$Class,SplitRatio =0.8 )

#training set
training_set <-subset(log_data,split==T)

#test set
test_set <-subset(log_data,split==F)


#generalised linear model
classifier <- glm(formula = Class~. ,family = binomial(),data=training_set)
summary(classifier)
pre = predict(classifier,type="response",newdata=test_set[-1])
y_pre <- ifelse(pre>0.5,"recurrence","no_recurrence")
y_pre
