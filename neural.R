data = read.csv("https://raw.githubusercontent.com/pnhuy/bioinfo/master/datasets/breast_cancer/breast-cancer.csv")
attach(data)

library(dplyr)
library(caret)
library(keras)
library(caTools)

data = filter(data, breast.quad != '?' & node.caps != '?')

#Fix NA cua nodecaps
data$node.caps=ifelse(is.na(data$node.caps),ave(data$node.caps,FUN=function(x)"no"),data$node.caps)
#Fix NA cua breasquad
data$breast.quad=ifelse(is.na(data$breast.quad),ave(data$breast.quad,FUN=function(x)"left_low"),data$breast.quad)
#Kiem tra levels cua cac cot
factor (data$irradiat)
#label 
data$Class= factor (data$Class, labels= c(0,1) , levels= c("no-recurrence-events", "recurrence-events"))

data$age= factor (data$age,labels= c(0,1,2,3,4,5) , levels= c("20-29","30-39","40-49","50-59","60-69","70-79")) 

data$menopause= factor (data$menopause, labels= c(0,1,2) , levels= c("premeno","ge40","lt40")) 

data$tumor.size= factor (data$tumor.size, labels= c(0,1,2,3,4,5,6,7,8,9,10) , levels= c("0-4","5-9","10-14", "15-19", "20-24","25-29", "30-34", "35-39","40-44","45-49","50-54")) 

data$inv.nodes= factor (data$inv.nodes, labels= c(0,1,2,3,4,5,6) , levels= c("0-2","3-5","6-8", "9-11","12-14", "15-17", "24-26"))

data$node.caps= factor (data$node.caps, labels= c(0,1) , levels= c("2", "3"))

data$deg.malig= factor (data$deg.malig, labels= c(0,1,2) , levels= c("1", "2", "3"))

data$breast= factor (data$breast, labels= c(0,1) , levels= c("left", "right")) 

data$breast.quad= factor (data$breast.quad, labels= c(0,1,2,3,4) , levels= c("2", "3", "4", "5", "6")) 

data$irradiat= factor (data$irradiat, labels= c(0,1) , levels= c("no", "yes")) 

#hypothesistestClass~tumorsize
t.test(as.numeric(data$tumor.size)~data$Class)

set.seed(100)
split <- sample.split(data$Class, SplitRatio = 0.8)
y_train <- subset(data, split == TRUE)
y_train <- y_train[1]
y_train = as.matrix(y_train)
y_train <- as.numeric(y_train)

y_test <- subset(data, split == FALSE)
y_test <- y_test[1]
y_test = as.matrix(y_test)
y_test <- as.numeric(y_test)

data_col_1  <- colnames(data)
data_col_1 <- data_col_1[data_col_1 != "Class"]
data <- data[data_col_1]

x_train <- subset(data, split == TRUE)
x_train <- x_train[1:9]

x_train$age <- as.numeric(x_train$age)
x_train$menopause <- as.numeric(x_train$menopause)
x_train$tumor.size <- as.numeric(x_train$tumor.size)
x_train$inv.nodes <- as.numeric(x_train$inv.nodes)
x_train$node.caps <- as.numeric(x_train$node.caps)
x_train$deg.malig <- as.numeric(x_train$deg.malig)
x_train$breast <- as.numeric(x_train$breast)
x_train$breast.quad <- as.numeric(x_train$breast.quad)
x_train$irradiat <- as.numeric(x_train$irradiat)

x_train <- as.matrix(x_train)
x_train <- scale(x_train)

x_test <- subset(data, split == FALSE)
x_test <- x_test[1:9]
x_test$age <- as.numeric(x_test$age)
x_test$menopause <- as.numeric(x_test$menopause)
x_test$tumor.size <- as.numeric(x_test$tumor.size)
x_test$inv.nodes <- as.numeric(x_test$inv.nodes)
x_test$node.caps <- as.numeric(x_test$node.caps)
x_test$deg.malig <- as.numeric(x_test$deg.malig)
x_test$breast <- as.numeric(x_test$breast)
x_test$breast.quad <- as.numeric(x_test$breast.quad)
x_test$irradiat <- as.numeric(x_test$irradiat)

x_test <- as.matrix(x_test)
col_means_train <- attr(x_train, "scaled:center") 
col_stddevs_train <- attr(x_train, "scaled:scale")
x_test <- scale(x_test, center = col_means_train, scale = col_stddevs_train)

# Build neural network with AHD
model <- keras_model_sequential()
model %>%
  layer_dense(units = 32, activation = 'relu', input_shape = dim(x_train)[2]) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")


model %>% compile(
  optimizer = optimizer_adam(),
  loss = 'mse',
  metrics = c('accuracy')
)

model %>% fit(x_train, y_train, epoch = 200, batch_size = 64)

score <- model %>% evaluate(x_test, y_test)

p1 <- model %>% predict(x_test)

pred = as.factor(ifelse(p1 > 0.5 , '1','0'))


confusionMatrix(pred,factor(y_test))

