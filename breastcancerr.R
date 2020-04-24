#Load the require library
library(dplyr)
library(Hmisc)
library(ggplot2)
library(plotly)
library(gridExtra)

#Load the data in csv file
data <- read.csv("https://github.com/pnhuy/bioinfo/raw/master/datasets/breast_cancer/breast-cancer.csv")

#Get some insight of data
head(data)
tail(data)
summary(data)
dim(data)

#Structure
str(data)
View(data)
#Missing Value
data = na.omit(data)
sum(is.na(data))
colSums(is.na(data))

#Kiem tra levels cua cac cot
factor (data$irradiat)

#Fix l???i ?
data = filter(data, breast.quad != '?' & node.caps != '?')

#Bar plots for one variable
ggplot(data, aes(age)) + geom_bar(aes(fill=age))+labs(title = "Age")
p2 = ggplot(data, aes(Class)) +geom_bar(aes(fill=Class))+labs(title = "Class")
p3= ggplot(data, aes(irradiat)) + geom_bar(aes(fill=irradiat))+labs(title = "irradiat")
p4 = ggplot(data, aes(breast.quad)) + geom_bar(aes(fill=breast.quad))+labs(title = "breast.quad")
p5 = ggplot(data, aes(breast)) + geom_bar(aes(fill=breast))+labs(title = "breast")
p6 = ggplot(data, aes(node.caps)) + geom_bar(aes(fill=node.caps))+ labs(title = "node.caps")
p7 = ggplot(data, aes(menopause)) + geom_bar(aes(fill=menopause))+labs(title = "menopause")
p8 = ggplot(data, aes(tumor.size)) + geom_bar(aes(fill=tumor.size))+labs(title = "Tumor Size")
p9 = ggplot(data, aes(inv.nodes)) + geom_bar(aes(fill=inv.nodes))+labs(title = "inv nodes")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9)

# Plot of multiple variables
ggplot(data=data,aes(x=age,fill=irradiat,))+geom_bar()
ggplot(data,aes(tumor.size))+geom_bar()+facet_wrap(~irradiat)+theme(axis.text.x = element_text(angle = 90))+ggtitle("Irradiation Based on Tumor Size")+xlab("Tumor Size")+ylab("Count")
ggplot(data,aes(x=Class,fill=irradiat))+geom_bar(position ="dodge")
ggplot(data,aes(x=Tumoursize,fill=Menopause))+geom_bar(position="dodge")
ggplot(data,aes(x=age,fill=tumor.size))+geom_bar(position ="dodge")
##Piechart
ggplot(data,aes(x="",fill=menopause)) + geom_bar(width=3)+coord_polar("y")

#NA
sum(is.na(data))

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

log_data <-cancer[c(1,7,10)]
View(log_data)
cancer$Deg_malig=factor(cancer$Deg_malig,levels=c(1,2,3))



#shuffling
set.seed(1000)
shuf_ind <-sample(1:nrow(log_data))
log_data <-log_data[shuf_ind,]

#splitting data
library(caTools)
## Warning: package 'caTools' was built under R version 3.6.1
set.seed(1234)
split <-sample.split(log_data$Irradiat,SplitRatio =0.8 )

#training set
training_set <-subset(log_data,split==T)

#test set
test_set <-subset(log_data,split==F)

View(training_set)
View(test_set)

#generalised linear model
classifier <-glm(formula=Irradiat~. ,family = binomial(),data=training_set)
summary(classifier)


data$breast= factor (data$breast, labels= c(0,1) , levels= c("left", "right")) 

data$breast.quad= factor (data$breast.quad, labels= c(0,1,2,3,4) , levels= c("2", "3", "4", "5", "6")) 

data$irradiat= factor (data$irradiat, labels= c(0,1) , levels= c("no", "yes")) 
data


#Model building
#Logistic Regression
#generalised linear model
classifier <-glm(irradiat ~ tumor.size + node.caps,family = 'binomial',data=data)
summary(classifier)
log_data <-data[c(1,7,10)]
View(log_data)
cancer$Deg_malig=factor(cancer$Deg_malig,levels=c(1,2,3))



#shuffling
set.seed(1000)
shuf_ind <-sample(1:nrow(log_data))
log_data <-log_data[shuf_ind,]

#splitting data
library(caTools)
## Warning: package 'caTools' was built under R version 3.6.1
set.seed(1234)
split <-sample.split(log_data$Irradiat,SplitRatio =0.8 )

#training set
training_set <-subset(log_data,split==T)

#test set
test_set <-subset(log_data,split==F)

View(training_set)
View(test_set)

#generalised linear model
classifier <-glm(formula=Irradiat~. ,family = binomial(),data=training_set)
summary(classifier)