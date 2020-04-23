data = read.csv("https://raw.githubusercontent.com/pnhuy/bioinfo/master/datasets/breast_cancer/breast-cancer.csv")
head(data)

str(data)
summary(data)
#table
table(data$Class,data$age)

#library
library(dplyr)
library(car)
library(MASS)
library(ggplot2)
library(Hmisc)
library(gridExtra)
library(psych)

data = filter(data, breast.quad != '?' & node.caps != '?')

#do thi
p1=ggplot(data, aes(age)) + geom_bar(aes(fill=age))+labs(title = "Age")
p2=ggplot(data, aes(Class)) +geom_bar(aes(fill=Class))+labs(title = "Class")
p3=ggplot(data, aes(irradiat)) + geom_bar(aes(irradiat))+labs(title = "irradiat")
p4=ggplot(data, aes(breast.quad)) + geom_bar(aes(fill=breast.quad))+labs(title = "breast.quad")
p5=ggplot(data, aes(breast)) + geom_bar(aes(fill=breast))+labs(title = "breast")
p6=ggplot(data, aes(node.caps)) + geom_bar(aes(fill=node.caps))+ labs(title = "node.caps")
p7=ggplot(data, aes(menopause)) + geom_bar(aes(fill=menopause))+labs(title = "menopause")
p8=ggplot(data, aes(tumor.size)) + geom_bar(aes(fill=tumor.size))+labs(title = "Tumor Size")
p9=ggplot(data, aes(inv.nodes)) + geom_bar(aes(fill=inv.nodes))+labs(title = "inv nodes")
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9)

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

data$deg.malig= factor (data$deg.malig, labels= c(0,1,2) , levels= c("1", "2", "3"))


data$breast= factor (data$breast, labels= c(0,1) , levels= c("left", "right")) 

data$breast.quad= factor (data$breast.quad, labels= c(0,1,2,3,4) , levels= c("2", "3", "4", "5", "6")) 

data$irradiat= factor (data$irradiat, labels= c(0,1) , levels= c("no", "yes")) 
data

#phantichdulieu
describe(data)


#Testing
pairs.panels(data)
t.test(as.numeric(data$age)~data$irradiat)
ANOVA = aov(as.numeric(data$menopause)~data$node.caps, data=data)
ANOVA
TukeyHSD(ANOVA)

