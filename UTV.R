data = read.csv("https://raw.githubusercontent.com/pnhuy/bioinfo/master/datasets/breast_cancer/breast-cancer.csv")
head(data)

str(data)
summary(data)

#library
library(dplyr)
library(car)
library(MASS)
library(ggplot2)
library(Hmisc)
library(gridExtra)
library(psych)

sum(is.na(data))

#Fix ? cua nodecaps:
data$node.caps[data$node.caps=="?"]="yes"
#Fix NA cua nodecaps
data$node.caps=ifelse(is.na(data$node.caps),ave(data$node.caps,FUN=function(x)"no"),data$node.caps)
#Fix ? cua breastquad
data$breast.quad[data$breast.quad=="?"]="left_low"
#Fix NA cua breasquad
data$breast.quad=ifelse(is.na(data$breast.quad),ave(data$breast.quad,FUN=function(x)"left_low"),data$breast.quad)
#Kiem tra levels cua cac cot
factor (data$irradiat)
#label 
data$Class= factor (data$Class, labels= c(0,1) , levels= c("no-recurrence-events", "recurrence-events"))

data$age= factor (data$age,labels= c(0,1,2,3,4,5) , levels= c("20-29","30-39","40-49","50-59","60-69","70-79")) 

data$meno.pause= factor (data$menopause, labels= c(0,1,2) , levels= c("premeno","ge40","lt40")) 

data$tumor.size= factor (data$tumor.size, labels= c(0,1,2,3,4,5,6,7,8,9,10) , levels= c("0-4", "10-14", "15-19", "20-24","25-29", "30-34", "35-39","40-44","45-49","5-9","50-54")) 

data$inv.nodes= factor (data$inv.nodes, labels= c(0,1,2,3,4,5,6) , levels= c("0-2", "12-14", "15-17", "24-26", "3-5", "6-8", "9-11"))


data$node.caps= factor (data$node.caps, labels= c(0,1) , levels= c("2", "3"))

data$deg.malig= factor (data$deg.malig, labels= c(0,1,2) , levels= c("1", "2", "3"))


data$breast= factor (data$breast, labels= c(0,1) , levels= c("left", "right")) 

data$breast.quad= factor (data$breast.quad, labels= c(0,1,2,3,4) , levels= c("2", "3", "4", "5", "6")) 

data$irradiat= factor (data$irradiat, labels= c(0,1) , levels= c("no", "yes")) 
data

#phantichdulieu
describe(data)

#table
table(data$Class,data$age)

#do thi
plot(a$tyle,type="o",main="do tuoi so voi ty le mac",xlab="dotuoi",ylab="tyle")

#plot?
plot(data$age, data$deg.malig)

#Testing
pairs.panels(data)

#model
