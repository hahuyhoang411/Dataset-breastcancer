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

#Relationship between colums
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

#Sidebyside barchart
ggplot(data=data,aes(x=age,fill=irradiat,))+geom_bar()

#Connect
ggplot(data,aes(tumor.size))+geom_bar()+facet_wrap(~irradiat)+theme(axis.text.x = element_text(angle = 90))+ggtitle("Irradiation Based on Tumor Size")+xlab("Tumor Size")+ylab("Count")
ggplot(data,aes(x=Class,fill=irradiat))+geom_bar(position ="dodge")
ggplot(data,aes(x=Tumoursize,fill=Menopause))+geom_bar(position="dodge")
ggplot(data,aes(x=age,fill=tumor.size))+geom_bar(position ="dodge")
ggplot(data,aes(x="",fill=menopause)) + geom_bar(width=3)+coord_polar("y")
