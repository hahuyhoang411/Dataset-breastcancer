data = read.csv("https://raw.githubusercontent.com/pnhuy/bioinfo/master/datasets/breast_cancer/breast-cancer.csv")
head(data)

dim(data)
str(data)

#table
table(data$Class,data$age)

library(dplyr)

summary(data)

#du lieu duoc loc theo nhom tuoi
AGE = data%>%group_by(age)
AGE

#ty le mac ung thu vu (nhom tuoi)
a<-data%>%count(age)
n<-nrow(data)
b=vector()
for(i in a$n){
  b=c(b,(i/n)*100)
}
a=data.frame(a,tyle=b)
a

#do thi
library(ggplot2)
plot(a$tyle,type="o",main="do tuoi so voi ty le mac",xlab="dotuoi",ylab="tyle")
