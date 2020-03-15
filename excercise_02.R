getwd()
rm(list = ls())
mydata<-read.csv("xy.csv")
head(mydata)
plot(mydata$y~mydata$x,main="y~x")
par(mfrow=c(1,2))
boxplot(mydata$x,main="x",sub=paste("Outlier rows:",
                                    boxplot.stats(mydata$x)$out))
boxplot(mydata$y,main="y",sub=paste("Outlier rows",
                                    boxplot.stats(mydata$y)$out))
#检查数据，包括是否具有线性关系，有无异常值，服从正态分布等
library(e1071)
par(mfrow=c(1,2))
plot(density(mydata$x),main="Density plot: x",ylab="Frequency",
     sub=paste("Skewness:",round(e1071::skewness(cars$dist),2)))
polygon(density(mydata$x),col="red")
plot(density(mydata$y),main="Density plot: y",ylab="Frequency",
     sub=paste("Skewness:",round(e1071::skewness(cars$dist),2)))
polygon(density(mydata$y),col="red")

##计算相关系数并建模
cor(mydata$x,mydata$y)
linearMod<-lm(y~x,data=mydata)
print(linearMod)

summary(linearMod)

