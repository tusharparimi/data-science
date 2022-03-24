#Project Milestone 2
getwd()
setwd("C:\\Users\\tusha\\OneDrive\\Desktop\\MPS_Alalytics\\Prob_theory_&_stats\\Projects")
getwd()
KC<-read.csv("kc_house_data.csv", header = TRUE, sep = ",")
head(KC,5)
class(KC)
KC$date<-as.Date(KC$date,"%Y%m%dT000000")
head(KC,5)
#Question1 Correlation test between price and grade of the houses
x<-KC$grade
y<-KC$price
cor(x,y)
cor(x, y,  method = "pearson")
cor.test(x, y, method=c("pearson"))
library(ggplot2)
ggplot(KC,aes(x=grade,y=price))+geom_point()+labs(title="Scatter plot of house grade vs price")+geom_smooth(method=lm,se=FALSE,color="blue")
#Question2 Regression analysis
y<-KC$price
x1<-KC$grade
x2<-KC$bedrooms
model<-lm(y~x1+x2)
model
summary(model)
library(car)
avPlots(model)
