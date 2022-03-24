#Project Milestone 1
getwd()
setwd("C:\\Users\\tusha\\OneDrive\\Desktop\\MPS_Alalytics\\Prob_theory_&_stats\\Projects")
getwd()
KC<-read.csv("kc_house_data.csv", header = TRUE, sep = ",")
head(KC,5)
class(KC)
KC$date<-as.Date(KC$date,"%Y%m%dT000000")
head(KC,5)
str(KC)
nrow(KC)
summary(KC)
library(ggplot2)
ggplot(KC,aes(price))+geom_histogram(color="darkblue",fill="lightblue")+labs(title="King County house prices histogram",x="Price",y="No. of houses")

ggplot(KC,aes(x=grade,y=price))+geom_point()+labs(title="Scatter plot of house grade vs price")


ggplot(KC, aes(y=price)) + geom_boxplot(outlier.colour="red")+labs(title="Boxplot of prices of houses in King county")

#dividing into subsets based on year 2014 and 2015
class(KC$date)
date1<-KC[KC$date>="2014-05-01" & KC$date<="2014-12-31",]
nrow(date1)
summary(date1)
date2<-KC[KC$date>="2015-01-01" & KC$date<="2015-05-31",]
nrow(date2)
summary(date2)
#year 2014
ggplot(date1,aes(price))+geom_histogram(color="black",fill="grey")+labs(title="King County house prices histogram for year 2014",x="Price",y="No. of houses")

ggplot(date1,aes(x=bedrooms,y=price))+geom_point()+labs(title="Scatter plot of house grade vs price in year 2014")

ggplot(date1, aes(y=price)) + geom_boxplot(outlier.colour="darkgreen")+labs(title="Boxplot of prices of houses in King county for year 2014")

#year 2015
ggplot(date2,aes(price))+geom_histogram(color="black",fill="grey")+labs(title="King County house prices histogram for year 2015",x="Price",y="No. of houses")

ggplot(date2,aes(x=bedrooms,y=price))+geom_point()+labs(title="Scatter plot of house grade vs price in year 2015")

ggplot(date2, aes(y=price)) + geom_boxplot(outlier.colour="blue")+labs(title="Boxplot of prices of houses in King county for year 2015")


