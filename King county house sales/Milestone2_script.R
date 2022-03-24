#Project Milestone 2
getwd()
setwd("C:\\Users\\tusha\\OneDrive\\Desktop\\MPS_Alalytics\\Prob_theory_&_stats\\Projects")
getwd()
KC<-read.csv("kc_house_data.csv", header = TRUE, sep = ",")
head(KC,5)
class(KC)
KC$date<-as.Date(KC$date,"%Y%m%dT000000")
head(KC,5)
#Question 1
mean(KC$price)
t.test(KC$price,alternative="greater",mu=345800,conf.level=0.90)
#Question 2
date1<-KC[KC$date>="2014-05-01" & KC$date<="2014-12-31",]
nrow(date1)
mean(date1$price)
var(date1$price)
date2<-KC[KC$date>="2015-01-01" & KC$date<="2015-05-31",]
nrow(date2)
mean(date2$price)
var(date2$price)
t.test(date1$price,date2$price,alternative="two.sided",conf.level=0.90)
