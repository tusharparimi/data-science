#PART 1:

# Total inventory cost= Holding costs + Ordering costs
#annual_ordering_costs<-(order_cost*annual_demand)/order_quantity
#annual_holding_costs<-((annual_per_unit_carry_cost*order_quantity)/2)
annual_demand<-15000
unit_cost<-80
annual_per_unit_carry_cost<-(18/100)*unit_cost
order_cost<-220
order_quantity<-seq(1,2000,by=1)
total_cost<-((order_cost*annual_demand)/order_quantity)+((annual_per_unit_carry_cost*order_quantity)/2)
plot(order_quantity,total_cost,'l',main='Total Cost vs Order quantity',xlab='Order quantity (units)',ylab='Total Cost (dollars)')

#Zooming in:
order_quantity<-seq(500,1000,by=1)
total_cost<-((order_cost*annual_demand)/order_quantity)+((annual_per_unit_carry_cost*order_quantity)/2)
plot(order_quantity,total_cost,'l',main='Total Cost vs Order quantity',xlab='Order quantity (units)',ylab='Total Cost (dollars)')

#using order quantity and total costs value to get approx. minimum total cost and corresponding order quantity
order_quantity[which.min(total_cost)]
total_cost[which.min(total_cost)]

#Now using optimise() in R to solve this minimization problem
my_interval<-c(600,700)
my_function<-function(order_quantity) ((order_cost*annual_demand)/order_quantity)+((annual_per_unit_carry_cost*order_quantity)/2)
my_optimization<-optimise(f=my_function,interval=my_interval,lower=min(my_interval),upper=max(my_interval),maximum=FALSE,tol=.Machine$double.eps^0.5)
my_optimization

paste('Total inventory cost minimizing order quantity:',round(my_optimization[[1]],1),'units')
paste('Minimum total inventory cost:',round(my_optimization[[2]],1),'dollars')

#PART 2:
#given random_annual_demand comes from a triangular distribution with min=13000,max=17000,mode=15000

#generating 1000 random values of triangular distribution
library(triangulr)
set.seed(10)
random_annual_demand<-rtri(1000,13000,17000,15000)
random_annual_demand


unit_cost<-80
annual_per_unit_carry_cost<-(18/100)*unit_cost
order_cost<-220
my_interval<-c(600,700)

Min_total_inventory_cost<-numeric()
Order_quantity_corres<-numeric()
annual_no_of_orders<-numeric()
for(d in random_annual_demand){
  new_value<-optimise(function(order_quantity) ((order_cost*d)/order_quantity)+((annual_per_unit_carry_cost*order_quantity)/2),interval=my_interval,maximum=FALSE)
  Min_total_inventory_cost<-c(Min_total_inventory_cost,new_value$objective)
  Order_quantity_corres<-c(Order_quantity_corres,new_value$minimum)
  annual_no_of_orders<-c(annual_no_of_orders,d/new_value$minimum)
}

#Minimum Total Inventory Costs
Min_total_inventory_cost
hist(Min_total_inventory_cost,main="Histogram of Minimum Total inventory costs")
sample_mean<-mean(Min_total_inventory_cost)
SE<-sd(Min_total_inventory_cost)/sqrt(length(Min_total_inventory_cost))
zscore<-1.96
ME<-zscore*SE
lower<-sample_mean-ME
upper<-sample_mean+ME
paste("95% confidence interval for minimum total cost estimate: (",lower,",",upper,")")
breaks<-seq(from=min(Min_total_inventory_cost),to=max(Min_total_inventory_cost),by=100)
pop<-cut(Min_total_inventory_cost,breaks=breaks,right=FALSE,include.lowest=TRUE)
title<-cbind(table(pop))
colnames(title)<-c("counts")
title
breaks
breaksshift<-breaks[3:12]
prob<-pnorm(breaks[2],mean = mean(Min_total_inventory_cost), sd = sd(Min_total_inventory_cost))
prob<-c(prob,pnorm(breaksshift, mean = mean(Min_total_inventory_cost), sd = sd(Min_total_inventory_cost))-pnorm(breaks[2:11], mean = mean(Min_total_inventory_cost), sd = sd(Min_total_inventory_cost)))
prob<-c(prob,1-pnorm(breaks[12],mean = mean(Min_total_inventory_cost), sd = sd(Min_total_inventory_cost)))
sum(prob)
prob
chisq.test(title,prob)
#Since p value is greater than 0.05
#Therefore we cannot reject null hypothesis
#Hence Minimum total inventory costs comes from a normal distribution


#Order quantity corresponding to minimum total costs
Order_quantity_corres
hist(Order_quantity_corres,main="Histogram of Order quantity corresponding to minimum total costs")
sample_mean<-mean(Order_quantity_corres)
SE<-sd(Order_quantity_corres)/sqrt(length(Order_quantity_corres))
zscore<-1.96
ME<-zscore*SE
lower<-sample_mean-ME
upper<-sample_mean+ME
paste("95% confidence interval for Order quantity corresponding to minimum total costs: (",lower,",",upper,")")
breaks<-seq(from=min(Order_quantity_corres),to=max(Order_quantity_corres),by=10)
pop<-cut(Order_quantity_corres,breaks=breaks,right=FALSE,include.lowest=TRUE)
title<-cbind(table(pop))
colnames(title)<-c("counts")
title
breaks
breaksshift<-breaks[3:6]
prob<-pexp(breaks[2],rate=1/mean(Order_quantity_corres))
prob<-c(prob,pexp(breaksshift,rate=1/mean(Order_quantity_corres))-pexp(breaks[2:5], rate=1/mean(Order_quantity_corres)))
prob<-c(prob,1-pexp(breaks[6],rate=1/mean(Order_quantity_corres)))
sum(prob)
prob
chisq.test(title,prob)
#Since p value is greater than 0.05
#Therefore we cannot reject null hypothesis
#Hence Order quantity corresponding to min total costs comes from a exponential distribution



#Annual number of orders corresponding to minimum total costs
annual_no_of_orders
hist(annual_no_of_orders,main="Histogram of Annual no. of orders for minimum total costs")
sample_mean<-mean(annual_no_of_orders)
SE<-sd(annual_no_of_orders)/sqrt(length(annual_no_of_orders))
zscore<-1.96
ME<-zscore*SE
lower<-sample_mean-ME
upper<-sample_mean+ME
paste("95% confidence interval Annual no. of orders for minimum total costs: (",lower,",",upper,")")
m<-mean(annual_no_of_orders)
s<-sd(annual_no_of_orders)
mlog<-log(m^2/sqrt(s^2+m^2))
slog<-sqrt(log(1+(s^2/m^2)))
breaks<-seq(from=min(annual_no_of_orders),to=max(annual_no_of_orders),by=0.5)
pop<-cut(annual_no_of_orders,breaks=breaks,right=FALSE,include.lowest=TRUE)
title<-cbind(table(pop))
colnames(title)<-c("counts")
title
breaks
breaksshift<-breaks[3:7]
prob<-plnorm(breaks[2],meanlog=mlog,sdlog=slog)
prob<-c(prob,plnorm(breaksshift,meanlog=mlog,sdlog=slog)-plnorm(breaks[2:6],meanlog=mlog,sdlog=slog))
prob<-c(prob,1-plnorm(breaks[7],meanlog=mlog,sdlog=slog))
sum(prob)
prob
chisq.test(title,prob)
#Since p value is greater than 0.05
#Therefore we cannot reject null hypothesis
#Hence Annual no. of orders corresponding to min total costs comes from a lognormal distribution





