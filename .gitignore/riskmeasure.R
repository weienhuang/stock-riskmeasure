Y9999 = fread('~/MMHW6/Y9999.csv', ',')
mydata = Y9999[,c(1, 5, 8)]
colnames(mydata) = c('Date', 'Price', 'Return')
pre_return = c(0, mydata$Return[1:nrow(mydata)-1])
mydata$PreReturn = pre_return
mydata$Price = gsub(',', '', mydata$Price)
mydata$Price = as.double(mydata$Price)
range = 30
## Volatility
getVolatility = function(x){
  result = sd(mydata$Price[x:(x-range-1)])^2
  return(result)
}
volatility = unlist(lapply(c(range+1:nrow(mydata)), FUN = function(x) getVolatility(x)))
## Drawdown
getDrawdown = function(x){
  data = mydata$Price[x:(x-range-1)]
  maxPrice = max(data)
  minPrice = min(data)
  result = (maxPrice - minPrice) / maxPrice
  return(result)
} 
drawdown = unlist(lapply(c(range+1:nrow(mydata)), FUN = function(x) getDrawdown(x)))
## Regression beta
getlmBeta = function(x){
  data = mydata[(x-range+1):x,]
  coef = lm(formula = Return ~ PreReturn, data = data)$coefficients[2]
  return(coef)
}
regressionbeta = unlist(lapply(c(31:nrow(mydata)), FUN = function(x) getlmBeta(x)))

# build new data frame
date = mydata$Date[range+1:nrow(mydata)]
return = mydata$Return[range+1:nrow(mydata)]
newData = data.frame(date, volatility, drawdown, regressionbeta, return)
newData$year = substr(newData$date, 1, 4)

#plot result
library(ggplot2)
library(gtable)
library(grid)
#library(extrafont)
year_1 = '2007'
year_2 = '2008'
one_year_data = newData[newData$year == year_1 | newData$year == year_2,]
p1 = ggplot(data = one_year_data, aes(x = date, y=return, group = 1))+
  scale_x_discrete(breaks = one_year_data$date[seq(1,nrow(one_year_data),7)])+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_line(colour = "dodgerblue2")
p2 = ggplot(data = one_year_data, aes(x = date, y=volatility, group = 1))+
  scale_x_discrete(breaks = one_year_data$date[seq(1,nrow(one_year_data),7)])+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(year_1,'~',year_2, 'Volatility and Return', range, 'days'))+
  geom_line(colour = "firebrick3")
p3 = ggplot(data = one_year_data, aes(x = date, y=drawdown, group = 1))+
  scale_x_discrete(breaks = one_year_data$date[seq(1,nrow(one_year_data),7)])+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(year_1,'~',year_2, 'Drawdown and Return', range, 'days'))+
  geom_line(colour = "green3")
p4 = ggplot(data = one_year_data, aes(x = date, y=regressionbeta, group = 1))+
  scale_x_discrete(breaks = one_year_data$date[seq(1,nrow(one_year_data),7)])+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle(paste(year_1,'~',year_2, 'RegressionBeta and Return', range, 'days'))+
  geom_line(colour = "orange2")

source("~/MMHW6/ggplot_dual_axis.R")
ggplot_dual_axis(p2, p1, "y")
ggplot_dual_axis(p3, p1, "y")
ggplot_dual_axis(p4, p1, "y")
