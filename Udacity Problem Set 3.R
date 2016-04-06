#load ggplot 2 library - diamonds data set is part of this
library(ggplot2)
#load dplyr
library(dplyr)

#basic observations of diamonds
str(diamonds)
#number of observations: 53,940
#number of variables: 10
#number of ordered factors: 3
#best color: D

#create histogram of all diamonds in data set
qplot(x = price, data = diamonds, binwidth = 100, color = I("black"), fill = I("orange"))
ggsave("diamonds1-01.png")

#describe shape and centre of distribution
#right skewed distibution
#check summary to find mean, median, etc.
summary(diamonds$price)

#no of diamonds cost under $500
sum(diamonds$price < 500)
#no of diamonds cost under $250
sum(diamonds$price < 250)
#no of diamonds cost $15,000 or more
sum(diamonds$price >= 15000)

#alter price histogram
qplot(x = price, data = diamonds, binwidth = 250, color = I("black"), fill = I("gold")) +
  scale_x_continuous(limits = c(0, 18823), breaks = seq(0, 18823, 1000))
ggsave("diamonds1-02.png")

#cut histograms
qplot(x = price, data = diamonds, binwidth = 250, color = I("black"), fill = I("yellow")) +
  facet_wrap(~cut)
ggsave("diamonds1-03.png")

#cut summaries
summary(diamonds$cut == "Fair")
#highest price diamond: premium
filter(diamonds, price == max(diamonds$price))
#lowest price diamond: premium, ideal
filter(diamonds, price == min(diamonds$price))
#lowest median price: ideal
table(diamonds$cut)
  by(diamonds$price, diamonds$cut, summary)
  
#change histogram so y axis isn't fixed
qplot(x = price, data = diamonds, binwidth = 250, color = I("black"), fill = I("green")) +
  facet_wrap(~cut, scales = "free_y")
ggsave("diamonds1-04.png")

#price per carat
qplot(x = log10(price/carat + 1), data = diamonds, binwidth = 0.05, color = I("black"), fill = I("blue")) +
  facet_wrap(~cut, scales = "free_y")
ggsave("diamonds1-05.png")
  
#investigate price of diamonds using boxplots and numerical summaries
#compare price vs. color
qplot(x = color, y = price, data = diamonds, geom = "boxplot")
ggsave("diamonds1-06.png")

table(diamonds$color)
  by(diamonds$price, diamonds$color, summary)

#find IQR for color D and J - can also find from color summaries
IQR(diamonds$price[diamonds$color == "D"])
IQR(diamonds$price[diamonds$color == "J"])

#price per carat across different colors using a boxplot
qplot(x = color, y = price/carat, data = diamonds, geom = "boxplot")
ggsave("diamonds1-07.png")

#weight of diamonds using freq polygon
qplot(x = carat, data = subset(diamonds, !is.na(carat)), geom = "freqpoly", binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0, 5, 0.1))
ggsave("diamonds1-08.png")

#carat size with count over 2000: 0.3, 1.01

#gapminder data set, create 2-5 plots, describe what is seen
#use data set on Corruption perception Index
cpi <- read.table("cpi.csv" , header = TRUE, sep = ",", quote = "\"", na.strings = "")
#plotting CPI index of 2008 vs 2009: high linear correlation of scores fromk 1 year to next
#indicates one year change is small
qplot(x = X2008, y = X2009, data = subset(cpi, !is.na(X2009)), 
      color = I("black"), fill = I("red"))
ggsave("diamonds1-09.png")
#plotting freq polygon, 2008 and 2009, most common score increased from 2 in 2008 to 2.5 in 2009
#indicates CPI score may be rising
qplot(x = X2008, data = subset(cpi, !is.na(X2008)), geom = "freqpoly", binwidth = .5) +
  scale_x_continuous(breaks = seq(0, 10, 1))
ggsave("diamonds1-10.png")
qplot(x = X2009, data = subset(cpi, !is.na(X2009)), geom = "freqpoly", binwidth = .5) +
  scale_x_continuous(breaks = seq(0, 10, 1))
ggsave("diamonds1-11.png")
#plotting box plots for 2008 and 2009, we see very little change, 
#but IQR looks to have dropped slightly from 2008 to 2009
qplot(1, X2008, data = subset(cpi, !is.na(X2008)), geom = "boxplot", 
      xlab = "2008", ylab = "CPI Score") +
  scale_x_continuous(breaks = seq(0, 1, 1))
ggsave("diamonds1-12.png")
qplot(1, X2009, data = subset(cpi, !is.na(X2009)), geom = "boxplot", 
      xlab = "2009", ylab = "CPI Score") +
  scale_x_continuous(breaks = seq(0, 1, 1))
ggsave("diamonds1-13.png")

#ggsave doesn't work with grid.arrange!

#facebook friends birthday analysis - skipped final step. 
#This was a 2 hour exercise and have spent significantly more than 2 hours so far

  