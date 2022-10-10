library(Ecdat)
head(Cigarette)
View(Cigarette)
library(ggplot2)
library(dplyr)

##########boxplot of the average number of packs per capita by state

boxplot_pack <- ggplot(Cigarette, aes(x=state, y=packpc)) + geom_boxplot() + 
  xlab("State") + ylab("Average Number Of Packs") + 
  ggtitle("Boxplot Of The Average Number Of Packs Per Capita By State")

#I can't decide clearly from this plot wich state have the highest average 
#number of packs so to be more accurate I used also this code:

packpc_state <- Cigarette %>% group_by(state) %>%
  summarise(mean_pack = mean(packpc)) %>% arrange(desc(mean_pack))
View(packpc_state)

#after running this code KY have the highest average number of packs 

packpc_state1 <- Cigarette %>% group_by(state) %>%
  summarise(mean_pack = mean(packpc)) %>% arrange((mean_pack))
View(packpc_state1)

#after running this code UT have the lowest average number of packs

#############################################################

#the median over all the states of the number of packs per capita for each year

unique(Cigarette$year)

med_pack <- Cigarette %>% group_by(year) %>% summarise(medPack = median(packpc))

ggplot(med_pack, aes(x=year, y=medPack)) + geom_point() +
  ylab("Median Of Numb Of PackPC")+ ggtitle("Median Of Numb Of PackPC For Each Year")

#we can notice obviously that the consumption of cigarettes have decreased 
#between 1985 and 1995

##############################################################

#a scatter plot of price per pack vs number of packs per capita for all states and years.

ggplot(Cigarette, aes(x=avgprs, y=packpc)) + geom_point() + 
  xlab("Average price per pack") + ylab("Average numbers of packs") + 
  ggtitle("Scatter Plot Of Price Per Pack VS Numbers Of Packs") + 
  geom_smooth(method=lm, se=FALSE)

#From the scatter plot we can deduce that the price and the per capita packs are
#negatively correlated. this is what we should expect because when the the 
#price of a product increase the consumption must decrease.

#calculating correlation:
cor.test(Cigarette$avgprs, Cigarette$packpc, method = "pearson", use= "complete.obs")

#this is a significant correlation, p-value less than 0.05 and very small, r=-0.58
#that's mean there is a moderate correlation and they are negatively correlated

#################################################################


#scatter plot to show the points for each year in a different color
ggplot(Cigarette, aes(x=avgprs, y=packpc, color = year)) + geom_point() + 
  xlab("Average price per pack") + ylab("Average numbers of packs") + 
  ggtitle("Scatter Plot Of Price Per Pack VS Numbers Of Packs") + 
  geom_smooth(method=lm, se=FALSE, colour = "red")

#From this scatter plot with the addition of years, the relationship between
#the two variable doesn't change! their still negatively correlated also we can
#assume again that the consumption of cigarettes decrease between 1985 and 1995.

#####################################################################

#Do a linear regression for these two variables. How much variability does the
#line explain?

#calculating linear regression
cigarette_regression <- lm(packpc~avgprs, Cigarette)
cigarette_regression
summary(cigarette_regression)

ggplot(Cigarette, aes(x=avgprs, y=packpc, col= packpc)) + geom_point() +
  geom_abline(aes(intercept= 167.87, slope = -0.40879))

#the line explain only 34.15% of the variability of the data; the the Adjusted 
#R-squared value is 0.3415,  this means that the average price is able to 
#explain about 34% of the variance of the average number of packs per capita .

################################################################

#price adjust
price_adjust <- Cigarette$avgprs / Cigarette$cpi
adj_cigarette <- cbind(Cigarette, price_adjust)
View(adj_cigarette)

adjust_plot <- ggplot(adj_cigarette, aes(x=price_adjust, y=packpc, color = year)) + geom_point() + 
  xlab("Average price per pack") + ylab("Average numbers of packs") + 
  ggtitle("Scatter Plot Of Price Per Pack VS Numbers Of Packs") + 
  geom_smooth(method=lm, se=FALSE, colour = "red")

#conclusion: also these two variables are negatively correlated

#calculating correlation:
cor.test(adj_cigarette$price_adjust, adj_cigarette$packpc, method = "pearson", use= "complete.obs")


#linear regression of price adjustment and packpc
cigarette_regression2 <- lm(packpc~price_adjust, adj_cigarette)
cigarette_regression2
summary(cigarette_regression2)

ggplot(adj_cigarette, aes(x=price_adjust, y=packpc, col= packpc)) + geom_point() +
  geom_abline(aes(intercept= 211.76821, slope = -0.9164))
#after the price adjustment, the line explain 37.57% of the variability of the data; the the Adjusted 
#R-squared value is 0.3757,  this means that the average price is able to 
#explain about 37% of the variance of the average number of packs per capita, 
#3% more than for the old price.

#############################################################

#dataframe 1985 and 1995 
Cigarette_85 <- filter(Cigarette, year == 1985)
View(Cigarette_85)
Cigarette_95 <- filter(Cigarette, year == 1995)
View(Cigarette_95)

# Dependent t-test
t_dep <- t.test(Cigarette_85$packpc, Cigarette_95$packpc, paired = TRUE)
t_dep
#conclusion: the number of packs per capita in 1995 was significantly different
#than the number of packs per capita in 1985;true difference in means!!

#######################################################


#QUESTION2:
#are the personal income and the average number of packs correlated?
ggplot(Cigarette, aes(x=income, y=packpc)) + geom_point() + 
  xlab("Personal income") + ylab("Average numbers of packs") + 
  ggtitle("Scatter Plot Of Personal Income VS Numbers Of Packs") + 
  geom_smooth(method=lm, se=FALSE, colour = "red")

##calculating correlation:
cor.test(Cigarette$income, Cigarette$packpc, method = "pearson", use= "complete.obs")

#conclusion: we can deduce from the plot and from the cor.test that the two variables
#income and packpc are uncorrelated or they have a weak negative correlation.

#QUESTION3:
#Are the population and the average number of packs correlated?
ggplot(Cigarette, aes(x=pop, y=packpc, color=state)) + geom_point() + 
  xlab("Population") + ylab("Average numbers of packs") + 
  ggtitle("Scatter Plot Of Population VS Numbers Of Packs") + 
  geom_smooth(method=lm, se=FALSE, colour = "red")

##calculating correlation:
cor.test(Cigarette$pop, Cigarette$packpc, method = "pearson", use= "complete.obs")
#conclusion: we can deduce from the plot and from the cor.test that the two variables
#pop and packpc are uncorrelated or they have a weak negative correlation

# let's see if the population affect the packpc in a specific state (CA) between
#1985 and 1995
Cigarette_CA <- filter(Cigarette, state == "CA")
View(Cigarette_CA)

ggplot(Cigarette_CA, aes(x=pop, y=packpc, color=year)) + geom_point() + 
  xlab("Population") + ylab("Average numbers of packs") + 
  ggtitle("Scatter Plot Of Population VS Numbers Of Packs") + 
  geom_smooth(method=lm, se=FALSE, colour = "red")

cor.test(Cigarette_CA$pop, Cigarette_CA$packpc, method = "pearson", use= "complete.obs")

#we can see clearly that these two variables are strongly negative correlated.
#But this is not what i expect!! since the population increased in CA the
#consumption of cigarette should be bigger! But wait, we have see above while 
#exploring the affect of years on packpc, that the consumption of cigarette decrease
#between 1985 and 1995, so there is another factor here affecting the decreasing
#of packcs and it's the time because the population increase through years in CA.


