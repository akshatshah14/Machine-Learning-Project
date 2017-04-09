ICNSA <- read_csv("C:/Users/akshat/Downloads/ICNSA.csv")
View(ICNSA)
#Part 1
#Plot and Inference
#1)	Show a time series plot. 
str(ICNSA)
jclaim <- ICNSA$Jobless_Claims
jclaim_ts <- ts(jclaim,start=c(2012,3),frequency = 12)
plot(jclaim_ts)


#2)	Please summaries your observations of the times series plot
#Trend : a general direction in which something is developing or changing.
#We see that there is a decreasing trend in the data 

#seasonality: Seasonality is a characteristic of a time series in which the data experiences regular 
#and predictable changes that recur every calendar year.Any predictable change or pattern in a 
#time series that recurs or repeats over a one-year period can be said to be seasonal.
#we see that there is seasonality present as we see that at the start of every year there is an increase
#in the job claims.


#Part 2
#Central Tendency
#1)	What are the min, max, mean, median, 1st and 3rd Quartile values of the times series? 
summary(jclaim_ts)
#min - 203800, 
#max - 203800,
#mean - 307800, 
#median - 307600
#1st Quartile - 254500
#3rd Quartile - 341400

#2)	Show the box plot. 
boxplot(jclaim_ts)


#3)Can you summarize your observation about the time series from the summary stats and box plot? 
# We see that the minimum no of jobs claims is 203800 and the max is 480600 in the span of 6 years.
#THe mean being close to 307800. Also we see that the values of 1st and 3rd quartile to be 254500 and 341400 respectively.
#we see that the maximum no of jobless claims is an outlier from the boxplot and it is further away from the rest of the data.
#It therefore acts as a outlier.

#Part 3
#Decomposition

#1)	Plot the decomposition of the time series.
x=decompose(jclaim_ts)
plot(x)
#2)	Is the times series seasonal?
x$seasonal
#yes the time series is seasonal as we see there is increase in job claims after a certain period of time.

#3) Is the decomposition additive or multiplicative? 
#The decomposition is additive.

#4)	If seasonal, what are the values of the seasonal monthly indices? 
x$figure

#5)	For which month is the value of time series high and for which month is it low? 
#Septemer has the lowest and December has the highest.

#6)Can you think of the reason behind the value being high in those months and low in those months?
# no the data does not tell us or state any reason *


#7)	Show the plot for time series adjusted for seasonality. 
#Overlay this with the line for actual time series? 
#Does seasonality have big fluctuations to the value of time series? 

adjust_jc = jclaim_ts - x$seasonal
plot(adjust_jc)
lines(jclaim_ts,col='red')

#The seasonality has a very high impact on the data as we can see the actual line
#which is in red colour has bigger peaks and very high fluctuations.

#Part 4
#Naïve Method
#1)	Output
#install.packages('TTR')
require(TTR)
require(FPP)
#install.packages('forecast')
#install.packages('FPP')
require(forecast)
naive_method = naive(jclaim_ts,2)
plot(naive_method)

#2)	Perform Residual Analysis for this technique. 
#a)	Do a plot of residuals. What does the plot indicate?
plot(naive_method$residuals)
#we dont see any pattern in the residuals.

#b)	Do a Histogram plot of residuals. What does the plot indicate?
hist(naive_method$residuals)

#c)	Do a plot of fitted values vs. residuals. What does the plot indicate? 

o	Do a plot of actual values vs. residuals. What does the plot indicate?
o	Do an ACF plot of the residuals? What does this plot indicate?
.	Print the 5 measures of accuracy for this forecasting technique
.	Forecast 
o	Time series value for next year. Show table and plot
.	Summarize this forecasting technique
o	How good is the accuracy?
o	What does it predict the value of time series will be in one year?
o	Other observation
