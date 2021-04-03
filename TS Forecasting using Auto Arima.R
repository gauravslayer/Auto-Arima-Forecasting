#Loading the required packages and dataset

library(readxl)
library(tseries)
t1<-read_excel(file.choose())

#Converting the data into Time Series
ts1<-ts(t1$`Private Health Expenditure`,start = c(1993,1),frequency = 12)

#Plotting the data to determine trend if any:
plot(ts1,main='Time Series Plot',xlab='Year',ylab='Private Health Construction Expenditure',col='Red')

#As you can see, the data has an upward trend. Let us check mathematically if the data is stationary or not. 

adf.test(ts1)

#The data is non-stationary as the null hypothesis of ADF Test has not been rejected.

#To perform ARIMA Modelling, we need the data to be stationary.
#The Auto Arima function would handle non stationarity by differencing the dataset.

#TIME SERIES FORECASTING USING AUTO ARIMA 

#LOADING THE REQUIRED PACKAGES

library(forecast)

#Loading the dataset
tstat<-ts1
data1=tstat

#Plotting the Time series data
plot(data1,main='Time Series Plot',xlab='Year',ylab='Private Health Construction Expenditure',col='blue')

#Developing the Optimal Auto Arima Model

autoarima<-auto.arima(data1)
autoarima

# We have developed are ARIMA Model for forecasting.

#Developing the Forecast for 10 years using Auto Arima

forecast1<-forecast(autoarima,h=120)

# Let us look at the forecasted Values

forecast1

#Plotting the Auto Arima Forecasted Values
plot(forecast1,main = ' ARIMA FORECAST',xlab = 'Year',ylab = 'Private Health Construction Expenditure',col = 'purple')
plot(forecast1$upper,main = ' UPPER ARIMA FORECAST',xlab = 'Year',ylab = 'Private Health Construction Expenditure',col = 'purple')
plot(forecast1$lower,main = '  LOWER ARIMA FORECAST',xlab = 'Year',ylab = 'Private Health Construction Expenditure',col = 'purple')
# Plotting the residuals of the model to see congruence or variance

plot(forecast1$residuals,main='Residuals',xlab='Year',ylab='Residuals',col='orange')

# Sample vs Theoretical Residuals

qqnorm(forecast1$residuals,col='green')

acf(forecast1$residuals)
pacf(forecast1$residuals)

#Measuring the accuracy of our forecast

summary(autoarima)
accuracy(autoarima)


