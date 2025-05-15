# =============================================================
# Metadata
# Name: Youngjae Cho
# Date: 2025-04-16
# Purpose: Time Series Analysis for Google Stock Price in 200 days with using fpp2 package.
# Forecast the predicted value 
# =============================================================

# --------------------------
# Library setting.
library(fpp2)
# --------------------------

head(goog200) # Head of the data set "goog200."

plot(goog200, col="cornflowerblue", lwd=2,
     xlab="Day", ylab="Dollars",
     main="Google Stock Prices") # Graph of time series data.

# First off, we can see that there was a trend, this means that the time series for
# google stock prices is non-stationary. 

# Stationary Check to use acf test.
library(forecast)
acf(goog200, main="Google Stock Prices") # From lag0 to lag 20, autocorrelation steadily drops. 
                                         # Autocorrelation between them is large. 
                                         # 95% Confidence interval about the autocorrelation 0,
                                         # almost autocorrelations escape from the 95% C.Is.
                                         # Therefore, we can say that the data from time series
                                         # plot of Google Stock Price is non-stationary.
# Use ndiff function for number of appropriate differencing.
# diff function for building differenced time series. 
ndiffs(goog200) # d = 1
dgoog200 <- diff(goog200)
head(dgoog200)  # Second observation - First observaton = -0.317932
                # Third observation - Second observation = 4.793823

# Building a differenced time series plot. 
plot(dgoog200, col="salmon", lwd=2,
     xlab="Day", ylab="Dollars",
     main="Google Stock Prices\nTransformed by Differencing") 

# Building a acf plot from a differenced time series plot. 
acf(dgoog200, main="Google Stock Prices\nTransformed by Differencing") # Stationary confirm! Also, after lag0, data suddenly dropped.
                                                                       # From 95% CIs, autocorrelations were rely on zero. That means stationary.
                                                                       # Differenced time series data was stationary between other observations.
# Compute the ADF hypothesis test to check the stationary for testing the null (data mu = non-stationary)
# If the p-value is smaller than the significance level, the data from time series is stationary. 
library(tseries)
adf.test(goog200) # Original time series data is non-stationary. 

# Use differenced time series data for adf testing.
adf.test(dgoog200) # The data from differencing is statistically significant in the significance level.
                   # Therefore, the data from differencing is stationary. 

# Steps for stational time series transformation.
# 1. If the fluctuation range is not certain, we should use log-transformation to change data to constant variance by time flow.
# 2. If there were a trend or seasonality, differencing would be the best method for constant mean to the certain stational time series.
# 3. If the fluctuation range is not constant, and a trend and seasonality were observed, we can use the
#    log-transformation and differencing process to do change original data to stational time series. 




