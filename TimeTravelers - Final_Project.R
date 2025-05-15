#-------------------------------------------------------------------------------
# Metadata
# Title: "Time Traveler (Group 5) Final Project - Demand Forecasting Using Seasonal ARIMA: Model Fit and Forecast Accuracy"
# Author:  Youngjae Cho
# Date:    2025-04-23 ~ 2025-05-01, 2025-05-05 ~ 2025-05-07, 2025-05-08 ~ 2025-05-10
# Purpose: R Markdown file for Final Project
#-------------------------------------------------------------------------------

#Library setting
library(RTseries)
library("knitr")

getwd()
setwd("C:/Users/choyo/Downloads")

## Purpose: 
## In Milestone 1, at the last printed R result, we have seen the seasonality. 
## In this Milestone 2, I will use range-mean plot to check the transformation and iden() function to 
## check the stationarity for 4 differencing schemes. 
## First off, I will load the data set and build the original time series plot that used in Milestone 1. 
miles2 <- read.csv("Daily Demand Forecasting Orders.csv") # Data source. 
miles2 

# For seeing the first few rows to check.
head(miles2)

# To check the column for variables.
colnames(miles2)

# To generate a sequential time index
miles2$time_index <- seq(from=1, to = nrow(miles2), by =1)
miles2$time_index

# Select the total orders variables.
miles2_ts <- ts(miles2$Target..Total.orders., frequency = 5) # Weekly frequency for 5 business days.
miles2_ts # Start from 1 to 60. 

# Time Series Plot.
plot(miles2_ts, type="l", col="salmon", lwd=2,
     main = "Time Series Plot - Realization vs Time",
     xlab = "Day", ylab = "Total Orders")


## In this Milestone2, the time series plot between Realization and Time, we can see the seasonality.
## Therefore, I will use s = 5 for five business days, that is weekly cycles, Monday to Friday. 
## I will use acf, pacf plots after log-transformation. 

# Base functions for ACF & PACF comparison
par(mfrow=c(2,2))

# ACF & PACF plots for original time series data.
acf(miles2_ts, main="ACF: Original Data")
pacf(miles2_ts, main="PACF: Original Data")

# Log-transformed data
log_miles2_ts <- log(miles2_ts)

# ACF & PACF plots of log-transformed data
acf(log(miles2_ts), main="ACF: Log-Transformed Data")
pacf(log(miles2_ts), main="PACF: Log-Transformed Data") # No need for transformation. 

par(mfrow=c(1,1)) 


## Then, I will use iden() function to check the stationary. 
# Run the Seasonal identification with two different differencing schemes.
miles2_tsd <- tsd(miles2_ts,
                  data.title = "Daily Demand Forecasting Orders",
                  response.units = "Orders",
                  time.units = "Day")
miles2_tsd

# iden function for only two differencing in seasonal time series. 
iden(miles2_tsd, d = 0, D = 1) # Seasonal only.
iden(miles2_tsd, d = 1, D = 1) # First order and seasonal but it would be over-differencing. 


#4. Library setting

library(forecast)
library(TSA)
library(tseries)

#5. Do ADF Test for getting Stationary and fit four candidate SARIMA Models. (5/5)

# data 
data <- read.csv("Daily Demand Forecasting Orders.csv")
ts_data <- ts(data$Target..Total.orders., frequency = 5) # For weekly seasonality.

print(data)
print(ts_data)

# ADF Tests for two differenced series
cat("ADF Test for Seasonal Differencing Only (d=0, D=1):\n") # No - Non-Stationary. (High p-value)
adf.test(diff(ts_data, lag = 5), alternative = "stationary")

cat("\nADF Test for First and Seasonal Differencing (d=1, D=1):\n") # Yes - Stationary Confirm! (small p-value)
adf.test(diff(diff(ts_data), lag = 5), alternative = "stationary")

# Fit four candidate SARIMA Models. 
fit1 <- Arima(ts_data, order =c(0,1,1), seasonal= list(order =c(0,0,1), period= 5))
fit2 <- Arima(ts_data, order =c(1,1,1), seasonal= list(order =c(0,1,1), period= 5))
fit3 <- Arima(ts_data, order =c(0,1,2), seasonal= list(order =c(1,0,1), period = 5))
fit4 <- Arima(ts_data, order =c(0,0,2), seasonal= list(order =c(1,0,1), period= 5))

print(fit1)
print(fit2)
print(fit3)
print(fit4)


#6. Compare AIC and BIC. 

cat("Model 1 (0,1,1)(0,0,1)[5]:\n", "AIC:",AIC(fit1), "BIC:", BIC(fit1),"\n")
cat("Model 2 (1,1,1)(0,1,1)[5]:\n", "AIC:",AIC(fit2), "BIC:", BIC(fit2),"\n")
cat("Model 3 (0,1,2)(1,0,1)[5]:\n", "AIC:",AIC(fit3), "BIC:", BIC(fit3),"\n")
cat("Model 4 (0,0,2)(1,0,1)[5]:\n", "AIC:",AIC(fit4), "BIC:", BIC(fit4),"\n")


#7. Residual diagnostics for fit4 (SARIMA(0,0,2)(1,0,1)[5]) AND Fit2 (SARIMA(1,1,1)(0,1,1)[5])

# Model 4, SARIMA(0,0,2)(1,0,1)[5] model for Residual Diagnostics.
# par(mfrow = c(2, 1))
# acf(residuals(fit4), main = "ACF of Residuals")
# pacf(residuals(fit4), main = "PACF of Residuals")
# par(mfrow = c(1, 1))

# Box.test(residuals(fit4), lag = 10, type = "Ljung-Box", fitdf = 3)  # df = 3.
# qqnorm(residuals(fit4)); qqline(residuals(fit4), col = "blue")

# Best Model: Model 2, SARIMA(1,1,1)(0,1,1)[5] model for Residual Diagnostics.
par(mfrow = c(2, 1))
acf(residuals(fit2), main = "ACF of Residuals(fit2)")
pacf(residuals(fit2), main = "PACF of Residuals(fit2)")
par(mfrow = c(1, 1))

Box.test(residuals(fit2), lag = 10, type = "Ljung-Box", fitdf = 3)
qqnorm(residuals(fit2)); qqline(residuals(fit2), col = "red")


#8. Forecasting with Model 2 (SARIMA(1,1,1)(0,1,1)[5])

# Forecast for the next 5 business days. (Short-term)
forecast_result_fit2 <- forecast(fit2, h = 5, level = c(80, 95))
plot(forecast_result_fit2, main = "Forecast: SARIMA(1,1,1)(0,1,1)[5] - 5 days ahead", xlab = "Day", ylab = "Forecasted Orders")
print(forecast_result_fit2)

# Forecast for the next 10 days. 
forecast_result_fit2 <- forecast(fit2, h = 10, level = c(80, 95))
plot(forecast_result_fit2, main = "Forecast: SARIMA(1,1,1)(0,1,1)[5] - 10 days ahead", xlab = "Day", ylab = "Forecasted Orders")
print(forecast_result_fit2)

# Forecast for the next 20 days.
forecast_result_fit2 <- forecast(fit2, h = 20, level = c(80, 95))
plot(forecast_result_fit2, main = "Forecast: SARIMA(1,1,1)(0,1,1)[5] - 20 days ahead", xlab = "Day", ylab = "Forecasted Orders")
print(forecast_result_fit2)

# Forecast for the next 30 days. (1 month)
forecast_result_fit2 <- forecast(fit2, h = 30, level = c(80, 95))
plot(forecast_result_fit2, main = "Forecast: SARIMA(1,1,1)(0,1,1)[5] - 1 month ahead", xlab = "Day", ylab = "Forecasted Orders")
print(forecast_result_fit2)

# Forecast for the next 60 days. (2 month)
forecast_result_fit2 <- forecast(fit2, h = 60, level = c(80, 95))
plot(forecast_result_fit2, main = "Forecast: SARIMA(1,1,1)(0,1,1)[5] - 2 month ahead", xlab = "Day", ylab = "Forecasted Orders")
print(forecast_result_fit2)

# Forecast for the next 90 days. (3 month)
forecast_result_fit2 <- forecast(fit2, h = 90, level = c(80, 95))
plot(forecast_result_fit2, main = "Forecast: SARIMA(1,1,1)(0,1,1)[5] - 3 month ahead", xlab = "Day", ylab = "Forecasted Orders")
print(forecast_result_fit2)

# Forecast for the next 120 days. (4 month)
forecast_result_fit2 <- forecast(fit2, h = 120, level = c(80, 95))
plot(forecast_result_fit2, main = "Forecast: SARIMA(1,1,1)(0,1,1)[5] - 4 month ahead", xlab = "Day", ylab = "Forecasted Orders")
print(forecast_result_fit2)

# Forecast for the next 150 days. (5 month)
forecast_result_fit2 <- forecast(fit2, h = 150, level = c(80, 95))
plot(forecast_result_fit2, main = "Forecast: SARIMA(1,1,1)(0,1,1)[5] - 5 month ahead", xlab = "Day", ylab = "Forecasted Orders")
print(forecast_result_fit2)

# Forecast for the next 180 business days. (6 months)
forecast_result_fit2 <- forecast(fit2, h = 180, level = c(80, 95))
plot(forecast_result_fit2, main = "Forecast: SARIMA(1,1,1)(0,1,1)[5] - 6 month ahead", xlab = "Day", ylab = "Forecasted Orders")
print(forecast_result_fit2)

# Forecast for the next 365 days. (1 year)
forecast_result_fit2 <- forecast(fit2, h = 365, level = c(80, 95))
plot(forecast_result_fit2, main = "Forecast: SARIMA(1,1,1)(0,1,1)[5] - 1 year ahead", xlab = "Day", ylab = "Forecasted Orders")
print(forecast_result_fit2)


#9. External Variables for improving process.

# Data set setting.
ts_data <- ts(miles2$Target..Total.orders., frequency = 5) # Original data.

# Add two external objects, urgent and non-urgent orders.
nonurgent_ts <- miles2$Non.urgent.order
urgent_ts <- miles2$Urgent.order
xreg_vars <- cbind(urgent_ts, nonurgent_ts) # Design matrix for two external variables. 

print(nonurgent_ts)
print(urgent_ts)
print(xreg_vars)

# Fit the SARIMA Model2 with external variables - dynamic regression.
fit_regression_sarima2 <- Arima(ts_data,
                                order = c(1,1,1),
                                seasonal = list(order = c(0,1,1), period = 5),
                                xreg = xreg_vars)
fit_regression_sarima2

summary(fit_regression_sarima2) # lower AIC, BIC than original SARIMA Model.

# Check process for residual diagnostics.
par(mfrow = c(2,1))
acf(residuals(fit_regression_sarima2), main = "ACF of Residuals (Regression SARIMA 2)") # White Noise again.
pacf(residuals(fit_regression_sarima2), main = "PACF of Residuals (Regression SARIMA 2)") # Similar with ACF - For two plots, there is no meaningful autocorrelation in 
# the residuals.
par(mfrow = c(1,1))

Box.test(residuals(fit_regression_sarima2), lag = 10, type = "Ljung-Box") # Higher P-value than significance level - good.

# Normal Q-Q Plot.
qqnorm(residuals(fit_regression_sarima2)); qqline(residuals(fit_regression_sarima2), col = "black")

# Forecast with external variables for the next 60 days.
future_nonurgent <- rep(mean(nonurgent_ts), 60)
future_urgent <- rep(mean(urgent_ts), 60)
future_xreg <- cbind(future_urgent, future_nonurgent)

forecast_regression_sarima2 <- forecast(fit_regression_sarima2,
                                        h = 60,
                                        xreg = future_xreg,
                                        level = c(80,95))
# Visualization.
plot(forecast_regression_sarima2, main = "Regression SARIMA Model2 Forecast for the next 60 Days",
     xlab = "Day", ylab = "Forecasted Total Orders")