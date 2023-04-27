##########################################################
## Basic Time Series Analysis
##########################################################

# Time-series library
library("TSA")
library("tseries")

# simulated data
data("ar2.s")
data("ma1.1.s")
z = ts(rnorm(120), frequency=1)

# check data
ar2.s

# plot time series data
plot(ar2.s)
plot(ma1.1.s)
plot(z)

# change frequency
z1 = ts(z, frequency=10) # even if frequency was changed, index is unchanged
plot(z1)

# check for autocorrelation
# default is lag = 1
Box.test(ar2.s, type="Ljung")
Box.test(ma1.1.s, type="Ljung")
Box.test(z, type="Ljung", lag=3)

# compute for ACF
acf(ar2.s)
acf(ma1.1.s)
acf(z)

# compute for PACF
acf(ar2.s, type="partial")
acf(ma1.1.s, type="partial")
acf(z, type="partial")

# test for stationarity
adf.test(ar2.s)
adf.test(ma1.1.s)
adf.test(z)

# fit ARMA models
# order = AR lag, integration, MA lag 
arima(ar2.s, order=c(1,0,0))
arima(ar2.s, order=c(2,0,0))
arima(ar2.s, order=c(9,0,0))

arima(ma1.1.s, order=c(0,0,1))
arima(ma1.1.s, order=c(0,0,5))
arima(ma1.1.s, order=c(0,0,14))

##########################################################
## Modelling with Data
##########################################################

# Quarterly S&P Composite Index, 1936Q1 - 1977Q4.
data("SP")

# test for stationarity
adf.test(SP)

# perform pre-processing
SP.ln = log(SP) # take log transformation
SP.ln.d = diff(SP.ln) # take first difference of log
plot(SP.d)

# perform stationarity test for differenced data
adf.test(SP.ln.d)

# check autocorrelation
Box.test(SP.ln.d)

# Daily USD/HKD exchange rate from January 1, 2005 to March 7, 2006
data("usd.hkd")
forex = ts(usd.hkd$hkrate)

# perform initial tests
adf.test(forex)
Box.test(forex)

# check ACF, PACF
acf(forex)
acf(forex, type="partial")

# check models, lower AIC is better
arima(forex, order=c(0,0,1))
arima(forex, order=c(2,0,0))
arima(forex, order=c(2,0,1))