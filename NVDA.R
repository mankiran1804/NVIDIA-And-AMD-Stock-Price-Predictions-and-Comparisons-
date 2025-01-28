#NVDA CODE

# Library
library(fBasics)
library(tseries)
library(astsa)
library(lmtest)
library(TSA)
library(forecast)
library(tsoutliers)

#NVDA
set.seed(99)
nvda = read.csv("NVDA.csv", header = T)
head(nvda)
summary(nvda)

total_rows <- nrow(nvda)  
total_rows
#Data frame for all rows other than last 10 rows
nvda_part1 <- nvda[1:(total_rows - 10), ]
nvda_part1
total_rows1 <- nrow(nvda_part1) 
total_rows1
# Data frame for the last 10 rows
nvda_part2 <- nvda[(total_rows - 9):total_rows, ]
total_rows2 <- nrow(nvda_part2) 
total_rows2
x=nvda_part1[,6] 
length(x)

#BEFORE LOG TRANSFORMATION
date1 = as.Date(nvda_part1$Date, format = "%m/%d/%Y")
plot(date1, x,  xlab = "Time", ylab = "Adjusted Close",type = 'l', main = 'NVDIA Daily Stock price from 2014 Jan to 2024 March')
acf(x, main = 'ACF for data')
pacf(x, main = 'PACF for data')
adf.test(x)#still not stationary

#LOG TRANSFORMATION
logx = log(x)
length(logx)
plot(date1,logx, xlab = "Time", ylab = "Adjusted Close", main = "Time Series Plot of NVIDIA Adjusted Close Prices", type = "l")
acf(logx, main = 'ACF for log data')
pacf(logx, main = 'PACF for log data')
adf.test(logx)#still not stationary


#DIFFERNCE
dlogx = diff(logx)
par(mfrow =c(1, 3))
plot(date1[-1],dlogx, xlab = "Time", ylab = "Adjusted Close", main = "Time Series Plot of NVIDIA Adjusted Close Prices", type = "l")
acf(dlogx, main = 'ACF for diff data')
pacf(dlogx,main = 'PACF for diff data')
adf.test(dlogx)#Data is stationary


#SEASONALITY
logx_seas <- mstl(logx)
plot(logx_seas)



#MEAN RETURN ANALYSIS
return_NVDA = dlogx
return_NVDA
mean(return_NVDA)
#Sd
sd(return_NVDA)
#skewness+histogram
hist(return_NVDA)
skewness(return_NVDA)


#MODEL BUILDING AND SELECTION
acf(dlogx, main = 'ACF for diff data')
pacf(dlogx,main = 'PACF for diff data')
library(TSA)
eacf(dlogx)


#models
AR8_MODEL= arima(dlogx, order = c(8,0,0)) #taking order of d as 0 as we are using the difference data from previous steps
AR8_MODEL
coeftest(AR8_MODEL)

AR8_MODEL_sig= arima(dlogx, order = c(8,0,0), fixed = c(NA,0,0,0,0,0,NA,NA,NA))#king order of d as 0 as we are using the difference data from previous steps
AR8_MODEL_sig
coeftest(AR8_MODEL_sig)

AR8_MODEL$aic
AR8_MODEL_sig$aic

#test for stationary and redundancy 
polyroot(c(1, -AR8_MODEL_sig$coef[1:8]))
abs(polyroot(c(1, -AR8_MODEL_sig$coef[1:8])))
#Their absolute values are all greater than 1, which indicates that the corresponding AR process is stationary.


#MA8
MA8_MODEL= arima(dlogx, order = c(0,0,8)) #taking order of d as 0 as we are using the difference data from previous steps
MA8_MODEL
coeftest(MA8_MODEL)

MA8_MODEL_sig= arima(dlogx, order = c(0,0,8), fixed = c(NA,0,0,0,0,0,NA,NA,NA)) #taking order of d as 0 as we are using the difference data from previous steps
MA8_MODEL_sig
coeftest(MA8_MODEL_sig)

MA8_MODEL$aic
MA8_MODEL_sig$aic  #Choosing new models as lower AIC values


#COMPARSION TO CHOOSE THE BEST MODEL

#AIC
AR8_MODEL_sig$aic
MA8_MODEL_sig$aic
warnings() 

#ROLLING FORECASTING
source("rolling.forecast.R")
rolling.forecast(dlogx, 1, length(x)-50, order = c(8,0,0),fixed = c(NA,0,0,0,0,0,NA,NA,NA))
rolling.forecast(dlogx, 1, length(x)-50 ,c(0,0,8),fixed = c(NA,0,0,0,0,0,NA,NA,NA))


#choosing AR(8) as less Rolling forecasting and Lower AIC Value


#RESIDUAL ANALYSIS OF THE CHOOSEN MODEL
par(mfrow =c(1, 3))
plot(AR8_MODEL_sig$residuals)
acf(AR8_MODEL_sig$residuals)
pacf(AR8_MODEL_sig$residuals)
Box.test(AR8_MODEL_sig$residuals, lag = 12, type = 'Ljung')
tsdiag(AR8_MODEL_sig)


#PREDICTION
n=length(logx)
len_logx = length(logx)
len_logx
predictions <- numeric(10)
predup <- numeric(10)
predlow <- numeric(10)
xreg = 1:n

for (i in 1:10) {
  out4 <- arima(logx, order = c(8,1,0),fixed = c(NA,0,0,0,0,0,NA,NA,NA), xreg = xreg)
  out4
  coeftest(out4)
  h <- 1  
  newxreg <- (n + 1):(n + h)
  pp <- predict(out4, n.ahead = h, newxreg = newxreg)
  pred <- exp(pp$pred)
  pred.upp = exp(pp$pred+2*pp$se)
  pred.low = exp(pp$pred-2*pp$se)
  logx <- c(logx, log(pred))
  xreg <- c(xreg, newxreg)  
  predictions[i] <- pred
  predup[i] <- pred.upp 
  predlow[i] <- pred.low
  
}
#Predictions
predictions
predup 
predlow
nn = len_logx	#length of your data
nn
nt = 10	#forecast horizon
nb = 50	#number of data points you want to plot
tt = (nn-nb):nn	#indexes of data points you want to plot
tt
xxx = nvda_part1$Adj.Close[tt]		#data you want to plot
xxx

rr = range(c(xxx, predictions, predup, predlow))	#find the minimum and maximum y values in your plot
par(mfrow = c(1, 1))

plot(tt, xxx, pch=2, xlim=c(nn-nb,nn+nt), ylim=rr, main='Prediction', ylab='Return', xlab='Time')	
lines(tt, xxx)	#observed values
points(nn+1:nt, predictions, pch=2, col='red', type='o')	#predicted values
lines(nn+1:nt, predup, lty=2, col='red')	#upper bound of predicted interval
lines(nn+1:nt, predlow, lty=2, col='red')	#lower bound of predicted interval
points(nn+1:nt, nvda_part2[,6], pch=2, col='black', type='o')
legend.text = c("Actual value", "Prediction")
legend("bottomright", legend.text, col=1:2, pch=1:2, lty=rep(1,2))

#OUTLIERS

outliers = locate.outliers(MA8_MODEL_sig$residuals, pars = coefs2poly(MA8_MODEL_sig))
outlier_counts = table(outliers$type)
print(outlier_counts)

