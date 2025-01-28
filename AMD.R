#AMD CODE

# Library
library(fBasics)
library(tseries)
library(astsa)
library(lmtest)
library(TSA)
library(forecast)
library(tsoutliers)

set.seed(99)
amd = read.csv("AMD.csv", header = T)
head(amd)
summary(amd)

total_rows <- nrow(amd)  
total_rows
#Data frame for all rows other than last 10 rows
amd_part1 <- amd[1:(total_rows - 10), ]
amd_part1
total_rows1 <- nrow(amd_part1) 
total_rows1
# Data frame for the last 10 rows
amd_part2 <- amd[(total_rows - 9):total_rows, ]
total_rows2 <- nrow(amd_part2) 
total_rows2

x=amd_part1[,6] 
length(x)

#BEFORE LOG TRANSFORMATION
date1 = as.Date(amd_part1$Date, format = "%m/%d/%Y")
plot(date1, x,  xlab = "Time", ylab = "Adjusted Close",type = 'l', main = 'AMD Daily Stock price from 2014 Jan to 2024 March')
acf(x, main = 'ACF for data')
pacf(x, main = 'PACF for data')
adf.test(x) #data is non=stationary

#LOG TRANSFORMATION
logx = log(x)
length(logx)
par(mfrow = c(1, 3))
plot(date1, logx, type = 'l',  xlab = "Time", ylab = "Adjusted Close",main="Log Transformed Time Series")
acf(logx, main = 'ACF for log data')
pacf(logx, main = 'PACF for log data')
adf.test(logx) #still not stationary

#SEASONALITY
seas= mstl(logx)
plot(seas, main = 'Seasonality Plot')

#DIFFERNCE
dlogx = diff(logx)
plot(dlogx, type = 'l',  xlab = "Time", ylab = "Adjusted Close", main = 'AMD Differenced Time Series')
par(mfrow =c(1, 2))
acf(dlogx, main = 'ACF for difference data')
pacf(dlogx,main = 'ACF for difference data')
adf.test(dlogx) #now it is stationary
eacf(dlogx) 

#MEAN RETURN ANALYSIS
mean(dlogx) #mean of nvdia is more. so we will invest in nvidia as compared to amd
sd(dlogx) #sd of nvdia less than amd, safer to invest in nvidia
skewness(dlogx) #positively skewed: returns are balanced and spread out but nvidia is closer to 0 than amd.

#CORRELATION OF AMD AND NVDA
nvda = read.csv("NVDA.csv", header = T) #reading the file
head(nvda)
summary(nvda)
x1 <- nvda[,6]
logx1 = log(x1)
dlogx1 = diff(logx1)
newdata=cbind(dlogx1,dlogx)
cor(newdata)

#MODEL BUILDING AND SELECTION

#MA(1)
MA_1= arima(dlogx, order = c(0,0,1))
MA_1
coeftest(MA_1) #all models 

#MA(3)
MA_3 = arima(dlogx, order = c(0,0,3))
MA_3
coeftest(MA_3)

#ARMA(1,2)
ARMA_12 = arima(dlogx, order = c(1,0,2))
ARMA_12
coeftest(ARMA_12)

ARMA_12.sig = arima(dlogx, order = c(1,0,2),fixed= c(NA,NA,0,NA))
ARMA_12.sig
coeftest(ARMA_12.sig)

ARMA_12$aic
ARMA_12.sig$aic
#test for stationary and redundancy 
polyroot(c(1, -ARMA_12.sig$coef[1:1]))
abs(polyroot(c(1, -ARMA_12.sig$coef[1:1])))
polyroot(c(1, -ARMA_12.sig$coef[1:1]))
polyroot(c(1, ARMA_12.sig$coef[2:3]))

#COMPARSION TO CHOOSE THE BEST MODEL
#AIC
MA_1$aic
MA_3$aic
ARMA_12.sig$aic

#ROLLING FORECASTING
source("rolling.forecast.R")
rolling.forecast(dlogx, 1, length(x)-50, order = c(0,0,1))
rolling.forecast(dlogx, 1, length(x)-50 ,order = c(0,0,3)) 
rolling.forecast(dlogx, 1, length(x)-50, order = c(1,0,2),fixed= c(NA,NA,0,NA))

#choosing ARMA(1,2) BASED ON AIC AND ROLLING FORECASTING ERROR

#RESIDUAL ANALYSIS OF THE CHOOSEN MODEL
par(mfrow =c(1, 3))
plot(ARMA_12.sig$residuals)
acf(ARMA_12.sig$residuals)
pacf(ARMA_12.sig$residuals)
Box.test(ARMA_12.sig$residuals, lag = 12, type = 'Ljung')
tsdiag(ARMA_12.sig)

#PREDICTION
n=length(logx)
len_logx = length(logx)
len_logx
predictions <- numeric(10)
predup <- numeric(10)
predlow <- numeric(10)
xreg = 1:n
for (i in 1:10) {
  out4 <- arima(logx, order = c(1,1,2),fixed= c(NA,NA,0,NA), xreg = xreg)
  print(summary(out4))  # Printing model summary to check coefficients and statistics
  
  h <- 1  
  newxreg <- (n + 1):(n + h)
  pp <- predict(out4, n.ahead = h, newxreg = newxreg)
  
  print(pp$pred)  # Print the predicted log value
  
  pred <- exp(pp$pred)
  pred.upp = exp(pp$pred + 2 * pp$se)
  pred.low = exp(pp$pred - 2 * pp$se)
  
  print(c(pred, pred.upp, pred.low))  # Print transformed predictions and confidence intervals
  
  logx <- c(logx, log(pred))
  xreg <- c(xreg, newxreg)
  
  predictions[i] <- pred
  predup[i] <- pred.upp
  predlow[i] <- pred.low
}
predictions
predup
predlow

nn = len_logx
nn
nt = 10
nb = 50
tt = (nn-nb):nn
tt
xxx = amd_part1$Adj.Close[tt]
xxx
rr = range(c(xxx, predictions, predup, predlow))
par(mfrow = c(1, 1))

plot(tt, xxx, pch=2, xlim=c(nn-nb,nn+nt), ylim=rr, main='Prediction', ylab='Return', xlab='Time')	
lines(tt, xxx)	#observed values
points(nn+1:nt, predictions, pch=2, col='red', type='o')
lines(nn+1:nt, predup, lty=2, col='red')
lines(nn+1:nt, predlow, lty=2, col='red')
amd_part2[,6]
points(nn+1:nt, amd_part2[,6], pch=2, col='black', type='o')
legend.text = c("Actual value", "Prediction")
legend("bottomright", legend.text, col=1:2, pch=1:2, lty=rep(1,2))


#OUTLIERS

outliers = locate.outliers(ARMA_12.sig$residuals, pars = coefs2poly(ARMA_12.sig))
outlier_counts = table(outliers$type)
print(outlier_counts)

