# Blog Zcash Script #

# Blog post found at http://btc.bashingbitcoin.com/2018/08/revisiting-relationship-between-zcash.html #

## Loading packages ##
require(forecast)
require(tseries)
require(arima)
require(ggplot2)


# Read in and set appropriate timeframe
setwd('/Users/nickbruno/Documents/Summer_Project/bashingbitcoin/blog2')
zcash_data <- read.csv('new_zcash_data.csv', header=T)
zcash_current <- zcash_data[311:639,] # starts at september 3 to July 28
zcash_interest <- read.csv('zcash_interest.csv', header=T) # starts at september 3
zcash_interest_current <- zcash_interest[1:48,] # starts at september 3 and ends July 29

# Merging the dataframes
total <- merge(zcash_current,zcash_interest_current,by="Counter")
total
t <- 1:(nrow(total))
t

# Looking at overall trends
plot(total$Interest, type="l", col="red", ylab='', yaxt='n', xlab='' ) # interest
par(new=TRUE)
plot(total$price, type="l", col="green", ylab='Zcash Price', xlab='Date', main='Comparison of Zcash Interest and Price') # 
  # allows both lines to be present in the same graph

# Fitting a linear model
linmod <- lm((log(price)) ~ Interest + t , data = total)
linmod
# Coefficients:
#   (Intercept)     Interest            t  
# 5.28832      0.01239      0.00104  
summary(linmod)
  # low adjusted R-squared(0.389)
plot(linmod$residuals)
  # look not as correlated

# Now see if ARIMA would be appropriate
acf(diff(log((zcash_current$price))))
pacf(diff(log((zcash_current$price))))


# Now we will try to create an ARIMA function
auto.arima(diff(log(zcash_current$price)))
  # suggests an ARIMA (2,0,2) model

fitarima <- Arima(diff(log(zcash_current$price)), order=c(2,0,2))
summary(fitarima)
# Coefficients:
#   ar1     ar2    ma1    ma2
# -0.513  -0.870  0.469  0.930
# s.e.   0.058   0.089  0.045  0.074
  # negative AIC of -787.11
  # log-likelihood of 399.56

# Below code doesn't do much
x <- ARMAacf(ar = c(-0.513, -0.87), ma = c(0.469, 0.93))
plot(x) # plots points

plot(forecast(fitarima)) # gives us the desired ARIMA plot
autoplot(fitarima)
  # roots outside the unit circle
adf.test(diff(log(zcash_current$price)), alternative = "stationary", k = 0)
  # THIS WILL PROVE THAT OUR DATA IS STATIONARY
# Dickey-Fuller = -19.08, Lag order = 0, p-value = 0.01
# alternative hypothesis: stationary

# Now look at ARIMA predictions
predict <- predict(fitarima, n.ahead=14) # predicts basically no movement
tail <- zcash_data[641:655,] # same as 'tail_zcash_data'
  # in reality the price dropped a lot, which was not predicted by the model
just_price <- as.numeric(tail$price) # keeps the decimal places
tail2 <- as.data.frame(sapply(tail, function(x) diff(log(x))))
  # looks at the difference in log returns during the tail that ARIMA tried to predict.

tail_zcash_data <- tail(zcash_data, 16)
plot(diff(log(tail_zcash_data$price)), type='l', xlab="Zcash Returns")
  # graphs actual returns of data from july 29 to august 13
actual_zcash_returns <- diff(log(tail_zcash_data$price))
predictions <- predict
predictions$pred
actual_zcash_returns
