#install libraries
library(quantmod)
library(rugarch)
library(vars)
library(tseries)
library(forecast)
library(marima)

#create environments
ENV.train = new.env()
ENV.test = new.env()

#retrieve data 
tickers = c('SPY', 'SPXS', 'SPXL')
getSymbols(tickers, src = 'yahoo', from = '2008-12-31', to = '2022-12-31', 
           env = ENV.train)
getSymbols(tickers, src = 'yahoo', from = '2023-1-1', env = ENV.test)

#estimate returns
return = function(xts){
  (Cl(xts) - Op(xts))/Op(xts)
}

daily.ret = na.omit(do.call(merge, eapply(ENV.train, return)))
colnames(daily.ret) = sub(".Close", ".ret", colnames(daily.ret))

#estimate returns of a hedged portfolio
port0 = na.omit(cbind(daily.ret[,1:2], 3 * daily.ret[,3]))
port0$hedged.Close = 2*port0[,2] + port0[,1] + port0[,3]

#repeat for the test data
daily.ret.test = na.omit(do.call(merge, eapply(ENV.test, return)))
colnames(daily.ret.test) = sub(".Close", ".ret", colnames(daily.ret.test))
port0.test = na.omit(cbind(daily.ret.test[,1:2], 3 * daily.ret.test[,3]))
port0.test$hedged.Close = 2*port0.test[,2] + port0.test[,1] + port0.test[,3]

#estimate volatility for train data
vol.gk.yz = function(xts){
  volatility(xts, n = 1, calc = "gk.yz", N = 252)
}

assets.vols = na.omit(do.call(merge, eapply(ENV.train, vol.gk.yz)))

#estimate the value of your portfolio if at t = 1 you bought/sold at $1, 
# closed the postion at the closeing return, repeated the process with the new
#value of your porfolio 
port.price.short = numeric(nrow(port0))
port.price.long = numeric(nrow(port0))

for (i in 1:(nrow(port0)) ){
  port.price.short[i] = prod((1 + (-1 * port0$hedged.Close))[1:i])
  port.price.long[i] = prod((1 + port0$hedged.Close)[1:i])
}

port.prices = merge(xts( as.data.frame(port.price.short) , 
                         order.by = index(ENV.train[["SPY"]]) ), 
                    xts ( as.data.frame(port.price.long) ,
                          order.by = index(ENV.train[["SPY"]]) ))

#estimate the value of a purely SPY portfolio if at t = 1 you bought/sold at $1
SPY.price.long = numeric(nrow(port0))

for (i in 1:nrow(port0) ){
  SPY.price.long[i] = prod((1 + port0$SPY.ret)[1:i])
}

SPY.price.long = xts(as.data.frame(SPY.price.long),
                     order.by = index(ENV.train[["SPY"]]) )

#compare both portfolio's returns, showing that shorting the hedged portfolio daily
# beats buying SPY daily
plot(SPY.price.long)
plot(port.prices$port.price.short)

#TEST 1, difference and try a VAR model of volatility and portfolio returns
#difference SPY volatility and portfolio returns mean/median and bind them into
#one testable xts object

#test for stationarity in both differenced volatility measures and the returns 
#of the short portfolio
assets.vols.diff = na.omit(diff(assets.vols$SPY, 1, 1))


adf.test(assets.vols$SPY)
adf.test(port0$hedged.Close)

#determine if there is an AR or MA process 
#portfolio is MA(2) while volatility is AR(3)

acf(assets.vols$SPY);pacf(assets.vols$SPY)
acf(port0$hedged.Close);pacf(port0$hedged.Close)


#We have a stationary model, try and fit ARIMAX
ARIMAX.data = na.omit(cbind(port0$hedged.Close, assets.vols$SPY))
ARIMAX.1.fit = auto.arima(ARIMAX.data[,1], xreg = ARIMAX.data[,2])
checkresiduals(ARIMAX.1.fit)

ARIMAX.2.fit = Arima(ARIMAX.data[,1], order = c(0,0,2), 
                     xreg = ARIMAX.data[,2])
checkresiduals(ARIMAX.2.fit)

#found a good fit, do the same with lagged value of volatility
ARIMAX.data.lag = na.omit(cbind(ARIMAX.data, lag(assets.vols$SPY)))
ARIMAX.3.fit = arima(ARIMAX.data.lag[,1], order = c(0,0,2), 
                     xreg = ARIMAX.data.lag[,2:3])
checkresiduals(ARIMAX.3.fit)


#Try new data for hedged port without underlying
ARIMAX.data.2 = cbind((daily.ret[,1] + daily.ret[,2] ), daily.ret[,3])
colnames(ARIMAX.data.2) = c("hedged.ret", "SPY.ret")

acf(ARIMAX.data.2[,1])
pacf(ARIMAX.data.2[,1])

ARIMAX.5.fit = arima(ARIMAX.data.2[,1], order = c(9,0,9), 
                     xreg = ARIMAX.data.2[,2])
checkresiduals(ARIMAX.5.fit)

#have to add the extra observation at the begging of the test set so that I
#don't skip 2023-01-01 obs of vol diffs
SPY.vol = rbind(ENV.train[["SPY"]], ENV.test[["SPY"]])
asset.vol.test = vol.gk.yz(SPY.vol)
asset.vol.diff.test = na.omit(diff(asset.vol.test,1,1))
asset.vol.diff.test = asset.vol.diff.test[-c(1:nrow(assets.vols.diff))]
colnames(asset.vol.diff.test) = c("SPY")

SPY.ret = port0.test$hedged.Close

#not any better, so I am going to compare the first two forecasts to the actual
#value
forecast.1 = forecast(ARIMAX.1.fit, xreg = asset.vol.diff.test)
forecast.2 = forecast(ARIMAX.2.fit, xreg = asset.vol.diff.test)
forecast.5 = forecast(ARIMAX.5.fit, xreg = SPY.ret)


comp = xts(cbind(forecast.1$mean, forecast.2$mean), order.by = index(port0.test))
comp = cbind(port0.test$hedged.Close, comp)

#using positive/negative prediction as indication, do we beat the short strategy
#for the selected period?
long.or.short.for.1 = c()
long.or.short.for.2 = c()

for ( x in 1:nrow(comp) ){
  if(comp$forecast.1.mean[x] > 0){
    long.or.short.for.1[x] = 1
  }else{
    long.or.short.for.1[x] = -1}
}

for ( x in 1:nrow(comp) ){
  if(comp$forecast.2.mean[x] > 0){
    long.or.short.for.2[x] = 1
  }else{
    long.or.short.for.2[x] = -1}
}

#visualize the returns of holding vs using forecasts
hold = c()
test.returns.1 = c()
test.returns.2 = c()

for (i in 1:nrow(port0.test) ){
  hold[i] = prod((1 - port0.test$hedged.Close)[1:i])
  test.returns.1[i] = prod((1 + (port0.test$hedged.Close*long.or.short.for.1))[1:i])
  test.returns.2[i] = prod((1 + (port0.test$hedged.Close*long.or.short.for.2))[1:i])
}

SPY.test.price.long = c()

for (i in 1:nrow(port0.test) ){
  SPY.test.price.long[i] = prod((1 + port0.test$SPY.ret)[1:i])
}

SPY.test.price.long = xts(as.data.frame(SPY.test.price.long),
                     order.by = index(port0.test) )

comp.test.ret.1 = merge( xts( as.data.frame(hold), order.by = index(port0.test)), 
                       xts( as.data.frame(test.returns.1), order.by = index(port0.test)))


comp.test.ret.1.2 = merge( xts( as.data.frame(hold), order.by = index(port0.test)), 
                       xts( as.data.frame(test.returns.1), order.by = index(port0.test)),
                       xts( as.data.frame(test.returns.2), order.by = index(port0.test)))

comp.test.ret.spy = merge( xts( as.data.frame(hold), order.by = index(port0.test)), 
                  xts( as.data.frame(test.returns.1), order.by = index(port0.test)),
                  xts( as.data.frame(test.returns.2), order.by = index(port0.test)),
                  xts( as.data.frame(SPY.test.price.long), order.by = index(port0.test)))
plot(comp.test.ret)
plot(comp.test.ret)


#for a brief period we over perform, but by thend we are nolonger overperforming


#DID NOT MAKE IT INTO PAPER

#VAR fit
VAR.data = na.omit(cbind(port0$hedged.Close, assets.vols$SPY))
VARselect(VAR.data, lag.max = 30, type = "none")

#As Because we have an MA process it is asking us to use a lot of lags, let's 
#try that out

VAR.fit = VAR(VAR.data, lag = 20)

#try again with a lagged data set
VAR.data.lag = na.omit(cbind(port0$hedged.Close, lag(assets.vols$SPY)))
VARselect(VAR.data.lag, lag.max = 30, type = "none")
VAR.lag.fit = VAR(VAR.data.lag, lag = 17)

#compare AICs, we see very little difference
VAR.fit$totobs
VAR.lag.fit$totobs

#forecasting ability 
forecast.3 = forecast(VAR.lag.fit, h = 88)
forecast.4 = predict(VAR.fit, h = 88)

