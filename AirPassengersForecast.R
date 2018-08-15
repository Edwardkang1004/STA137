 

# Remove the last year.
# We will forecast this last year and see how well we did
x = window(AirPassengers, start=c(1949,1), end=c(1959,12))

x = as.vector(x)
n = length(x)
t = 1:n


plot(t,x, type="l", main="Number of Int. Airline Passengers (1949-1959)", ylab="")

#transform the data with a ln transformation to stabilize the variance
x = log(x)

plot(t, x, type="l", ylab="", main="Log transformed")


# remove trend
t2 = t^2
trend.fit = lm(x~t+t2)

plot(t, x, type="l", ylab="", main="Log transformed")
lines(t,fitted(trend.fit))

# plot residuals after trend is removed
y = residuals(trend.fit)
plot(t,y, type="l", main="Trend Removed", ylab="")


#use t that is in the interval [0,1]
n = length(t)
t=1:length(y)
t = (t) / n

# make matrix of the harmonics
d=12 #number of time pionts in each season
n.harm = 6 #set to [d/2]
harm = matrix(nrow=length(t), ncol=2*n.harm)
for(i in 1:n.harm){
  harm[,i*2-1] = sin(n/d * i *2*pi*t)
  harm[,i*2] = cos(n/d * i *2*pi*t)
}
colnames(harm)= 
  paste0(c("sin", "cos"), rep(1:n.harm, each = 2))

#fit on all of the sines and cosines
dat = data.frame(y, harm)
fit = lm(y~., data=dat)
summary(fit)

# setup the full model and the model with only an intercept
full = lm(y~.,data=dat)
reduced = lm(y~1, data=dat)

#stepwise regression starting with the full model
fit.back = step( full, scope = formula(reduced), direction = "both")

#get back the original t so that we can plot over this range
t = 1:n

#plot the estimated seasonal components
plot(t,y, type="l", col="darkgrey", ylab="")
lines(t, fitted(fit.back), col="red")


#plot the residuals after seasonal component is removed
ts.plot(residuals(fit.back), main="After seasonal componenets removed", ylab = "", xlab="t")

# Use H-K algorithm to determine best model
require(forecast)
arma.fit = auto.arima(resid(fit.back),allowmean = F)

# examine the residuals of the arma fit
wn = resid(arma.fit)

acf(wn, na.action = na.pass)
pacf(wn, na.action = na.pass)

# since there were some significant correlations in the plots, test
# to see if there is enough to reject independence
Box.test(wn, type="Ljung-Box",lag = min(2*d, floor(n/5)) )

# forecast the next year of noise
noise.f = forecast(arma.fit, 12)

plot(noise.f)


# forecast the seasonal component with the noise
# Since the seasonal component just repeats for every year
# the forecast is just the estimated seasonal components for 1,...,12
season.f = fitted(fit.back)[1:12]

plot(season.f+noise.f$mean)


#forecast the trend
t.f = 133:144
t.f2 = t.f^2

trend.f = predict(trend.fit,newdata = data.frame(t=t.f, t2 = t.f2))

# add all components together to get the overall forecast
x.f = trend.f + season.f + noise.f$mean

#undo the natural log transformation
ex.f = exp(x.f)

#plot the forecasts on top of the true values
x = as.vector(AirPassengers)
plot(1:144, x, type="l", col="darkgrey")
lines(132:144, c(x[132], ex.f), col="red")

#closer view
plot(120:144, x[120:144], type="l", col="darkgrey")
lines(132:144, c(x[132], ex.f), col="red")

