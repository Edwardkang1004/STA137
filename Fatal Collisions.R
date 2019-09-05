library(forecast)
myvector=c(295, 272, 278, 309, 322, 296, 362, 353, 314, 317, 267, 316, 270, 247, 273, 289, 318, 304, 389, 360, 308, 370, 357, 337, 293, 265, 299, 286, 329, 330, 338,
           357, 346, 312, 322, 316, 280, 260, 316, 295, 295, 297, 290, 330, 290, 316, 317, 271, 236, 260, 223, 277, 250, 252, 249, 284, 288, 275, 248, 271, 248, 212,
           218, 218, 236, 219, 208, 276, 221, 263, 251, 235, 175, 198, 185, 211, 197, 201, 224, 206, 237, 222, 240, 224, 220, 201, 194, 179, 216, 206, 247, 215, 234,
           252, 236, 228, 216, 198, 209, 217, 224, 230, 244, 240, 246, 246, 256, 232, 194, 196, 242, 256, 223, 222, 247, 265, 237, 276, 254, 241)

#create the time series for data ¡°Fatal Collisions by Month ¨C 2004-2013¡±
FatalCollision <- ts(myvector, start=c(2004, 1), end=c(2013, 12), frequency=12)
x = myvector
n = length(x)
t = 1:n

#plot the time series data
plot(t,x, type="l", main="Number of Fatal Collision (2004-2013)", xlab = "Months", ylab="Number of Fatal Collision")

#time series decomposition
plot(decompose(FatalCollision))

#remove trend
trend.fit = lm(x~t)

#plot residuals after trend is removed
y.trend = resid(trend.fit)
plot(t,y.trend, type="l", main="Trend Removed", ylab="")

#remove the seasonal component
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
dat = data.frame(y.trend, harm)
fit = lm(y.trend~., data=dat)
summary(fit)

# setup the full model and the model with only an intercept
full = lm(y.trend~.,data=dat)
reduced = lm(y.trend~1, data=dat)

#stepwise regression starting with the full model
fit.back = step( full, scope = formula(reduced), direction = "both")

#get back the original t so that we can plot over this range
t = 1:n

#plot the estimated seasonal components
plot(t,y.trend, type="l", main = "Estimated Seasonal Components", col="darkgrey", ylab="")
lines(t, fitted(fit.back), col="red")

#plot the residuals after seasonal component is removed
ts.plot(residuals(fit.back), main="After seasonal componenets removed", ylab = "", xlab="t")

#plot the acf and pacf of the residuals
y = residuals(fit.back)
acf(y)
pacf(y)

fit.y = auto.arima(y,allowmean = F, trace=T, stepwise=F)

z = resid(fit.y)
acf(z)
pacf(z)

#test to see if there is enough to reject independence
Box.test(z,type="Ljung-Box", lag = floor(min(2*d, n/5)))

#look at the qqplot of the residuals to see if normality can be assumed
qqnorm(z)

#forecast the noise
fc = forecast(fit.y, h=12, level = .95)

#plot the noise forecast
plot(fc)

#forecast the seasonal component and noise
season.fc = fit.back$fitted.values[1:12]+fc$mean

#forecast the trend
trend.fc = predict(trend.fit, newdata = data.frame(t=121:132))

#add the seasonal and noise forecasts
x.hat = season.fc+trend.fc

#plot the forecasts
plot(t, x, xlim = c(1,132), type="l" ,xlab = "Months", ylab="Number of Fatal Collision")
lines(120:132, c(x[120], x.hat), col="red")

#add the forecast intervals
lines(120:132, c(x[120], x.hat+fc$lower), col="blue", lty=2)
lines(120:132, c(x[120], x.hat+fc$upper), col="blue", lty=2)

#closer view
plot(121:132, x.hat, type = "o", main="Forecasting Number of Fatal Collision in a 12-Month Period",
     col= "red", xlab = "Months", ylab="Number of Fatal Collision")