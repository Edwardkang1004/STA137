require(astsa)

data(cmort)

#use only years with a complete cycle
cmort.part = window(cmort, start=c(1970,1), end=c(1978,52))

ts.plot(cmort.part)

x = as.vector(cmort.part)
n = length(x)
t = 1:n

#remove the trend
trend.fit = lm(x~t)
y.trend = resid(trend.fit)

ts.plot(x)
lines(fitted(trend.fit), col = "red")



#remove the seasonal component
t = (t) / n

d=52
n.harm = 26 #set to [d/2]
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
plot(t,y.trend, type="l", col="darkgrey", ylab="")
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


Box.test(z,type="Ljung-Box", lag = floor(min(2*d, n/5)))

#look at the qqplot an histogram of the residuals to see if
#normality can be assumed
qqnorm(z)
hist(z)

shapiro.test(z)
#a small p-value reject the hypothesis that the residuals are normal. 
#since we are rejecting normality here, the forecasts intervals may not
#be correct.

#let's forecast the noise
fc = forecast(fit.y, h=40, level = .95)


#plot the noise forecast
plot(fc)

#zoom in on the forecast
plot(fc, xlim=c(450,508))


#forecast the seasonal component and noise
season.fc = fit.back$fitted.values[1:40]+fc$mean

#forecast the trend
trend.fc = predict(trend.fit, newdata = data.frame(t=469:508))

#add the seasonal and noise forecasts
x.hat = season.fc+trend.fc



plot(t, cmort.part, xlim = c(1,508), type="l")
lines(468:508, c(cmort.part[468], x.hat), col="red")
#add the forecast intervals
lines(468:508, c(cmort.part[468], x.hat+fc$lower), col="red", lty=2)
lines(468:508, c(cmort.part[468], x.hat+fc$upper), col="red", lty=2)
#the actual values
lines(468:508, cmort[468:508], col="blue")




