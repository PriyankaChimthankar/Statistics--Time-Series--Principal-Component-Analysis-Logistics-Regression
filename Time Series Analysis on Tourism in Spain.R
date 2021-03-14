install.packages('fpp2')
library(fpp2)
library(tseries)
library(forecast)
# Creating a time series object in R
tourismdata <- read.csv('C:\\Users\\HP\\Desktop\\BedPlacesSpain.csv')


ttourismdata <- ts(tourismdata, start=c(1990,1), frequency=12) 
is.ts(ttourismdata)
plot.ts(ttourismdata,main="Tourism in Spain")


plot(ttourismdata)
start(ttourismdata) 
end(ttourismdata)
frequency(ttourismdata)

#Subsetting the series with the window function
ttourismdata.subset <- window(ttourismdata, start=c(1999, 1), end=c(2000, 12))
ttourismdata.subset
autoplot(ttourismdata.subset)

#seasonal plot
ggseasonplot(ttourismdata,year.labels = TRUE,year.labels.left = TRUE)+ylab('Hotel Rooms booked in Spain ')+ggtitle('Seasonal Plot for Tourism in Spain')

#seasonal subseries plot
ggsubseriesplot(ttourismdata)+ylab('Hotel Rooms booked in Spain')+ggtitle('Seasonal Subseries Plot for Tourism in Spian')

#moving average
tt<-ma(ttourismdata,5)
forecast(tt)
plot(forecast(tt))
plot(ma(ttourismdata,5),main="Tourism in Spain", ylab="Hotel Rooms booked in Spain")
#autoplot(ttourismdata)+autolayer(ma(ttourismdata,3))+autolayer(ma(ttourismdata,7))

ggtsdisplay(ttourismdata)

#seasonal decomposition

monthplot(ttourismdata)
seasonplot(ttourismdata)
#Seasonal decomposition using decompose() - additive [Not appropriate here]
fit.decadd<-decompose(ttourismdata, type = "additive")
fit.decadd
plot(fit.decadd)

#Seasonal decomposition using decompose() - multiplicative
fit.decmult<-decompose(ttourismdata,type = "multiplicative")
fit.decmult
plot(fit.decmult)


  #Seasonal decomposition using stl()
lttourismdata <- log(ttourismdata)
plot(lttourismdata, ylab="log(TourismData)")
fit.stl <- stl(ttourismdata, s.window="period")           
plot(fit.stl)
fit.stl$time.series                                 
exp(fit.stl$time.series)
ttourismdata
dim(lttourismdata)

#simple exponential smoothning
tourismfit<-ses(ttourismdata,h=4)
tourismfit
round(accuracy(tourismfit),2)
autoplot((tourismfit))
autoplot(tourismfit)+autolayer(fitted(tourismfit),series='Fitted')

fit<-ets(ttourismdata,model="ZZZ")
fit
forecast(fit,10)
round(accuracy(fit),2)

#holt's winter seasonal model
additivefit<-hw(ttourismdata,seasonal="additive")
summary(additivefit)
autoplot(ttourismdata,main="Forecasting using Holt's Winter Seasonal Method",ylab="Hotel Rooms booked in Spain ")+autolayer(additivefit,series="HW Additive forecasts",PI=FALSE)
#xlab("Year")
#ylab("(Percentage)")+ggtitle("")+guides(colour=guide_legend(title="Forecast"))

#Average model
m<-meanf(ttourismdata,h=20)
summary(m)
plot(m,ylab="Hotel Rooms booked in Spain",xlab="Time")

#ARIMA  
plot(ttourismdata)

ndiffs(ttourismdata)
nsdiffs(ttourismdata) 
diff_tourismdata<-diff(ttourismdata)
plot(diff_tourismdata)
adf.test(diff_tourismdata)
Acf(ttourismdata)
pacf(ttourismdata)
Acf(diff_tourismdata)
pacf(diff_tourismdata)
#seasonal Arima
plot(ttourismdata)
ndiffs(ttourismdata)
nsdiffs(ttourismdata) 
diff_tourismdata<-diff(ttourismdata)
adf.test(diff_tourismdata)
Acf(diff_tourismdata)
Pacf(diff_tourismdata)
plot(diff_tourismdata,main="Stationary Time Series")
#fit<-arima(ttourismdata,order=c((1,1,1)(2,1,1)[12]))

fit_sarima<-Arima(ttourismdata,order = c(2,1,1),seasonal = c(2,1,1))


#####fit_2<-Arima(ttourismdata,order = c(2,1,1),seasonal = c(2,1,1))   #####1971.76 ####3.616769####

summary(fit_sarima)
forecast(fit_sarima,h=4)
plot(forecast(fit_sarima), xlab="Year", ylab="Hotel Rooms booked in Spain")

#forecast(fit,10)

#Evaluating Model Fit
qqnorm(fit_sarima$residuals)
qqline(fit_sarima$residuals)
Box.test(fit_sarima$residuals, type="Ljung-Box")
checkresiduals(fit_sarima)
accuracy(fit_sarima)
summary(fit_sarima)
#Forecasting with the fitted model
forecast(fit, 10)
plot(forecast(fit, 20), xlab="Year", ylab="Hotel Rooms booked in Spain")

#auto ARIMA function
plot(ttourismdata)
fit5 <- auto.arima(ttourismdata)
fit5

#seasonal Naive Model
seasonalnaive<-snaive(ttourismdata,h=4)
summary(seasonalnaive)
plot(seasonalnaive,ylab="Hotel Rooms booked in Spain",xlab="Time")

fcast<-meanf(pigs,h=4)
summary(fcast)
plot(fcast)
