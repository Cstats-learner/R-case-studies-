library(readr)
library(timeDate)
library(timeSeries)
library(forecast)
library(ggplot2)
library(imputeTS)
library(zoo)
library(xts)
library(tseries)
win.graph(width=4.875,height=2.5,pointsize = 8)
library(leaps)
library(TTR)
library(quantmod)
library(lubridate)



df<- read.csv("GC=F.csv")
df$Date<-dmy(df$Date)
head(df)
qplot(df$Date,df$Close,data=df,main="Closing price of Gold in the US bullion market in dollars/ounce")
date.sequence <- timeSequence(as.Date("2020-07-06"), as.Date("2021-07-06"));  # a short example period with three London holidays
date.sequence;

# holidays in the period
years.included <- unique( as.integer( format( x=date.sequence, format="%Y" ) ) );
holidays <- holidayNYSE(years.included)  #  (mentioning the number of holidays during that time period )
business.days <- date.sequence[isBizday(date.sequence, holidays)]; 
business.days
closeRates<-df$Close
plot(business.days,closeRates,type="o",ylab="closing price of Gold",xlab="MONTHS",main="plot of closing price of gold rates in a period of one year")

stockvar.de=decompose(closingPrice)



closingPrice<-xts(closeRates,order.by = business.days,frequency = 3)
plot.xts(closingPrice)
closingPriceSMA3<-SMA(closingPrice,n=5)
plot.xts(closingPriceSMA3)
closingPriceSMA30<-SMA(closingPrice,n=30)
plot.xts(closingPriceSMA30)
Date<-as.Date(imp_df$Date)
#decomposing seasoality, if any
closingPriceseriesComponent<- decompose(closingPrice)
#Error in decompose(HighestPrice) : 
#time series has no or less than 2 periods
#differencing the time series
par(mfrow=c(1,1))
diff1<-diff(log(closingPrice))
plot(diff(closingPrice))
diff2<-diff(diff1)
plot(diff2)
acf(diff1)
plot(acf(diff1))
pacf(diff1)
print(pacf(closingPrice))
plot(pacf(closingPrice))
print(acf(diff1,lag=43))
#plotting the differenced series
plot(Acf(diff1, main= "ACF plot of 1st order differenced time series"))
adf.test(closingPrice)
adf <- adf.test(diff2[3:253], alternative = c("stationary", "explosive"))

 
 adf
 library(forecast)
 library(caTools)
 train_data <- diff2[3:220]
 set.seed(123)
 arima_model <- auto.arima(train_data, stationary = TRUE, ic = c("aicc", "aic", "bic"), 
                           trace = TRUE)
 summary(arima_model)
