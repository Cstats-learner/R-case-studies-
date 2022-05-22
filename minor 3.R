#load the data
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
   


df<- read_csv("19MSMS33 MINOR3.csv")
df<-as.data.frame(df)
imp_df<-na_kalman(df)#imputed missing values with mean
 #making a time date format
# A timeDate Sequence
date.sequence <- timeSequence(as.Date("2020-07-06"), as.Date("2021-07-06"));  # a short example period with three London holidays
date.sequence;

# holidays in the period
years.included <- unique( as.integer( format( x=date.sequence, format="%Y" ) ) );
holidays <- holidayNYSE(years.included)  #  (mentioning the number of holidays during that time period )
business.days <- date.sequence[isBizday(date.sequence, holidays)]; 
business.days
HighRates<-imp_df$High
plot(business.days,HighRates,type="o",ylab="highest price of Gold stock in USD",xlab="MONTHS",main="plot of highest price of gold rates in a period of one year")
#plotting done
HighestPrice<-xts(HighRates,order.by = business.days)
plot(HighestPrice)
plot.xts(HighestPrice)
 plot.ts(HighestPrice)
#decomposing a time series
 #order of SMA=5
 HighestPriceSMA3<-SMA(HighestPrice,n=5)
 plot.xts(HighestPriceSMA3)
 HighestPriceSMA43<-SMA(HighestPrice,n=43)
 plot.xts(HighestPriceSMA43)
Date<-as.Date(imp_df$Date)
 #decomposing seasoality, if any
HighestPriceseriesComponent<- decompose(HighestPrice)
#Error in decompose(HighestPrice) : 
#time series has no or less than 2 periods
#differencing the time series
diff1<-diff(HighPrice)
plot(diff(HighPrice))
diff2<-diff(diff1)
plot(diff2)
acf(HighestPrice,lag=30)
plot(acf(HighestPrice))
pacf(HighestPrice)
print(pacf(HighestPrice))
plot(pacf(HighestPrice))
print(acf(HighestPrice,lag=30))
#plotting the differenced series
plot(Acf(diff1, main= "ACF plot of 1st order differenced time series"))
adf.test(HighestPrice)

G_rate<-df$High
G_rate<-ts(G_rate)
G_rate<-is.na(G_rate)


 H_rates<-df$High
 Date<-df$Date
 plot
 gold_rate<-xts(H_rates,order.by=as.POSIXct(df$Date))
 
