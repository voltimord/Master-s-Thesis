d=read.csv(file.choose())
d2 =read.csv(file.choose())

install.packages("ggplot2")
library(ggplot2)
install.packages("xts")
library(xts)
install.packages("forecast")
library("forecast")
install.packages("fUnitRoots")
library("fUnitRoots")
install.packages("tseries")
library("tseries")
install.packages("urca")
library("urca")
install.packages("fpp")
library("fpp")

basicStats(Newsales_SD)

cor(Newsales_SD, Creditloan_SD)

HPI<-ts(d$HPI,start=c(1978,1), end=c(2014,5), frequency=12)
HPI_SD<-ts(d$HPI_SD,start=c(1978,1), end=c(2014,5), frequency=12)
HPI_SR<-ts(d$HPI_SR,start=c(1978,1), end=c(2014,5), frequency=12)
HPI_NE<-ts(d$HPI_NE,start=c(1978,1), end=c(2014,5), frequency=12)
HPI_NE_SD<-ts(d$HPI_NE_SD,start=c(1978,1), end=c(2014,5), frequency=12)
HPI_MW<-ts(d$HPI_MW,start=c(1978,1), end=c(2014,5), frequency=12)
HPI_MW_SD<-ts(d$HPI_MW_SD,start=c(1978,1), end=c(2014,5), frequency=12)
HPI_S<-ts(d$HPI_S,start=c(1978,1), end=c(2014,5), frequency=12)
HPI_S_SD<-ts(d$HPI_S_SD,start=c(1978,1), end=c(2014,5), frequency=12)
HPI_W<-ts(d$HPI_W,start=c(1978,1), end=c(2014,5), frequency=12)
HPI_W_SD<-ts(d$HPI_W_SD,start=c(1978,1), end=c(2014,5), frequency=12)
CCI<-ts(d$CCI, start=c(1978,1), end=c(2014,5), frequency=12) 
CCI_SR<-ts(d$CCI_SR, start=c(1978,1), end=c(2014,5),frequency=12)
CCI_SD<-ts(d$CCI_SD, start=c(1978,1), end=c(2014,5),frequency=12)
CCI_NE<-ts(d$CCI_NE, start=c(1978,1), end=c(2014,5), frequency=12) 
CCI_NE_SD<-ts(d$CCI_NE_SD, start=c(1978,1), end=c(2014,5), frequency=12) 
CCI_MW<-ts(d$CCI_MW, start=c(1978,1), end=c(2014,5), frequency=12) 
CCI_MW_SD<-ts(d$CCI_MW_SD, start=c(1978,1), end=c(2014,5), frequency=12) 
CCI_S<-ts(d$CCI_S, start=c(1978,1), end=c(2014,5), frequency=12) 
CCI_S_SD<-ts(d$CCI_S_SD, start=c(1978,1), end=c(2014,5), frequency=12) 
CCI_W<-ts(d$CCI_W, start=c(1978,1), end=c(2014,5), frequency=12) 
CCI_W_SD<-ts(d$CCI_W_SD, start=c(1978,1), end=c(2014,5), frequency=12) 
Creditloan<-ts(d$Creditloan, start=c(1978,1), end=c(2014,5), frequency=12) 
Creditloan_SR<-ts(d$Creditloan_SR, start=c(1978,1), end=c(2014,5), frequency=12) 
Creditloan_SD<-ts(d$Creditloan_SD, start=c(1978,1), end=c(2014,5), frequency=12) 
FCFI<-ts(d$FCFI, start=c(1978,1), end=c(2014,5), frequency=12) 
FCFI_SR<-ts(d$FCFI_SR, start=c(1978,1), end=c(2014,5), frequency=12) 
FCFI_SD<-ts(d$FCFI_SD, start=c(1978,1), end=c(2014,5), frequency=12) 
HouseSt<-ts(d$HouseSt, start=c(1978,1), end=c(2014,5), frequency=12) 
HouseSt_SR<-ts(d$HouseSt_SR, start=c(1978,1), end=c(2014,5), frequency=12)
HouseSt_SD<-ts(d$HouseSt_SD, start=c(1978,1), end=c(2014,5), frequency=12)
Unem<-ts(d$Unem, start=c(1978,1), end=c(2014,5), frequency=12) 
Unem_SR<-ts(d$Unem_SR, start=c(1978,1), end=c(2014,5), frequency=12) 
Unem_SD<-ts(d$Unem_SD, start=c(1978,1), end=c(2014,5), frequency=12) 
HousePer<-ts(d$HousePer, start=c(1978,1), end=c(2014,5), frequency=12) 
HousePer_SR<-ts(d$HousePer_SR, start=c(1978,1), end=c(2014,5), frequency=12)
HousePer_SD<-ts(d$HousePer_SD, start=c(1978,1), end=c(2014,5), frequency=12)
Newsales<-ts(d$Newsales, start=c(1978,1), end=c(2014,5), frequency=12) 
Newsales_SR<-ts(d$Newsales_SR, start=c(1978,1), end=c(2014,5), frequency=12) 
Newsales_SD<-ts(d$Newsales_SD, start=c(1978,1), end=c(2014,5), frequency=12) 
Year<-ts(d$Year,start=c(1978,1), end=c(2014,5), frequency=12)
Year_SD<-ts(d$Year_SD,start=c(1978,1), end=c(2014,5), frequency=12)

test_data <- data.frame(
  var0 = HPI_SR,
  var1 = CCI_SR,
  date = seq.Date(as.Date("1978-01-01"), by="1 month", length.out=437))

ggplot(test_data, aes(date)) + 
  geom_line(aes(y = var0, colour = "HPI_SR")) + 
  geom_line(aes(y = var1, colour = "CCI_SR"))

plot("HPI_SR")

HPI_SD_Diff1 <- diff(HPI_SD, differences=1)
HPI_SD_Diff2 <- diff(HPI_SD, differences=2)
HPI_SD_Diff3 <- diff(HPI_SD, differences=3)

CCI_SD_Diff1 <- diff(CCI_SD, differences=1)
CCI_SD_Diff2 <- diff(CCI_SD, differences=2)
CCI_SD_Diff3 <- diff(CCI_SD, differences=3)

plot(Year,CCI, xlab="Year",ylab="CCI")
lines(CCI)
plot(Year,CCI_SD, xlab="Year",ylab="CCI_SD")
lines(CCI_SD)
plot(Year,CCI_SR, xlab="Year",ylab="CCI_SR")
lines(CCI_SR)
plot(Year,HPI, xlab="Year",ylab="HPI")
lines(HPI)
plot(Year,HPI_SD, xlab="Year",ylab="HPI_SD")
lines(HPI_SD)
plot(Year,HPI_SR, xlab="Year",ylab="HPI_SR")
lines(HPI_SR)
plot(Year,Creditloan_SR, xlab="Year",ylab="Creditloan_SR")
lines(Creditloan_SR)
plot(Year,Creditloan_SD, xlab="Year",ylab="Creditloan_SD")
lines(Creditloan_SD)
plot(Year,FCFI_SD, xlab="Year",ylab="FCFI_SD")
lines(FCFI_SD)
plot(Year,FCFI_SR, xlab="Year",ylab="FCFI_SR")
lines(FCFI_SR)
plot(Year,HouseSt_SD, xlab="Year",ylab="HouseSt_SD")
lines(HouseSt_SD)
plot(Year,HouseSt_SR, xlab="Year",ylab="HouseSt_SR")
lines(HouseSt_SR)
plot(Year,HousePer_SD, xlab="Year",ylab="HousePer_SD")
lines(HousePer_SD)
plot(Year,HousePer_SR, xlab="Year",ylab="HousePer_SR")
lines(HousePer_SR)
plot(Year,Unem_SR, xlab="Year",ylab="Unem_SR")
lines(Unem_SR)
plot(Year,Unem_SD, xlab="Year",ylab="Unem_SD")
lines(Unem_SD)
plot(Year,Newsales_SD, xlab="Year",ylab="Newsales_SD")
lines(Newsales_SD)
plot(Year,Newsales_SR, xlab="Year",ylab="Newsales_SR")
lines(Newsales_SR)

basicStats(HPI_SD)

cor(HPI_SD_Diff2,CCI_SD_Lag2)

#Multivariate OLS Regression: 
lm1 <- lm(HPI_SD ~ CCI_SD + FCFI_SD +  Unem_SD + Newsales_SD + Creditloan_SD, data=d)
summary(lm1)

fit123 <- fitted(lm1)
plot(fitted(lm1),HPI_SD)

test_data2 <- data.frame(
  var0 = fit123,
  var1 = HPI_SD,
  date = seq.Date(as.Date("1978-01-01"), by="1 month", length.out=437))

ggplot(test_data, aes(date)) + 
  geom_line(aes(y = var0, colour = "fitted(lm1)")) + 
  geom_line(aes(y = var1, colour = "HPI_SD"))

resid.lm1 <- resid(lm1)
plot(d$HPI_SD, resid.lm1, ylab="Residuals", xlab="CCI_SD", main="Residuals of OLS Regression on HPI_SD") 
abline(0, 0)

acf(resid.lm1, main="ACF of lm1 Residuals")

#Multivariate OLS regression with lagged variables

HPI <-ts(d2$HPI,start=c(1979,1), end=c(2013,1), frequency=12)
CCI_Lead3 <-ts(d2$CCI_Lead3,start=c(1978,10), end=c(2012,10), frequency=12)
CCI_Lead6 <-ts(d2$CCI_Lead6,start=c(1978,7), end=c(2012,7), frequency=12)
CCI_Lead9 <-ts(d2$CCI_Lead9,start=c(1978,4), end=c(2012,4), frequency=12)
CCI_Lead12 <-ts(d2$CCI_Lead12,start=c(1978,1), end=c(2012,1), frequency=12)
CCI_Lag3 <-ts(d2$CCI_Lag3,start=c(1979,4), end=c(2013,4), frequency=12)
CCI_Lag6 <-ts(d2$CCI_Lag6,start=c(1979,7), end=c(2013,7), frequency=12)
CCI_Lag9 <-ts(d2$CCI_Lag9,start=c(1979,10), end=c(2013,10), frequency=12)
CCI_Lag12 <-ts(d2$CCI_Lag12,start=c(1980,1), end=c(2014,1), frequency=12)
FCFI <-ts(d2$FCFI,start=c(1979,1), end=c(2013,1), frequency=12)
Creditloan <-ts(d2$Creditloan,start=c(1979,1), end=c(2013,1), frequency=12)
HouseSt <-ts(d2$HouseSt,start=c(1979,1), end=c(2013,1), frequency=12)
HousePer <-ts(d2$HousePer,start=c(1979,1), end=c(2013,1), frequency=12)
Unem <-ts(d2$Unem,start=c(1979,1), end=c(2013,1), frequency=12)
Newsales <-ts(d2$Newsales,start=c(1979,1), end=c(2013,1), frequency=12)

lm10<- lm(HPI ~ CCI_Lag12 + FCFI + HouseSt + HousePer + Unem + Newsales + Creditloan, data=d2)
summary(lm10)


resid.lm1 <- resid(lm1)
plot(d$HPI, resid.lm1, ylab="Residuals", xlab="CCI_SD", main="Residuals of OLS Regression on HPI") 
abline(0, 0)

acf(resid.lm1, main="ACF of lm1 Residuals")


HPI_SD_Diff1 <- diff(HPI_SD, differences=1)
CCI_SD_Diff1 <- diff(CCI_SD, differences=1)
Creditloan_SD_Diff1 <-diff(Creditloan_SD, difference=1)
HouseSt_SD_Diff1 <- diff(HouseSt_SD, differences=1)
FCFI_SD_Diff1 <- diff(FCFI_SD, differences=1)
Unem_SD_Diff1 <- diff(Unem_SD, differences=1)
HousePer_SD_Diff1 <- diff(HousePer_SD, differences=1)
Newsales_SD_Diff1 <- diff(Newsales_SD, differences=1)
Year_SD_Diff1 <- diff(Year_SD, differences=1)
HPI_W_SD_Diff1<- diff(HPI_W_SD, differences=1)
HPI_S_SD_Diff1<- diff(HPI_S_SD, differences=1)
HPI_MW_SD_Diff1<- diff(HPI_MW_SD, differences=1)
HPI_NE_SD_Diff1<- diff(HPI_NE_SD, differences=1)


HPI_SD_Diff2 <- diff(HPI_SD, differences=2)
CCI_SD_Diff2 <- diff(CCI_SD, differences=2)
Creditloan_SD_Diff2 <-diff(Creditloan_SD, difference=2)
HouseSt_SD_Diff2 <- diff(HouseSt_SD, differences=2)
FCFI_SD_Diff2 <- diff(FCFI_SD, differences=2)
Unem_SD_Diff2 <- diff(Unem_SD, differences=2)
HousePer_SD_Diff2 <- diff(HousePer_SD, differences=2)
Newsales_SD_Diff2 <- diff(Newsales_SD, differences=2)
Year_SD_Diff2 <- diff(Year_SD, differences=2)
HPI_W_SD_Diff2<- diff(HPI_W_SD, differences=2)
HPI_S_SD_Diff2<- diff(HPI_S_SD, differences=2)
HPI_MW_SD_Diff2<- diff(HPI_MW_SD, differences=2)
HPI_NE_SD_Diff2<- diff(HPI_NE_SD, differences=2)


lm11 <- lm(HPI_SD_Diff1 ~ CCI_SD_Diff1 + FCFI_SD_Diff1 + HouseSt_SD_Diff1 + HousePer_SD_Diff1 + Unem_SD_Diff1 + Newsales_SD_Diff1 + Creditloan_SD_Diff1, data=d)
summary(lm11)
resid.lm11<- resid(lm11)
plot(d$HPI_SD_Diff1, resid.lm11, ylab="Residuals", xlab="CCI_SD", main="Residuals of First Difference Models on HPI_SD") 
abline(0, 0)

lm3 <- lm(HPI_SD_Diff2 ~CCI_SD_Diff2 + FCFI_SD_Diff2 + HouseSt_SD_Diff2 + HousePer_SD_Diff2 + Unem_SD_Diff2 + Newsales_SD_Diff2 + Creditloan_SD_Diff2, data=d)
summary(lm3)


#ARIMA Models
   #Difference the HPI_SD once and testing for stationarity

plot.ts(HPI_SD_Diff1)
acf(HPI_SD_Diff1)
pacf(HPI_SD_Diff1)

plot.ts(HPI_SD_Diff2)
acf(HPI_SD_Diff2)
pacf(HPI_SD_Diff2)

Box.test(HPI_SD, lag = 20, type = "Ljung-Box")
Box.test(HPI_SD_Diff1,lag=20,type="Ljung-Box")
Box.test(HPI_SD_Diff2,lag=20,type="Ljung-Box")

adf.test(HPI_SD,alternative="stationary")
adf.test(HPI_SD_Diff1,alternative="stationary")
adf.test(HPI_SD_Diff2,alternative="stationary")

kpss.test(HPI_SD)
kpss.test(HPI_SD_Diff1)
kpss.test(HPI_SD_Diff2)

#Selecting the ARIMA model parameters, where d=2

acf(HPI_SD_Diff2, lag.max = 20)
acf(HPI_SD_Diff2, lag.max = 20,plot=FALSE)
pacf(HPI_SD_Diff2, lag.max = 20)
pacf(HPI_SD_Diff2, lag.max = 20,plot = FALSE)

plot.ts(HPI_SD_Diff2)
plot.ts(HPI_SD_Diff1)
plot.ts(HPI_SD_Diff3)

auto.arima(HPI_SD_Diff2, approximation = FALSE,stepwise = FALSE)
auto.arima(HPI_SD, approximation = FALSE,stepwise = FALSE)
auto.arima(HPI_SD_Diff3, approximation = FALSE,stepwise = FALSE)
auto.arima(HPI_W_SD_Diff2, approximation = FALSE,stepwise = FALSE)
auto.arima(HPI_MW_SD_Diff2, approximation = FALSE,stepwise = FALSE)
auto.arima(HPI_NE_SD_Diff2, approximation = FALSE,stepwise = FALSE)
auto.arima(HPI_S_SD_Diff2, approximation = FALSE,stepwise = FALSE)

fit2<-Arima(HPI_SD_Diff2, order = c(0,0,2), seasonal = c(1,0,1))
plot(forecast(fit2, h=60))

acf(fit2$residuals,lag.max = 20)
Box.test(fit2$residuals,lag=20,type = "Ljung-Box")

fit3<-Arima(HPI_SD, order = c(3,1,2))
plot(forecast(fit3, h=60))

fit4<-Arima(HPI_SD_Diff3, order = c(4,0,0), seasonal = c(1,0,0),lambda =0)
Box.test(fit4$residuals,lag=20,type = "Ljung-Box")
acf(fit4$residuals,lag.max = 20)

fit5<-Arima(HPI_W_SD_Diff2, order = c(4,0,0), seasonal = c(1,0,0))
plot(forecast(fit5, h=72), main="6 Year Forecast of HPI_W_SD_Diff2")
plot.ts(fit5$residuals)
HPI_W_SD_Diff2_forecasts <- HoltWinters(HPI_W_SD_Diff2, gamma=FALSE)
plot(HPI_W_SD_Diff2_forecasts)

fit6<-Arima(HPI_W_SD, order = c(4,0,0),seasonal = c(1,0,0))
plot(forecast(fit6, h=72),main="6 Year Forecast of HPI_W_SD")

fit7<-Arima(HPI_MW_SD_Diff2, order = c(2,0,2), seasonal = c(0,0,1))
plot(forecast(fit7, h=72), main="6 Year Forecast of HPI_MW_SD_Diff2")

fit8<-Arima(HPI_MW_SD, order = c(2,0,2),seasonal = c(0,0,1))
plot(forecast(fit8, h=72),main="6 Year Forecast of HPI_MW_SD")

fit9<-Arima(HPI_NE_SD_Diff2, order = c(2,0,2), seasonal = c(0,0,1))
plot(forecast(fit9, h=72), main="6 Year Forecast of HPI_NE_SD_Diff2")

fit10<-Arima(HPI_NE_SD, order = c(2,0,2),seasonal = c(0,0,1))
plot(forecast(fit10, h=72),main="6 Year Forecast of HPI_NE_SD")

fit11<-Arima(HPI_S_SD_Diff2, order = c(4,0,0), seasonal = c(1,0,0))
plot(forecast(fit11, h=72), main="6 Year Forecast of HPI_S_SD_Diff2")

fit12<-Arima(HPI_S_SD, order = c(4,0,0),seasonal = c(1,0,0))
plot(forecast(fit12, h=72),main="6 Year Forecast of HPI_S_SD")

#Holt Winters Exponential Smoothing Forecasts on Regional HPIs
HPI_SD_forecasts <- HoltWinters(HPI_SD, gamma=FALSE)
HPI_SD_forecast <- forecast.HoltWinters(HPI_SD_forecasts, h=60)
plot.forecast(HPI_SD_forecast)

HPI_W_SD_forecasts <- HoltWinters(HPI_W_SD, gamma=FALSE)
HPI_W_SD_forecast <- forecast.HoltWinters(HPI_W_SD_forecasts, h=72)
plot.forecast(HPI_W_SD_forecast)

HPI_S_SD_forecasts <- HoltWinters(HPI_S_SD, gamma=FALSE)
HPI_S_SD_forecast <- forecast.HoltWinters(HPI_S_SD_forecasts, h=72)
plot.forecast(HPI_S_SD_forecast)

HPI_NE_SD_forecasts <- HoltWinters(HPI_NE_SD, gamma=FALSE)
HPI_NE_SD_forecast <- forecast.HoltWinters(HPI_NE_SD_forecasts, h=72)
plot.forecast(HPI_NE_SD_forecast)

HPI_MW_SD_forecasts <- HoltWinters(HPI_MW_SD, gamma=FALSE)
HPI_MW_SD_forecast <- forecast.HoltWinters(HPI_MW_SD_forecasts, h=72)
plot.forecast(HPI_MW_SD_forecast)



dd<-mtcars[c(HPI_SD,CCI_SD,FCFI_SD,HouseSt_SD)]
dd.r<-abs(cor(dd))
dd.col<-dmat.color(dd.r)
dd.o<-order.single(dd.r)
cpairs(dd,dd.o,panel.colors=dd.col, gap=0.05, main="Main")

HPIdecom <- decompose(HPI_SD)
plot(HPIdecom)

HPIdecom <- decompose(HPI_SD_Diff1)
plot(HPIdecom)

HPIdecom_Diff3 <- decompose(HPI_SD_Diff3)
plot(HPIdecom_Diff3)

CCIdecom <- decompose(CCI_SD)
plot(CCIdecom)

CCIdecom <- decompose(CCI_SR)
plot(CCIdecom)

HPI_SD_fore <- HoltWinters(HPI_SD, beta=FALSE, gamma=FALSE)
HPI_SD_fore

fit1 <- ses(HPI_SD,alpha=1, initial ="simple", h=3)
plot(fit1, plot.conf=FALSE, ylab="HPI", xlab="Year", main="",fcol="white", type="o")
lines(fitted(fit1),col="blue",type="o")

summary(ur.df(HPI_SD))

