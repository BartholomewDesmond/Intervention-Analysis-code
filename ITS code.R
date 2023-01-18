library(ggplot2)
library(forecast)
library(astsa)
library(lmtest)
library(fUnitRoots)
library(FitARMA)
library(strucchange)
library(reshape)
library(Rmisc)
library(fBasics)
library(readxl)
library(tseries)
#Input the data into R
mydata <- read_excel("covid19.xlsx", sheet =2)
mydata
library(ggpubr)
ggqqplot(mydata$Cases)
shapiro.test(mydata$Cases)
#number of rows of my data
nrow(mydata)
head(mydata)
tail(mydata)
autoplot(ts(mydata$Cases, frequency = 1, start = mydata$Day[1]))
#Convert data to time series
mydata_ts <- ts(mydata$Cases, frequency = 1, start = mydata$Day[1])
mydata_ts
head(mydata[,4:8])
basicStats(mydata[,4:8])
# defining start date
date <- as.Date("2020/02/28")
# defining length of range 
len <- 752
# generating range of dates
Date <- seq(date, by = "day", length.out = len)
length(Date)
length(mydata_ts)
data <- data.frame(mydata_ts,Date)
plot(data, ylim = c(0,4500))
ggplot(data,aes(Date, mydata_ts)) +
  geom_point(na.rm=TRUE, color="black", size=1, pch=18)+ 

  xlab("Day") + ylab("Daily COVID-19 Cases")
autoplot(mydata_ts, ylab = "Nigerian COVID-19 cases", xlab = "Day")
autoplot(diff(mydata_ts))
#stationarity test
adf.test(mydata_ts)
dim(mydata)
adf.test(diff(mydata_ts))
acf(diff(mydata_ts), lag =96, main =" ")
pacf(diff(mydata_ts), lag = 96, main = " ")
#Let us verify regression against a constant to investigate structural change
summary(lm(mydata_ts ~ 1))
#To check if there is any structural break
(break_point <- breakpoints(mydata_ts ~ 1))
plot(break_point)
summary(break_point)
#plot the original time series against its structural break and its confidence interval.
plot(mydata_ts, xlab = "Days", ylab ="Number of COVID19 cases")
lines(fitted(break_point, breaks = 1), col = 4)
print(confint(break_point, breaks = 1))
lines(confint(break_point, breaks = 1))
#covid19 cases changed from 
round(fitted(break_point)[1],0)
#To
round(fitted(break_point)[length(mydata_ts)],0)
#Running a t.test() to verify further the difference 
#in mean is significative across the two time windows 
#identified by the breakpoint date, 383 (Mar, 16 2021)
break_date <- breakdates(break_point)
break_date <- 372
win_1 <- window(mydata_ts, end = break_date)
win_2 <- window(mydata_ts, start = break_date + 1)
t.test(win_1, win_2)
#Model identification
model <- auto.arima(mydata_ts,stepwise = FALSE, trace = TRUE)
(auto.arima(mydata_ts))
summary(model)
checkresiduals(model)
Model1_summary <- cbind(AIC =model$aic, loglik=model$loglik)
xreg <- mydata[,5:8]
mydata_ts
dfTS<-as.matrix(ts(xreg))
new<-Arima(mydata_ts, order = c(4,1,4), xreg = dfTS)
summary(new)
Covid_fcast <- predict(new, h=752, newxreg = dfTS)
xreg2 <- read_excel("covid19.xlsx", sheet =6)
xreg2 <- as.matrix(ts(xreg2))
Covid_fcast <- predict(new, newxreg = xreg2)
Test_set1 <- read_excel("covid19.xlsx", sheet =9)
Test_set<-as.data.frame(Test_set1)
library(tidyr)
DateTime1 = seq(from=as.Date("2021-03-21"), to=as.Date("2023-03-31"), by="months")
DateTime = Date
a=data.frame(Test_set,DateTime)
dfplot <- a %>% gather(key, value, -DateTime)
k=ggplot(dfplot, mapping = aes(x = DateTime, y = value, color = key) ) +
  geom_line()+ labs(x=("Months (Mar. 2021 - Mar. 2023)"), y=" Daily COVID-19 cases")
k

# defining start date
date <- as.Date("2022/03/21")
# defining length of range 
len <- 376
# generating range of dates
Date <- seq(date, by = "day", length.out = len)
length(Date)
length(Covid_fcast)
data <- data.frame(Test_set1,Date)
plot(Test_set1, ylim = c(0,500))
ggplot(data,aes(Date, Test_set1)) +
  geom_point(na.rm=TRUE, color="blue", size=1, pch=18)+ 
  ggtitle("Forecast using ARIMA (4,1,4) with exogeneous variables") +
  xlab("Day") + ylab("Daily COVID-19 Cases")

df$Date <- as.Date( df$Date, '%m/%d/%Y')
require(ggplot2)
ggplot( data = df, aes(Date, Visits )) + geom_line() 
axis(1, mydata$Date, format(mydata$Date, "%b %d"), cex.axis = .7)

auto.arima(mydata_ts,order = c(4,1,4),seasonal=TRUE,xreg=dfTS)

model2 <- arima(mydata_ts, c(4,1,4))
checkresiduals(model2)
summary(model2)
BIC(model2)
Model2_summary <- cbind(AIC =model2$aic, loglik=model2$loglik)
model3 <- arima(mydata_ts, c(2,2,2))
summary(model3)
checkresiduals(model3)
Model3_summary <- cbind(AIC =model3$aic, loglik=model3$loglik)
model4 <- arima (mydata_ts, c(1,1,4))
summary(model4)
Model4_summary <- cbind(AIC =model4$aic, loglik=model4$loglik)
rbind(Model1_summary,Model2_summary,Model3_summary,Model4_summary)
#checking model residuals for model 1  & model 2
checkresiduals(model)
checkresiduals(model2)
checkresiduals(model3)
checkresiduals(model4)
#To check for the presence of seasonality
library(stats)
Box.test(residuals(model2), lag = 14)
#Checking the Ljungbox shows that there is a significant spike
#at lag 7 and 14, indicating a seasonal variations
sarima(mydata_ts, p = 1, d = 1, q = 2, P = 0, D = 1, Q = 1, S = 4)
sarima(mydata_ts, p = 0, d = 1, q = 2, P = 0, D = 1, Q = 1, S = 4)
sarima(mydata_ts, p = 0, d = 1, q = 1, P = 1, D = 1, Q = 2, S = 4)
sarima(mydata_ts, p = 0, d = 1, q = 4, P = 0, D = 1, Q = 0, S = 7)
model_1 <- Arima(mydata_ts, order = c(1,1,2), 
                 seasonal = list(order = c(0,1,1), period = 4), 
                 include.mean = TRUE)
summary(model_1)
model_2 <- Arima(mydata_ts, order = c(0,1,2), 
                 seasonal = list(order = c(0,1,1), period = 4), 
                 include.mean = TRUE)
summary(model_2)
model_3 <- Arima(mydata_ts, order = c(0,1,1), 
                 seasonal = list(order = c(1,1,2), period = 4), 
                 include.mean = TRUE)
summary(model_3)
model_4 <- Arima(mydata_ts, order = c(2,1,1), 
                 seasonal = list(order = c(0,1,1), period = 4), 
                 include.mean = TRUE)
summary(model_4)
Box.test(residuals(model_1), lag = 12)
Box.test(residuals(model_2), lag = 8)
Box.test(residuals(model_3), lag = 8)
Box.test(residuals(model_4), lag = 8)
#model that takes care of the level shift and the seasonal 
#component at lag 4
break_point$breakpoints <-383
level <- c(rep(0, break_point$breakpoints), 
           rep(1, length(mydata_ts) - break_point$breakpoints))
model_5 <- Arima(mydata_ts, order = c(1,1,2), 
                 seasonal = list(order = c(0,1,1), period = 4), 
                 xreg = level, include.mean = TRUE)
summary(model_5)
checkresiduals(model_5)
Box.test(residuals(model_2), lag = 14)
model_6 <- Arima(mydata_ts, order = c(0,1,2), 
                 seasonal = list(order = c(0,1,1), period = 4), 
                 xreg = level, include.mean = TRUE)
summary(model_6)
checkresiduals(model_6)
Box.test(residuals(model_6), lag = 14)

#summarizing the whole models
df <- data.frame(col_1_res = c(model$aic, model2$aic, model3$aic, model4$aic,
                               model_1$aic,model_2$aic,model_3$aic,model_4$aic,
                               model_5$aic,model_6$aic),
                 col_2_res = c(model$aicc, " ", " ", " ",
                               model_1$aicc,model_2$aicc,model_3$aicc,model_4$aicc,
                               model_5$aicc,model_6$aicc),
                 col_3_res = c(" ", model2$loglik, model3$loglik, model4$loglik,
                               model_1$loglik,model_2$loglik,model_3$loglik,model_4$loglik,
                               model_5$loglik,model_6$loglik))
colnames(df) <- c("AIC", "AICc", "loglik")
rownames(df) <- c("ARIMA(2,1,3) with drift",
                  "ARIMA(1,1,2",
                  "ARIMA(2,1,2)",
                  "ARIMA(1,1,4",
                  "ARIMA(1,1,2)(0,1,1)[4]", 
                  "ARIMA(0,1,2)(0,1,1)[4]", 
                  "ARIMA(0,1,1)(1,1,2)[4]", 
                  "ARIMA(2,1,1)(0,1,1)[4]",
                  "ARIMA(1,1,2)(0,1,1)[4] with level shift",
                  "ARIMA(0,1,2)(0,1,1)[4] with level shift")

df
#Forecasting with the best model
h_fut <- 382
plot(forecast(model_6, h = h_fut, xreg = rep(1, h_fut)))

fitted.ts <- fitted(break_point, breaks = 3)
autoplot(fitted.ts)
names(mydata)
xregdata <- as.matrix(mydata[4:6])
mydata_xreg <- Arima(mydata[,3], order = c(0,1,2), 
                     seasonal = list(order = c(0,1,1), period = 4), 
                     xreg = as.matrix(mydata[,4]), include.mean = F)
summary(mydata_xreg)
coeftest(mydata_xreg)
checkresiduals(mydata_xreg)

dataforecast<-forecast(model2, 90)
sink("New forecast.txt")
dataforecast
sink()
