library(tidyverse)
library(tseries)
library(forecast)
library(urca)
data(package = "tseries")
data(NelPlo, package = "tseries")
cpi # it's part of NelPlo

cpi <- ts(cpi,start = 1860, end = 1988)

cpi.train <- window(cpi, end = 1980)
cpi.test <- window(cpi, start = 1981)

autoplot(cpi.train, linewidth = 1)+
  labs(tittle = "Consumer Price Index: Training Data",
       y= "",
       caption = "Source: C. R. Nelson and C. I. Plosser (1982),\n Trends and Random Walks in Macroeconomic Time Series. \n Journal of Monetary Economics, 10, 139–162.\n doi:10.1016/0304-3932(82)90012-5.")+
  scale_x_continuous(breaks= c(1860, 1900, 1950, 1988))+
  theme_classic()+
  theme(text = element_text(size = 20, family = "serif"),
        plot.caption = element_text(size = 7))



autoplot(diff(cpi.train), linewidth = 1)+
  labs(y = expression(Delta * "Consumer Price Index"),
                 caption = "Source: C. R. Nelson and C. I. Plosser (1982),\n Trends and Random Walks in Macroeconomic Time Series. \n Journal of Monetary Economics, 10, 139–162.\n doi:10.1016/0304-3932(82)90012-5.")+
  scale_x_continuous(breaks= c(1860, 1900, 1950, 1988))+
  theme_classic()+
  theme(text = element_text(size = 20, family = "serif"),
        plot.caption = element_text(size = 7))


autoplot(diff(cpi.train,2), linewidth = 1)+
  labs(y = expression(Delta^2 * "Consumer Price Index"),
                 caption = "Source: C. R. Nelson and C. I. Plosser (1982),\n Trends and Random Walks in Macroeconomic Time Series. \n Journal of Monetary Economics, 10, 139–162.\n doi:10.1016/0304-3932(82)90012-5.")+
  scale_x_continuous(breaks= c(1860, 1900, 1950, 1988))+
  theme_classic()+
  theme(text = element_text(size = 20, family = "serif"),
        plot.caption = element_text(size = 7))


Acf(cpi.train)

Pacf(cpi.train)

urca::ur.df(cpi.train,type = "drift") |>summary()


names(sm)
urca::ur.kpss(cpi.train, type = "tau") |> summary()

Acf(diff(cpi.train))

Pacf(diff(cpi.train))



urca::ur.df(diff(cpi.train),type = "none") |>summary()

urca::ur.kpss(diff(cpi.train,1), type = "mu") |> summary()
urca::ur.kpss(diff(cpi.train,2), type = "mu") |> summary()




arima.fit1 <- auto.arima(cpi.train, seasonal = F, PI = F)

f8 <- arima.fit1 %>% forecast(h = 8)

autoplot(cpi, color = "darkgreen", size =1, series = "CPI")+
  autolayer(f8, PI =F, color = "darkred", size = 1, series = "Arima Prediction")+
  theme_classic()

# simple exponential smoothing


ex.mod1 <- ses(cpi, alpha = 0.9, h = 10, initial = "optimal")
ex.mod1 |> autoplot()
ex.mod1
ex.mod2 <- ses(diff(cpi), alpha = 0.1, h = 10)
ex.mod2
ex.mod2 |> autoplot()
accuracy(ex.mod2) |> round(2)


lmd <- BoxCox.lambda(ap2)
autoplot(BoxCox(ap2, lmd))

plot(log(ap2))

autoplot(cpi)+
  # autolayer(fitted(ex.mod1), linewidth = 1)+
  autolayer(ex.mod2, PI = F, size = 1)+
  autolayer(fitted(ex.mod2), color = "red", linewidth = 1)

ex.mod2 <- holt(cpi, h = 10)

hw1  <- hw(ap2, damped = T, seasonal = "mult")
autoplot(hw1)
accuracy(hw1)
plot(hw1)
geom_forecast()


ex.mod3 <- holt(ap2, h =5)
ex.mod3 |> autoplot()
ets()


# 



cpi.fit.holt |> names()


ap <- ts(AirPassengers,frequency = 12)

ap
ggseasonplot(ap, polar = T) +theme_classic()
ggsubseriesplot(ap) +theme_classic()

agap <- aggregate(ap, nfrequency = 12)

boxplot(ap~cycle(ap))
cycle(ap)
agap

dim(agap)
boxplot(agap)


gglagplot(ap)
gglagplot(cpi)

shapiro.test(residuals(ex.mod1))
shapiro.test(residuals(ex.mod2))
checkresiduals(residuals(ex.mod1))

ggPacf(cpi)
ggAcf(cpi, lag =40)
ggAcf(diff(cpi))


ggPacf(ap)
ggAcf(ap,lag.max = 48)

apd <- cbind(monthly = ap, 
             avap = ap/monthdays(ap))
apd

autoplot(apd, facet = T)

apd[,2]
ggseasonplot(apd["avap"])
ap2 <- ts(apd[,2], freq = 12)

ap2
monthdays(ap)
autoplot(cpi)+
  autolayer(meanf(cpi, h = 5), PI = F)+
  autolayer(rwf(cpi, h = 5), PI = F)
  autolayer(snaive(cpi, h = 5), PI = F)

########
ap2
ap23 <- window(ap2, end = 10)
ap23
  
ap23fit1 <- hw(ap23, seasonal = "mult")   

autoplot(ap2)+
  autolayer(ap23fit1, PI = F, color = 'red')

## acuracy test testing.
accuracy(ap23fit1, window(ap2, start = 10)) |> round(1) 
data(package = "forecast")
data("gold")
autoplot(gold)

## cross-validation
er <- tsCV(cpi, rwf, drift = T, h = 1)
er
accuracy(er)

sqrt(mean(er^2, na.rm =T))
sqrt(mean(residuals(rwf(cpi, drift = T, h = 1)), na.rm =T))


cpi.fit1 <- tslm(ap2~ trend+ season)

autoplot(ap2)+
  autolayer(fitted(cpi.fit1))
plot(cpi)
plot(diff(cpi))
cpi %>% ur.df(type = "none") %>% summary()
cpi %>% ur.df(type = "drift") %>% summary()
cpi %>% ur.df(type = "trend") %>% summary()

diff(cpi) %>% ur.df(type = "none") %>% summary()
diff(cpi) %>% ur.df(type = "drift") %>% summary()
diff(cpi) %>% ur.df(type = "trend") %>% summary()
diff(cpi) %>% ur.df() %>% summary()
diff(cpi) %>% ur.df() %>% summary()
diff(cpi) %>% ur.kpss(type = "tau") %>% summary()

