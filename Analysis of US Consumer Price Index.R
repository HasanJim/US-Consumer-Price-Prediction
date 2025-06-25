library(tidyverse)
library(tseries)
library(forecast)
data(package = "tseries")
data(NelPlo)
cpi
cpi <- ts(cpi,start = 1860, end = 1988)
plot(cpi)
autoplot(cpi, linewidth = 1)+
  labs(y = "Consumer Price Index",caption = "Source: C. R. Nelson and C. I. Plosser (1982),\n Trends and Random Walks in Macroeconomic Time Series. \n Journal of Monetary Economics, 10, 139–162.\n doi:10.1016/0304-3932(82)90012-5.")+
  scale_x_continuous(breaks= c(1860, 1900, 1950, 1988))+
  theme_classic()+
  theme(text = element_text(size = 20, family = "Times New Roman"),
        plot.caption = element_text(size = 7))

autoplot(diff(cpi), linewidth = 1)+
  labs(y = expression(Delta * "Consumer Price Index"),
                 caption = "Source: C. R. Nelson and C. I. Plosser (1982),\n Trends and Random Walks in Macroeconomic Time Series. \n Journal of Monetary Economics, 10, 139–162.\n doi:10.1016/0304-3932(82)90012-5.")+
  scale_x_continuous(breaks= c(1860, 1900, 1950, 1988))+
  theme_classic()+
  theme(text = element_text(size = 20, family = "Times New Roman"),
        plot.caption = element_text(size = 7))

Acf(cpi)
acf(cpi)
Pacf(cpi)

head(cpi)

extrafont::loadfonts()



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
