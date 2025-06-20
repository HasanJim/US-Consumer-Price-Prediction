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


head(cpi)

extrafont::loadfonts()
