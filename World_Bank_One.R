remove(list = ls());
ls(); 

###  World Bank API package

# if(require("WDI", "ggplot2", "scales", 
#            "useful", "forecast")) {
#   message('Packages loaded correctly')
# } else {
#   install.packages("WDI", "ggplot2", "scales", 
#                    "useful", "forecast")
# }

library(WDI); #install.packages("WDI")
library(ggplot2); #install.packages("ggplot2")
library(scales); #install.packages("scales")
library(useful); #install.packages("useful")
library(forecast); #install.packages("forecast")
library(reshape2); #install.packages("reshape2")

# CountryCheck <- WDI(country = "all", start = 2005, end = 2006)
# 
# head(CountryCheck); str(CountryCheck)

#County Codes: 

# Latin Ameria (Some of them)
#         ("VE", "UY", "PE", "PY", "PA", "MX", 
#          "HN", "GT", "SV", "EC", "CU", "CR", 
#          "CO", "CL", "BR", "BO", "BZ", "AR")

# Africa: ("BWA", "CAF", "CIV", "CMR", "COG", "ETH", 
#         "KEN", "MLI", "MOZ", "MRT", "MWI", "NAM", 
#         "NER", "NGA", "RWA", "SDN", "SEN", "SLE", 
#         "SOM", "SSD", "SWZ", "TCD", "UGA", "ZAF", 
#         "COD", "ZMB", "ZWE")

# Latin America
GDP <- WDI(country = c("AR","BR","CO","EC","MX","UY","VE"),
           indicator = c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"),
           start = 1962, end = 2012)

########################################
################ ARMA ##################
########################################

### Prelim Info
names(GDP); str(GDP); head(GDP); tail(GDP)

###  GDP per Capita by Country ###
# ggplot.  Newer academic plots 
# First Plot for DomoR
ggplot(GDP, aes(year, NY.GDP.PCAP.CD, color = country, linetype = country)) +
  geom_line() +
  ggtitle("GDP per Capita by Country") +
  labs(x = "Year", y = "GDP per Capita")
  #scale_y_continuous(label=dollar)

###  Total GDP by Country  ###

#ggplot(GDP, aes(year, NY.GDP.MKTP.CD, color = country, linetype = country)) +
#  geom_line() +
#  ggtitle("GDP by Country") +
#  labs(x = "Year", y = "GDP by Country")
#scale_y_continuous(label=dollar)

###  First, let's check out ARMA models
# Examine One country.  Let's look at Mexico.  

Mexico <- GDP$NY.GDP.PCAP.CD[GDP$country == "Mexico"] #str(Mexico);

# Let's Convert to a time series

Mexico <- ts(Mexico, start = min(GDP$year), end = max(GDP$year)); #str(Mexico)

#Classic Academia Base Plot
# Second Plot for DomoR
plot(Mexico, ylab = "Per Capita GDP", xlab = "Year", main = "GDP Per Capita by Year")

### Autocovariance Function ###
# correlation of times series with with lags of itself.  Thus, how much
# the times series is correlated itself with one lag, two lags, etc
acf(Mexico); #acf

## Lags with Vertical bar above the horizontal indicate that 
# autocorrelation is significant at those points 

### Partal Autocovariance Function ###
# Amount of correlation between time series and lags of it self that
# that is not explained by a previous lag
pacf(Mexico)

###  Using Forecast package to figure out number of Differences to use
# rather than doing that process by hand
# The Time Series needs transformation because it is NOT stationary*
# Diffing the series is one way to control for that problem

ndiffs(x = Mexico)

plot(diff(Mexico, 1), main = "Differences over Time")

MexicoBest <- auto.arima(x = Mexico)

acf(MexicoBest$residuals)
pacf(MexicoBest$residuals)

coef(MexicoBest)

predict(MexicoBest, n.ahead = 5, se.fit = TRUE)

forecast(object = MexicoBest, h = 5)

# Third Plot for DomoR
plot(forecast(object = MexicoBest, h = 5))


#########################################
###  VAR Vector Autoregressive Model  ###
#########################################
#str(GDP)

GDPcast <- dcast(year ~ country,
                 data = GDP[,c("country", "year", "NY.GDP.PCAP.CD")],
                          value.var = "NY.GDP.PCAP.CD")
#head(GDPcast); tail(GDPcast)

GDPTS <- ts(data = GDPcast[ , -1], start=min(GDPcast$year), 
            end=max(GDPcast$year))

str(GDPTS)

# GDP by country Base Plot example
plot(GDPTS, plot.type="single", col=1:8)
legend("topleft", legend = colnames(GDPTS), ncol=2, lty = 1, col=1:8, cex = .9)

# Check data frame (and subsequently the columns) for NA's.  If they exist, remove them.
sum(is.na(GDPTS))

numDiffs <- ndiffs(GDPTS)


GDPDiffed <- diff(GDPTS, differences = numDiffs)
plot(GDPDiffed, plot.type = "single", col=1:7)

##  Model is going highwire  ##  Revist at later date

########################################
################ GARCH #################
########################################

#  ARMA does not handle extreme events or high volatility well  #
#  so we can use Generalized Autoregressive Conditional Heteroskedascity  # 
#  model for the variance GARCH(m,s) 

library("quantmod"); #install.packages("quantmod")
library(xts)


### Let's fit the model to a GARCH using rugarch package

###  rugarch does not seems to not work on Mac OS 
library("rugarch") #install.packages("rugarch")
require("rugarch")
# volatility to be modeled as a GARCH(1,1) and mean as ARMA(1,1)

Spec <- ugarchspec(variance.model=list(model="sGARCH",
                                          garchOrder=c(1,1)),
                      mean.model=list(armaOrder=c(1,1)),
                      distribution.model="std")

Garch <- ugarchfit(spec = attSpec, data = attClose)

#library(jsonlite); library(curl)

#WB <- fromJSON("http://api.worldbank.org/countries/ZMB/indicators/NY.GDP.PCAP.CD?per_page=100&date=1960:2016&format=json") 
#str(WB)

###  Note R does NOT want to name columns of a Data Frame with Numbers.  It will be okay if the 
###  data structure is a matrix

