
install.packages("forecast")
library(dplyr)
library(lubridate)
library(xts)
library(highcharter) # check it out! pretty fancy!
library(ggplot2)
library(plotly)
library(prophet)
library(forcast)
#### OR568 Final---------
crime <- read.csv("crimes-chicago/Chicago_Crimes_2012_to_2017.csv")
#### Cleaning data format-------
crime$Date <- as.Date(crime$Date, "%m/%d/%Y %I:%M:%S %p")
crime$Day <- factor(day(as.POSIXlt(crime$Date, format="%m/%d/%Y %I:%M:%S %p")))
crime$Month <- factor(month(as.POSIXlt(crime$Date, format="%m/%d/%Y %I:%M:%S %p")))
crime$Year <- factor(year(as.POSIXlt(crime$Date, format="%m/%d/%Y %I:%M:%S %p")))
crime$Weekday <- factor(wday(as.POSIXlt(crime$Date, format="%m/%d/%Y %I:%M:%S %p")))

# subset robbery & theft data---------
robbery <- crime[grep("ROBBERY", crime$Primary.Type), ]
theft <- crime[grep("THEFT", crime$Primary.Type), ]

#### timeseries 
robbery_Date <- na.omit(robbery) %>% group_by(Date) %>% summarise(Total = n())
robbery_tseries <- xts(robbery_Date$Total, order.by=as.POSIXct(robbery_Date$Date))

theft_Date <- na.omit(theft) %>% group_by(Date) %>% summarise(Total = n())
theft_tseries <- xts(theft_Date$Total, order.by=as.POSIXct(theft_Date$Date))

#### visualization
hchart(robbery_tseries, name = "Robbery") %>% 
  hc_add_series(theft_tseries, name = "Theft") %>%
  hc_add_theme(hc_theme_sandsignika()) %>%
  hc_credits(enabled = TRUE, text = "OR568 Final Chicago Crime", style = list(fontSize = "12px")) %>%
  hc_title(text = "Times Series plot of Chicago Crimes of Robbery & Theft") %>%
  hc_legend(enabled = TRUE)


lag.plot(robbery_tseries, lags=9, do.lines=FALSE)
Acf(robbery_tseries)
x <- acf(diff(robbery_tseries), plot = FALSE)
plot(x)
#### Forecasting for the next four years-------
robery_forecast <- window(robbery_tseries,start=2006-01-01,end=2006-12-31)
robbery_forecast1 <- meanf(robbery_tseries, h=12)
robbery_forecast2 <- naive(robbery_tseries, h=12)
robbery_forecast3 <- snaive(robbery_tseries, h=365)

test <- auto.arima(robbery_tseries)
robbery_test <- forecast(test, level = c(95, 80))
plot(robbery_test)
