####################
### Nivel básico ###
####################
###  Question 1  ###
####################

# limpiar la data 
rm(list = ls())

# Invocar las librerias a utilizar
library(quantmod)
library(PerformanceAnalytics)
library(tseries)

# Descargamos la base de datos de Microsoft
getSymbols.yahoo('MSFT', env = globalenv(), return.class = "xts",
                 from = '2010-01-01', to = Sys.Date(),
                 periodicity = 'monthly')

# Graficamos mediante una matriz para observar todos los datos

par(mfrow=c(2,2))
chart.TimeSeries(MSFT$MSFT.Open, main = "Open price", period.color = "aliceblue")
chart.TimeSeries(MSFT$MSFT.Low, main = "Low price", event.color = "darkgray")
chart.TimeSeries(MSFT$MSFT.Close, main = "Close price", grid.color = "lightgray")
chart.TimeSeries(MSFT$MSFT.High, main = "High price")
par(mfrow=c(1,1))