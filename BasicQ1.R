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
                 from = '2000-01-01', to = Sys.Date(),
                 periodicity = 'daily')

# Graficamos mediante una matriz para observar todos los datos
par(mfrow=c(2,2))
chartSeries(MSFT$MSFT.Open)
chartSeries(MSFT$MSFT.Low)
chartSeries(MSFT$MSFT.Close)
chartSeries(MSFT$MSFT.High)
par(mfrow=c(1,1))