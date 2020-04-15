####################
### Nivel básico ###
####################
###  Question 2  ###
####################

# limpiar la data 
rm(list = ls())

# Invocar las librerias a utilizar
library(quantmod)
library(PerformanceAnalytics)
library(tseries)

# Descargamos la base de datos de Microsoft
getSymbols.yahoo('TSLA', env = globalenv(), return.class = "xts",
                 from = '2010-01-01', to = Sys.Date(),
                 periodicity = 'monthly')

# Graficamos mediante una matriz para observar todos los datos
# utilizamos el comando layout(matrix(c(row, col)))
layout(matrix(c(1, 2, 3, 4, 5, 3), 2, 3, byrow = TRUE))
chart.TimeSeries(TSLA$TSLA.Open, main = "Open price")
chart.TimeSeries(TSLA$TSLA.Low, main = "Low price")
chart.TimeSeries(TSLA$TSLA.Close, main = "Close price")
chart.TimeSeries(TSLA$TSLA.High, main = "High price")