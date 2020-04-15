########################
### Nivel Intermedio ###
########################
###    Question 3    ###
########################

# limpiar la data 
rm(list = ls())

# Invocar las librerias a utilizar
library(quantmod)
library(PerformanceAnalytics)
# Para el test de raiz unitaria ADF
library(tseries)

# Descargando datos de Yahoo Finance
# periodicity = 'monthly'

# Coca cola / Indice bursatil de alemania / Tipo e cambio EURUSD#
getSymbols.yahoo('KO', env = globalenv(), return.class = "xts",
                 from = '2010-04-20', to = Sys.Date(),
                 periodicity = 'monthly')

getSymbols.yahoo('DAX', env = globalenv(), return.class = "xts",
                 from = '2010-04-20', to = Sys.Date(),
                 periodicity = 'monthly')

getSymbols.yahoo('EURUSD=X', env = globalenv(), return.class = "xts",
                 from = '2010-04-20', to = Sys.Date(),
                 periodicity = 'monthly')
# Cambiar de nombre la Variable
EURUSD = `EURUSD=X`

par(mfrow=c(2,1))
chart.TimeSeries(KO$KO.Close, main = 'APPLE_Close_Price')
chart.TimeSeries(DAX$DAX.Close, main = 'APPLE_Close_Price')
chart.TimeSeries(EURUSD$`EURUSD=X.Close`, main = 'APPLE_Close_Price')

