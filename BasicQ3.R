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

# Descargando datos de Yahoo Finance
# return.class como retorna los datos dataframe_ time series
# periodicity = 'daily' 'weekly' 'monthly'
getSymbols.yahoo('AAPL', env = globalenv(), return.class = "xts",
                 from = '2010-04-20', to = Sys.Date(),
                 periodicity = 'monthly')

chart.TimeSeries(AAPL$AAPL.Close, main = 'APPLE_Close_Price')

# Calculando los retornos compound (simple)
AAPL_ret  = Return.calculate(AAPL$AAPL.Close, method = "compound")
# Eliminando el missing value
AAPL_ret = AAPL_ret[-1, ]
chart.TimeSeries(AAPL_ret, main="Retornos de Apple")
                            
# Calculando retornos con diferencias log. dlog()
AAPL_Ret = diff(log(AAPL$AAPL.Close))
AAPL_Ret = AAPL_Ret[-1, ]

# Calculando retornos (variación anualizada)
AAPL_Retu = diff(log(AAPL$AAPL.Close), lag = 12)
AAPL_Retu = AAPL_Retu[-12:-1,]   


par(mfrow=c(2,1))
chart.TimeSeries(AAPL$AAPL.Close, main = 'APPLE_Close_Price') 
chart.TimeSeries(AAPL_Ret, main="Retornos de Apple_Ret")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
chart.TimeSeries(AAPL_ret, main="Retornos de Apple")
chart.TimeSeries(AAPL_Ret, main="Retornos de Apple_Ret")
chart.TimeSeries(AAPL_Retu, main="Retornos de Apple_Ret")
chart.TimeSeries(AAPL$AAPL.Close, main = 'APPLE_Close_Price')
par(mfrow=c(1,1))