########################
### Nivel Intermedio ###
########################
###    Question 2    ###
########################

# limpiar la data 
rm(list = ls())

# Invocar las librerias a utilizar
library(quantmod)
library(PerformanceAnalytics)
# Para el test de raiz unitaria ADF
library(tseries)
library(urca)

# Descargando datos de Yahoo Finance
# periodicity = 'monthly'

# Coca cola / Indice bursatil de alemania / Tipo e cambio EURUSD#
#=========================En niveles============================#
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

#==========================Retornos=============================#
# Calculando retornos (variación anualizada)
KO_ret = diff(log(KO$KO.Close), lag = 12)
KO_ret = KO_ret[-12:-1,]   

DAX_ret = diff(log(DAX$DAX.Close), lag = 12)
DAX_ret = DAX_ret[-12:-1,]

EURUSD_ret = diff(log(EURUSD$`EURUSD=X.Close`), lag = 12)
EURUSD_ret = EURUSD_ret[-12:-1,]   

#================================================================#
# Gráficos de autocorrelación parcial y simple 
par(mfrow=c(3,2))
acf(EURUSD_ret, main = "ACF EURUSD_ret")
pacf(EURUSD_ret, main = "PACF EURUSD_ret")
acf(KO_ret, main = "ACF KO_ret")
pacf(KO_ret, main = "PACF KO_ret")
acf(DAX_ret, main = "ACF DAX_ret")
pacf(DAX_ret, main = "PACF DAX_ret")
par(mfrow=c(1,1))

# Utilizaremos el test de raíz unitaria de Dickey Fuller con los rendimientos      
adf.test(KO_ret)

adf.test(DAX_ret)

adf.test(EURUSD_ret)
# Diferenciamos las variables EURUSD y DAX
DAX_ret1 = diff(DAX_ret)
DAX_ret1 = DAX_ret1[-1,]

EURUSD_ret1 = diff(EURUSD_ret)
EURUSD_ret1 = EURUSD_ret1[-1,]  

adf.test(DAX_ret1)

adf.test(EURUSD_ret1)

plot(DAX_ret1)