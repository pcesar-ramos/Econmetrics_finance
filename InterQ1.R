########################
### Nivel Intermedio ###
########################
###    Question 1    ###
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
KO_ret  = Return.calculate(KO$KO.Close, method = "compound")
KO_ret = KO_ret[-1, ]

DAX_ret  = Return.calculate(DAX$DAX.Close, method = "compound")
DAX_ret = DAX_ret[-1, ]

EURUSD_ret  = Return.calculate(EURUSD$`EURUSD=X.Close`, method = "compound")
EURUSD_ret = EURUSD_ret[-1, ]



par(mfrow=c(3,2))
chart.TimeSeries(KO$KO.Close, main = 'COKE_Close_Price')
chart.TimeSeries(KO_ret, main="Retornos de Coke")
chart.TimeSeries(DAX$DAX.Close, main = 'DAX_Close_Price')
chart.TimeSeries(DAX_ret, main="Retornos Bursátil alemán")
chart.TimeSeries(EURUSD$`EURUSD=X.Close`, main = 'EURUSD_Close_Price')
chart.TimeSeries(EURUSD_ret, main="Retornos EURUSD")
par(mfrow=c(1,1))

# Utilizaremos el test de raíz unitaria de Dickey Fuller En niveles      
df_KO = ur.df(KO$KO.Close, type = c("trend"), selectlags = c("BIC"))
summary(df_KO) 
adf.test(KO$KO.Close)

df_DAX = ur.df(DAX$DAX.Close, type = c("trend"), selectlags = c("BIC"))
summary(df_DAX)  
adf.test(DAX$DAX.Close)

df_EURUSD = ur.df(EURUSD$`EURUSD=X.Close`, type = c("trend"), selectlags = c("BIC"))
summary(df_EURUSD)
adf.test(EURUSD$`EURUSD=X.Close`)

# Utilizaremos el test de raíz unitaria de Dickey Fuller con los rendimientos      
df_KO_ret = ur.df(KO_ret, type = c("trend"), selectlags = c("BIC"))
summary(df_KO_ret)  
adf.test(KO_ret)

df_DAX_ret = ur.df(DAX_ret, type = c("none"), selectlags = c("BIC"))
summary(df_DAX_ret)  
adf.test(DAX_ret)

df_EURUSD_ret = ur.df(EURUSD_ret, type = c("none"), selectlags = c("BIC"))
summary(df_EURUSD_ret) 
adf.test(EURUSD_ret)
