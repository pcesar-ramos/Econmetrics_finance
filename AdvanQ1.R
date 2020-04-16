########################
###  Nivel Avanzado  ###
########################
###    Question 2    ###
########################
rm(list = ls())

library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(urca)

# Descargando datos
# Dólares por pesos Peruanos
getSymbols.yahoo("PEN=X", env = globalenv(), return.class = "xts", from = '2005-12-31', 
                 to = '2020-03-31', periodicity = 'monthly')

getSymbols.yahoo("DX-Y.NYB", env = globalenv(), return.class = "xts", from = '2005-12-31',
                 to = '2020-03-31', periodicity = 'monthly')

USDPEN = `PEN=X` 
DXY    = `DX-Y.NYB`

# Graficando series
par(mfrow=c(2,1))
chart.TimeSeries(USDPEN$`PEN=X.Adjusted`, main = "USD/PEN")
chart.TimeSeries(DXY$`DX-Y.NYB.Adjusted`, main = "DXY")
par(mfrow=c(1,1))

# Calculando retornos (variación anualizada)
USDPEN_Ret = diff(log(USDPEN$`PEN=X.Adjusted`), lag = 12)
USDPEN_Ret = USDPEN_Ret[-12:-1,]          

DXY_Ret = diff(log(DXY$`DX-Y.NYB.Adjusted`), lag= 12)
DXY_Ret = DXY_Ret[-12:-1,]    


par(mfrow=c(2,1))
chart.TimeSeries(USDPEN_Ret, main = "Retornos USD/PEN")
chart.TimeSeries(DXY_Ret, main = "Retornos DXY")
par(mfrow=c(1,1))

# Observando correlaciones dinámicas
ccf(as.ts(DXY_Ret), as.ts(USDPEN_Ret), 
    main = bquote(USDPEN[t] == alpha[0] + alpha[1]*DXY[t+h] + epsilon[t]))

# METODOLOGÍA DE BOX and JENKINS
#    1) Identificación
# Test de raíz unitaria de Dickey Fuller ADF      
df = ur.df(USDPEN_Ret, type = c("trend"), selectlags = c("BIC"))
summary(df) 

# Correlogramas para definir valores tanto como para el orden de (p) y (q).

par(mfrow=c(1,2))
acf(USDPEN_Ret, main = "ACF de los retornos de USDPEN")
pacf(USDPEN_Ret, main = "PACF de los retornos de USDPEN")
par(mfrow=c(1,1))

#   2) Estimación 
#install.packages("forecast")
library(forecast)

# Sample: Omitimos los 9 últimos meses
# length(USDPEN_Ret) = 159
USDPEN_SMPL = USDPEN_Ret[1:150,]  
DXY_SMPL    = DXY_Ret[1:150,]

# Autor arima select model modelos ARMA(p,q) Criterio Bayesiano de Schwarz BIC
# ARMA
m1 = auto.arima(USDPEN_SMPL, d=NA, D=NA, max.p = 1, max.q = 5, max.P = NA, max.Q = NA,
                max.d = 0, max.D = NA, seasonal = FALSE, xreg = DXY_SMPL, ic = c("bic"))

# ARMAX
m2 = auto.arima(USDPEN_SMPL, d=NA, D=NA, max.p = 1, max.q = 5, max.P = NA, max.Q = NA,
                max.d = 0, max.D = NA, seasonal = FALSE, xreg = NULL, ic = c("bic"))

# 2) Utilizando el criterio de Akaike (AIC) 

m3 = auto.arima(USDPEN_SMPL, d=NA, D=NA, max.p = 1, max.q = 6, max.P = NA, max.Q = NA,
                max.d = 0, max.D = NA, seasonal = FALSE, xreg = DXY_SMPL, ic = c("aic"))


m4 = auto.arima(USDPEN_SMPL, d=NA, D=NA, max.p = 1, max.q = 6, max.P = NA, max.Q = NA,
                max.d = 0, max.D = NA, seasonal = FALSE, xreg = NULL, ic = c("aic"))

# Almacenamos los datos ajustados de cada modelo

Dates_S = as.Date(index(USDPEN_SMPL))
Dates   = as.Date(index(USDPEN_Ret))      

USDPEN_Hat1 = m1$fitted
USDPEN_Hat1 = xts(USDPEN_Hat1, order.by = Dates_S)

USDPEN_Hat2 = m2$fitted
USDPEN_Hat2 = xts(USDPEN_Hat2, order.by = Dates_S)

USDPEN_Hat3 = m3$fitted
USDPEN_Hat3 = xts(USDPEN_Hat3, order.by = Dates_S)

USDPEN_Hat4 = m4$fitted
USDPEN_Hat4 = xts(USDPEN_Hat4, order.by = Dates_S)


Model.Series1 = cbind(USDPEN_SMPL, USDPEN_Hat1)
Model.Series2 = cbind(USDPEN_SMPL, USDPEN_Hat2)
Model.Series3 = cbind(USDPEN_SMPL, USDPEN_Hat3)
Model.Series4 = cbind(USDPEN_SMPL, USDPEN_Hat4)


names(Model.Series1) = c('Observado', 'Ajustado')
names(Model.Series2) = c('Observado', 'Ajustado')
names(Model.Series3) = c('Observado', 'Ajustado')
names(Model.Series4) = c('Observado', 'Ajustado')


par(mfrow=c(2,2))
chart.TimeSeries(Model.Series1, main = "ARMAX(1,0)", legend.loc = "bottomleft")
chart.TimeSeries(Model.Series2, main = "ARMA(1,0)", legend.loc = "bottomleft")
chart.TimeSeries(Model.Series3, main = "ARMAX(1,1)", legend.loc = "bottomleft")
chart.TimeSeries(Model.Series4, main = "ARMA(1,1)", legend.loc = "bottomleft")
par(mfrow=c(1,1))

# Error Cuadrático Medio (MSE) para la bondad de ajuste de los modelos

#install.packages("MLmetrics")
library(MLmetrics)

MSE1 = MSE(USDPEN_Hat1, USDPEN_Ret)
MSE2 = MSE(USDPEN_Hat2, USDPEN_Ret)
MSE3 = MSE(USDPEN_Hat3, USDPEN_Ret)
MSE4 = MSE(USDPEN_Hat4, USDPEN_Ret)

MSE_Tot = cbind(MSE1, MSE2, MSE3, MSE4)
MSE_Tot  

# 3) Diagnóstico

# Diagnóstico de los Residuos  
checkresiduals(m1)
checkresiduals(m2)
checkresiduals(m3)
checkresiduals(m4)

# Test de Jarque Bera
# ("normtest")
library(normtest)

jb.norm.test(m1$residuals)
jb.norm.test(m2$residuals)
jb.norm.test(m3$residuals)
jb.norm.test(m4$residuals)

#     4) Predicción
# Se analizará al modelo que realiza el mejor pronóstico.


M1_F = forecast(m1, h = 9, level = c(30,60,90), xreg = DXY_Ret[151:159,])
M2_F = forecast(m2, h = 9, level = c(30,60,90))
M3_F = forecast(m3, h = 9, level = c(30,60,90), xreg = DXY_Ret[151:159,])
M4_F = forecast(m4, h = 9, level = c(30,60,90))


par(mfrow=c(2,2))
plot(M1_F, main = "Forecast ARMAX(1,0)")
plot(M2_F, main = "Forecast ARMA(1,0)")
plot(M3_F, main = "Forecast ARMAX(1,1)")
plot(M4_F, main = "Forecast ARMA(1,1)")
par(mfrow=c(1,1))


Date_F        = index(USDPEN_Ret[151:159])
USDPEN_SMPL_F = as.ts(USDPEN_Ret[151:159,], start=151, end=159)


# Error Cuadrático Medio (MSE) para las proyecciones
#
MSE1.F = MSE(M1_F$mean, USDPEN_SMPL_F)
MSE2.F = MSE(M2_F$mean, USDPEN_SMPL_F)
MSE3.F = MSE(M3_F$mean, USDPEN_SMPL_F)
MSE4.F = MSE(M4_F$mean, USDPEN_SMPL_F)

MSE.F  = cbind(MSE1.F, MSE2.F, MSE3.F, MSE4.F) 
MSE.F

Forecasts = cbind(USDPEN_SMPL_F, M1_F$mean, M2_F$mean, M3_F$mean, M4_F$mean)
Forecasts = xts(Forecasts, order.by = Date_F)
Forecasts = as.ts(Forecasts)

names(Forecasts) = c("USDPEN", "ARMAX(1,0)", "ARMA(1,0)", "ARMAX(1,1)", "ARMA(1,1)")

# Graficando Forecasts
chart.TimeSeries(Forecasts, main = "Proyecciones para USDPEN", legend.loc = "bottomleft")


