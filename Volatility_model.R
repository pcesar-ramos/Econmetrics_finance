# ECONOMETRÍA FINANCIERA
# MODELOS UNIVARIADOS DE VARIANZA CONDICIONADA
# Autor: Abdel Arancibia
# Estudiante: Paulo Cesar Ramos

# Librerías a utilizar
#   1) quantmod
#   2) PerformanceAnalytics
#   3) rugarch
#   4) forecast
#   5) MLmetrics
#   6) normtest
# install.packages("rugarch")
rm(list = ls())
# Llamando librerías
library(quantmod)
library(PerformanceAnalytics)
library(rugarch)
library(forecast)
library(MLmetrics)
library(normtest)

# Descargando datos SPY
getSymbols.yahoo("SPY", env = globalenv(), return.class = "xts", 
                 from = '2000-01-01', to = '2020-03-31', 
                 periodicity = 'daily')

# Graficando series
chartSeries(SPY, type = "candlesticks", theme = "white", 
            TA = 'addBBands(); addBBands(draw = "p"); addVo(); 
            addMACD(); addRSI(); addSMA()',
            subset = "last 60 weeks")

chart.TimeSeries(SPY$SPY.Adjusted, main = "GRÁFICO: SPY US - ETF")

# Calculando retornos  
SPY.Ret = Return.calculate(SPY$SPY.Adjusted, method = "compound")
SPY.Ret = SPY.Ret[-1,]

chart.TimeSeries(SPY.Ret, main = "Retornos del SPY US")  

# Graficamos las funciones de autocorrelación Retornos Ret_squared
par(mfrow=c(2,2))
acf(SPY.Ret, main="ACF de los Retornos");
pacf(SPY.Ret, main="PACF de los Retornos");
acf(SPY.Ret^2, main="ACF de los Retornos al Cuadrado");
pacf(SPY.Ret^2, main="PACF de los Retornos al Cuadrado")
par(mfrow=c(1,1))

# Los retornos se distribuyen normal
m = mean(SPY.Ret)
s = sd(SPY.Ret)

par(mfrow = c(1,2))
hist(SPY.Ret, nclass = 40, freq = FALSE, main = "Histograma de retornos"); curve(dnorm(x, mean = m, sd = s), from = -0.1, to = 0.1, add = TRUE, col = 'blue')
plot(density(SPY.Ret), main = "Retorno de Distribución Empírica"); curve(dnorm(x, mean = m, sd = s), from = -0.1, to = 0.1, add = TRUE, col = 'blue')
par(mfrow = c(1,1))

# Calculamos la kurtosis
kurtosis(SPY.Ret)

# Exploramos la cola
hist(SPY.Ret, nclass = 100, freq = FALSE, main = "Histograma de retornos", xlim = c(-0.06, 0), ylim = c(0, 5));
curve(dnorm(x, mean = m, sd = s), from = -0.06, to = 0, add = TRUE, col = 'red')

# Usamos el QQPlot  
qqnorm(SPY.Ret, col = "blue")
qqline(SPY.Ret, col = "blue")

#########################################
#########################################
# Estimamos modelos para la Volatilidad
#########################################
#########################################

#==============================================#
# 1) Modelo EWMA    
#==============================================#
lambda = 0.94  

SPY.EWMA = matrix(nrow = nrow(SPY.Ret), ncol = 1)

SPY.R2 = (SPY.Ret - m)^2
SPY.EWMA[1,1] = s^2

for (i in 2:nrow(SPY.EWMA)) {
  SPY.EWMA[i,1] = lambda*SPY.EWMA[i-1,1] + (1 - lambda)*(SPY.Ret[i-1,1] - m)^2
}

SPY.EWMA = xts(SPY.EWMA, order.by = as.Date(index(SPY.Ret)))
names(SPY.EWMA) = "EWMA"
chart.TimeSeries(SPY.EWMA, main = "SPY - Modelo EWMA para volatilidad")
#==============================================#
# 2) Estimador de rango diario    
#==============================================#
SPY.Range = log(SPY$SPY.High/SPY$SPY.Low)
chart.TimeSeries(SPY.Range, main = "SPY - Estimador de rango diario")

#==============================================#
# 3) Estimamos un modelo simple GARCH: sGARCH     
#==============================================#

# I) Encontramos el mejor modelo ARMA(p,q) verificando que el error es ruido blanco

SPY.ARIMA = auto.arima(SPY.Ret, d = NA, D = NA, max.p = 2, max.q = 1, max.P = NA, max.Q = NA,
                       max.d = 0, max.D = NA, seasonal = FALSE, xreg = NULL, ic = c("aic"))

summary(SPY.ARIMA)
checkresiduals(SPY.ARIMA)

par(mfrow=c(1,2))
acf(SPY.ARIMA$residuals, main="ACF de los Residuos");
pacf(SPY.ARIMA$residuals, main="PACF de los Residuos")
par(mfrow=c(1,1))

# II) Verificamos si hay clusters de volatilidad 

par(mfrow=c(1,2))
acf(SPY.ARIMA$residuals^2, main="ACF de los Residuos al Cuadrado");
pacf(SPY.ARIMA$residuals^2, main="PACF de los Residuos al Cuadrado")
par(mfrow=c(1,1))

# III) Como se identificaron clusters de volatilidad, procederemos a seleccionar modelos GARCH candidatos 

GARCH.Mods = list()
GARCH.Fit  = list()
GARCH.BIC  = list()

for (q in 1:2) {
  for (p in 1:2) {
    GARCH.Mods[[paste("GARCH",q,p, sep = "")]] = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)),
                                                            mean.model = list(armaOrder = c(2,0)))
    
    GARCH.Fit[[paste("GARCH",q,p, sep = "")]] = ugarchfit(spec = GARCH.Mods[[paste("GARCH",q,p, sep = "")]], data = SPY.Ret)
    
    GARCH.BIC[[paste("GARCH",q,p, sep = "")]] = infocriteria(GARCH.Fit[[paste("GARCH",q,p, sep = "")]])
    GARCH.BIC[[paste("GARCH",q,p, sep = "")]] = GARCH.BIC[[paste("GARCH",q,p, sep = "")]][2,1]
  }
}

GARCH.BIC = as.data.frame(GARCH.BIC)
colnames(GARCH.BIC[match(min(GARCH.BIC), GARCH.BIC)])

# IV) Verificamos que los residuos estandarizados están limpios de clusters de volatilidad   

par(mfrow=c(2,2))
acf(residuals(GARCH.Fit$GARCH21, standardize=TRUE), main="ACF de los Residuos");
pacf(residuals(GARCH.Fit$GARCH21, standardize=TRUE), main="PACF de los Residuos")
acf(residuals(GARCH.Fit$GARCH21, standardize=TRUE)^2, main="ACF de los Residuos al Cuadrado");
pacf(residuals(GARCH.Fit$GARCH21, standardize=TRUE)^2, main="PACF de los Residuos al Cuadrado")
par(mfrow=c(1,1))       


Box.test(residuals(GARCH.Fit$GARCH21, standardize=TRUE), lag = 10, type = c("Ljung-Box"), fitdf = 0)

GARCH.Fit$GARCH21


# V) Prueba de existencia de asimetría en la NIC       
NIC.GARCH21 = newsimpact(GARCH.Fit$GARCH21)
plot(NIC.GARCH21$zx, NIC.GARCH21$zy, type="l", lwd=2, col="blue",
     main="GARCH(2,1) - News Impact", ylab=NIC.GARCH21$yexpr, xlab=NIC.GARCH21$xexpr)


# VI) Dado que la NIC resultó simétrica, se procederá a estimar una extensión del modelo GARCH que capture
#     los efectos asimétricos de los choques. Estimaremos un EGARCH y un TGARCH (GJR)   


EGARCH.Spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2,1)),
                         mean.model = list(armaOrder = c(2,0)))

EGARCH.Fit  = ugarchfit(EGARCH.Spec, data = SPY.Ret)


TGARCH.Spec = ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(2,1)),
                         mean.model = list(armaOrder = c(2,0)))

TGARCH.Fit  = ugarchfit(TGARCH.Spec, data = SPY.Ret)


NIC.EGARCH21 = newsimpact(EGARCH.Fit)
NIC.TGARCH21 = newsimpact(TGARCH.Fit)

par(mfrow = c(1,1))
plot(NIC.GARCH21$zx, NIC.GARCH21$zy, type="l", lwd=2, col = "blue", ylab=NIC.GARCH21$yexpr, xlab=NIC.GARCH21$xexpr, ylim = c(0,0.009))
par(new = TRUE)
plot(NIC.EGARCH21$zx, NIC.EGARCH21$zy, ylab="", xlab="", type="l", lwd=2, col = "red", ylim = c(0,0.009))
par(new = TRUE)
plot(NIC.TGARCH21$zx, NIC.TGARCH21$zy, ylab="", xlab="", type="l", lwd=2, col = "green2", ylim = c(0,0.009))
title(main = "News Impact Curve", font.main = 4)          
grid()

box()
legend(x = 0.15, y = 0.008, legend = c("GARCH", "EGARCH", "TGARCH"), fill = c("blue", "red", "green2"), cex = 0.9)


# VII) Verificamos normalidad del error con el test de Jarque-Bera y si el error estandarizado no es normal, entonces
#      cambiamos la distribución a una t-student o una GED

# Para el modelo TGARCH
jb.norm.test(residuals(TGARCH.Fit, standardize = TRUE))

TGARCH.Spec = ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(2,1)),
                         mean.model = list(armaOrder = c(2,0)), distribution.model = "std")

TGARCH.Fit  = ugarchfit(TGARCH.Spec, data = SPY.Ret)


# Para el modelo EGARCH  
jb.norm.test(residuals(EGARCH.Fit, standardize = TRUE))                    

EGARCH.Spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(2,1)),
                         mean.model = list(armaOrder = c(2,0)), distribution.model = "std")

EGARCH.Fit  = ugarchfit(EGARCH.Spec, data = SPY.Ret)


# Agrupando las volatilidades estimadas por cada uno de los modelos  

GARCH.Vol = cbind(GARCH.Fit$GARCH21@fit$sigma, TGARCH.Fit@fit$sigma, EGARCH.Fit@fit$sigma)

GARCH.Vol = xts(GARCH.Vol, order.by = index(SPY.Ret))
names(GARCH.Vol) = c("GARCH", "TGARCH", "EGARCH")            

chart.TimeSeries(GARCH.Vol$GARCH, main = "Volatilidad GARCH(2,1)", lwd = 1)
chart.TimeSeries(GARCH.Vol$TGARCH, main = "Volatilidad TGARCH(2,1)", lwd = 1)
chart.TimeSeries(GARCH.Vol$EGARCH, main = "Volatilidad EGARCH(2,1)", lwd = 1)
chart.TimeSeries(GARCH.Vol, main = "Volatilidades estimadas", legend.loc = "topleft", lwd = 1)


