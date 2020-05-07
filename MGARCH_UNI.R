# Econometría Finnaciera
# Modelo GARCH multivariado Garch
# Diversificación del riesgo
# Eje x : Riesgo - Eje y: Retorno
# Curva e Markowitz (Frontera eficiente)
# Portafolios sobre la curva son eficiente
# La línea de mercado de capital (CAL)
# El óptimo portafolio es mCAL=mCE
# Ratio = Rentabilidad(y)/Riesgo(x) tenemos un esoeciue de pendiente
# Ret Cateto Opuesto Risk cateto adiacente
# Premio Nobel Markowitz - Sharpe contribuyo con un ratio (El ratio de sharpe)
# Rs = (E(Rp)-Rf )/ Desv standar del portafolio Rf tasa libre de riesgo
# Rf es el activo "libre de riesgo" Tiene que estar en el eje y porque el riesgo es 0 es una intersección en el eje y
# El asset allocation: cuanto distribuyo por cada activo en mi portafolio

# Vamos al R studio (Riesgo de crédito) cross section

# install.packages("fPortfolio")
library(fPortfolio)
# install.packages("lpSolve")
library(lpSolve)

# Ejercicio encontrar la forntera eficiente de 5 activos
# Ayuda considere el package fPortfolio

names(SMALLCAP)
head(SMALLCAP)

# Como vuelves retornos a tud datos
Data = SMALLCAP.RET
head(Data)

# View(Data) 
# Voy a sacar 5 activos extraigo 5 precios 
Data=Data[,c("BKE", "FCEL", "GG", "OII","SEB")]
head(Data)
# Aca lo trabajas desde tus datos con chart***
#Vamos a calcular la media voy a hacer el comando cov()
# Para sacar la matriz de las covarianzas media y Varianza
# Con este comando estimos la matriza de covarianzas
matcovData = covEstimator(Data)

# Voy a calcular el portafolio:
# Creando la especificación:
#En blanco por defalult de largo plazo
especificacioncorta = portfolioSpec()
# Es para que relaice una optimizacion
# Un especificacion de media varianza 
# Un espec simple; la vamos cambiando seun lo que nos diga nuestro jefe

setSolver(especificacioncorta) = "solveRshortExact"

# Creando la frontera eficiente de 5 activos
# que poones los datos la base de datos 
portfolioFrontier(Data, spec= especificacioncorta, 
                  constraints = "Short")
# Aún asi el solver resuelve el problema 
# la matriz es un poco dificil de invertir
# POr defecto te saca el 1 13 25 37 50
# En el excel cuantos hicimos simulaciones 
# combinaciones que le das a tu inversion
# sale negativo porque puedes vender pusiste la restriccion corta
# por eso puedes vender tu asset allocation es -
# Tambien genera las matriz de VAR Y COV
# y tambine la estimamos con Garch una matriz dinámica
# un proceso generador de datos
# objetivos de riesgos mead es el retorno
# como elgigo en funcion a su media a cov, Var desv Cvar

#Analisis mas rigutoso
# para crear la frontera


VentaFrontera = portfolioFrontier(Data, spec= especificacioncorta, 
                  constraints = "Short")
print(VentaFrontera)

# Mejor la graficamos
#(Portafolio weights#)
FronteraE = VentaFrontera
#corre aqui
x11()
frontierPlot(FronteraE, frontier = "both", 
             risk = "Sigma", type = "l")
#frontierPlot(FronteraE, frontier = "both", 
             #risk = "VaR", type = "l")
# Graficamos la fronter eficiente l de line
# fronter que solo es gris los efi y inef, sigma podriamos cambiar
# CVaR

# Algunos portafolioa dentro de l forntera de posibles 
# portafolios que puedas invertir

#minvarianza=minvariancePortfolio(FronteraE, 
#                                 pch=19, col="red")

#minvariancePortfolio(FronteraE, pch=19, col="red")
minvariancePoints(FronteraE, pch=19, col="red")

singleAssetPoints(FronteraE, risk ="Sigma",
                  pch=19, cex=1.5, col = topo.colors(6))
# hasta aquí

# El ejercicio a ustedes

# Ejercicio: Contruir un portafolio con el 
# S&P500 Netflix Renault y KLM AIR FRANCE
# Encuentre el portafolio de minima varianza grafique
library(quantmod)
library(tseries)

sp500 = get.hist.quote(instrument = "^GSPC", from = '2010-12-31', to = '2019-12-30')
netflix = get.hist.quote(instrument = "NFLX", from = '2010-12-31', to = '2019-12-30')
renault = get.hist.quote(instrument = "RNO.PA", from = '2010-12-31', to = '2019-12-30')
KLM = get.hist.quote(instrument = "AF.PA", from = '2010-12-31', to = '2019-12-30')
sp500 = getSymbols.yahoo("^GSPC", env = globalenv(), return.class = "xts", from = '2005-12-31', 
                 to = '2020-03-31', periodicity = 'monthly')
netflix =getSymbols.yahoo("NFLX", env = globalenv(), return.class = "xts", from = '2005-12-31', 
                 to = '2020-03-31', periodicity = 'monthly')
getSymbols.yahoo("RNO.PA", env = globalenv(), return.class = "xts", from = '2005-12-31', 
                 to = '2020-03-31', periodicity = 'monthly')
getSymbols.yahoo("AF.PA", env = globalenv(), return.class = "xts", from = '2005-12-31', 
                 to = '2020-03-31', periodicity = 'monthly')

library(fPortfolio)
library(lpSolve)
library(tseries)
datax = data.frame(sp500$Close, netflix$Close, renault$Close, KLM$Close)
View(datax)

############################


sp500 = get.hist.quote(instrument = "^GSPC", start = '2000-01-01', quote = c("Cl"))
netflix = get.hist.quote(instrument = "NFLX", start = '2000-01-01', quote = c("Cl"))
renault = get.hist.quote(instrument = "RNO.PA", start = '2000-01-01', quote = c("Cl"))
KLM = get.hist.quote(instrument = "AF.PA", start = '2000-01-01', quote = c("Cl"))

# Tarea para certificados - Comentar todo
# sirvio para abrir una ventana


# De aqui vamos a DINAMIZAR La matriz de covarianzas y Varianzas
# MODELOS MULTIVARIADOS GARCH

#install.packages("rmgarch")
library(rmgarch)

data(dji30retw)

#DCC Modelo Dinamico de correlacion condicional