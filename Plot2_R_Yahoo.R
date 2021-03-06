### _____Clase 2 de Gr�ficos____###
###____Financial Econometrics___###
###  Paulo Cesar Ramos Huarachi ###
###_________Ejemplo WTI_________###
###=============================###

# Librerias a utilizar
#
# 1) quantmod
# 2) PerformanceAnalytics

# Instalando librerias (Pero ya estan instaladas)
  #install.packages("quantmod")
  #install.packages("PerformanceAnalytics")

# Llamando librer�as
  library(quantmod)
  library(PerformanceAnalytics)

# Descargando datos de Yahoo Finance
# return.class como retorna los datos dataframe_ time series
# periodicity = 'daily' 'weekly' 'monthly'
  getSymbols.yahoo('WTI', env = globalenv(), return.class = "xts",
                   from = '2010-04-20', to = Sys.Date(),
                   periodicity = 'daily')

# Graficar de los 6 precios - gr�fico espec�fico
# TA permite agregar mas "cosas" al gr�fico
# Bandas de Bollinger
# MACD Medias moviles de convergencia o divergencia
# Indicador trader RSI Relative strenght Index
# Para recortar el periodo del gr�fico subset
  # 'last 50 weeks' or '2007::2017'

# Charting  

# Grafica el precio y el Volumen
  chartSeries(WTI, theme = "black")
  chartSeries(WTI, type = "candlesticks", theme = "white",
              TA = 'addBBands(); addBBands(draw = "p"); addVo(); addMACD(); addRSI(); addSMA()',
              subset = 'last 50 weeks')
# Gr�fica con tema oscuro y periodo 2006 - 2020

  chartSeries(WTI, type = "candlesticks", theme = "black", 
              TA='addBBands(); addBBands(draw = "p"); addVo(); addMACD(); addRSI(); addSMA()',
              subset = '2006::2008')
  
# Utilizando librer�a �Performance Analytics
# Vamo a trabajar con el precio ajustado
# WTI.Open; WTI.High; WTI.Low; WTI.Close; WTI.Volume; WTI.Adjusted
  # Para seleccionar una variable DENTRO del data frame usa "$" '� "@"
  chart.TimeSeries(WTI$WTI.Adjusted, main = 'WTI_history')
  
# Calculando los retornos
# M�todo a usar el simple = compound
# Usamos el precio ajustado porque es el �ltimo despu�s del cierre que es
# ajustado por dividendos, es la ultima interacci�n que tiene el precio 
  WTI_ret  = Return.calculate(WTI$WTI.Adjusted, method = "compound"
# Pero me sale un NA missing value como lo repongo?
# [fila, columna] [row, col]
  WTI_ret = WTI_ret[-1, ]
  
  chart.TimeSeries(WTI_ret, main="Retornos de WTI")
  
  acf(WTI_ret)
  pacf(WTI_ret)

  