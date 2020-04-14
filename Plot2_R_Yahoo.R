### _____Clase 2 de Gráficos____###
###____Financial Econometrics___###
###  Paulo Cesar Ramos Huarachi ###
###=============================###

# Librerias a utilizar
#
# 1) quantmod
# 2) PerformanceAnalytics

# Instalando librerias (Pero ya estan instaladas)
  #install.packages("quantmod")
  #install.packages("PerformanceAnalytics")

# Llamando librerías
  library(quantmod)
  library(PerformanceAnalytics)

# Descargando datos de Yahoo Finance
# return.class como retorna los datos dataframe_ time series
# periodicity = 'daily' 'weekly' 'monthly'
  getSymbols.yahoo('WTI', env = globalenv(), return.class = "xts",
                   from = '2010-04-20', to = Sys.Date(),
                   periodicity = 'daily')

# Graficar de los 6 precios - gráfico específico
# TA permite agregar mas "cosas" al gráfico
# Bandas de Bollinger
# MACD Medias moviles de convergencia o divergencia
# Indicador trader RSI Relative strenght Index
# Para recortar el periodo del gráfico subset
  # 'last 50 weeks' or '2007::2017'
  
  x11()
# Grafica el precio y el Volumen
  chartSeries(WTI, theme = "black")
  chartSeries(WTI, type = "candlesticks", theme = "white",
              TA = 'addBBands(); addBBands(draw = "p"); addVo(); addMACD(); addRSI(); addSMA()',
              subset = 'last 50 weeks')
# Gráfica con tema oscuro y periodo 2006 - 2020
  chartSeries(WTI, type = "candlesticks", theme = "black", 
              TA='addBBands(); addBBands(draw="p"); addVo(); addMACD(); addRSI(); addSMA()',
              subset = '2006::2010')
  

