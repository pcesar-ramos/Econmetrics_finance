#################################
####### Gráficos en Rstudio #####

# Instalar el paquete t-series
  #install.packages('tseries')
# Invocar la librería
  library(tseries)
# Obtener datos de facebook 
  preciofb = get.hist.quote(instrument = "FB")
  preciotesla = get.hist.quote(instrument = "TSLA")
# Hago un x11() para luego hacer un plot
  x11()
  plot(preciofb)
# Plot
  x11()
  plot(preciotesla)
# Para obtener ayuda de un comando solo el PRECIO DE CIERRE
  ?get.hist.quote
  preciotesla = get.hist.quote(instrument = "TSLA", quote = "Close")
  plot(preciotesla)
  
# Para graficar tomando en cuenta la fecha de inicio y final
  preciotesla = get.hist.quote(instrument = "TSLA", quote = "Close", start = "2010-06-28", end = "2015-06-23")
  x11()
  plot(preciotesla)
  
  
  