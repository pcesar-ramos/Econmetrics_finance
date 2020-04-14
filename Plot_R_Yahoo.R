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
# Para obtener ayuda de un comando
  ?get.hist.quote
  preciotesla = get.hist.quote(instrument = "TSLA", quote = "Close")
  plot(preciotesla)
  
  m
  