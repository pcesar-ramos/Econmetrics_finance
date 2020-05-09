# Ejercicio Econometría Financiera

library(fPortfolio)
library(lpSolve)
library(quantmod)
library(tseries)
library(PerformanceAnalytics)

sp500 = get.hist.quote(instrument = "^GSPC", start = '2000-01-01', quote = c("Cl"))
netflix = get.hist.quote(instrument = "NFLX", start = '2000-01-01', quote = c("Cl"))
renault = get.hist.quote(instrument = "RNO.PA", start = '2000-01-01', quote = c("Cl"))
KLM = get.hist.quote(instrument = "AF.PA", start = '2000-01-01', quote = c("Cl"))
