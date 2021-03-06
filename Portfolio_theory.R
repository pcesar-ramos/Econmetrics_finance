#-----------------------------------------------#
# Econometr�a Financiera - Teor�a del Portafolio
#-----------------------------------------------#

# install.packages('fPortfolio')
library(fPortfolio)
# install.packages('lpSolve')
library(lpSolve)

# Encontrar la frontera eficiente de 5 activos (fPortfolio)

names(SMALLCAP)
head(SMALLCAP)
Data = SMALLCAP.RET
head(Data)
# Ver toda la data
# View(Data)

Data = Data[ ,c("BKE","FCEL", "GG", "OII", "SEB")]
head(Data)

# Cov - Var
matcovData = covEstimator(Data)

# Calculo del portafolio �ptimo 
# Crear la especificaci�n 'simple' corta
especificacioncorta = portfolioSpec()
setSolver(especificacioncorta) = "solveRshortExact"

# Crear la frontera
portfolioFrontier(Data, spec = especificacioncorta, 
                  constraints = "Short")

VentaFrontera = portfolioFrontier(Data, spec= especificacioncorta, 
                                  constraints = "Short")
print(VentaFrontera)

FronteraE = VentaFrontera

# Corre junto desde aqu�
x11()
frontierPlot(FronteraE, frontier = "both", 
             risk = "Sigma", type = "l")

# minvariancePortfolio(FronteraE, pch=19, col="red")
minvariancePoints(FronteraE, pch=19, col="red")

singleAssetPoints(FronteraE, risk ="Sigma",
                  pch=19, cex=1.5, col = topo.colors(6))
# Hasta aqu�
