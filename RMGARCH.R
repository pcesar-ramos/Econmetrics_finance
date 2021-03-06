# Econometr�a Financiera
# MSC Rafael Capar�
# Multivarite GARCH Models

# A manera de calentamiento podemos hacer
# una introducci�n con el Eviews

# Ejercicio : Considere los datos del portafolio de 8 activos y ajustar el modelo DCC GARCH a los datos
# de rendimiento financiero multivariante. 

#install.packages("rmgarch")
library(rmgarch)

# Un view de los datos, los retornos de muchas empresas
data(dji30retw)
head(dji30retw)
# Extraigo los 8 primeros
Dat = dji30retw[, 1:8, drop = FALSE]
# Especificaci�n del modelo
# Varianza GARCH 11
uspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1), model = "eGARCH"), distribution.model = "norm")
# Modelo mvnorm y tstudent con Stress
spec1 = dccspec(uspec = multispec(replicate(8, uspec)), dccOrder = c(1,1),  distribution = "mvnorm")
# DCC es el MODELO Din�mico de Correlaci�n Condicional 
fit1 = dccfit(spec1, data = Dat)
?dccfit
?ugarchspec
print(fit1)

# Ejercicio: Muestre el gr�fico analizando el riesgo del portafolio
# considere el analisi del Valor en Riesgo (VaR) basado son el an�lisis de la 
# densidad condicional DCC ( De este modelo de GARCH MuLTIVARIADO). Interprete sus resultados

x11()
plot(fit1)


# Tarea nivel b�sico : Realice los ejercicios anteriores para un porfaolio de 6 activos que usted debe 
# elegir

# Tarea nivel super avanzado: Calcular la matriz de correlaci�n condicional 
# variable en el tiempo utilizando los residuos estandarizados obtenidos de la estimaci�n DCC-GARCH.















