# Importaci�n de datos desde Excel

# Librer�a readxls
library(readxl)
CRUDL <- read_excel("D:/USUARIO/Downloads/CRUDL.xlsx", 
                    sheet = "CRUD.L")
View(CRUDL)
library(foreign)
library(tseries)
library(help='tseries')

as.xts(CRUDL)
TSCRUDL.xts <- as.xts(CRUDL, descr='Objeto de datos')
