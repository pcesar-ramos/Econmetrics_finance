# Importación de datos desde Excel

# Librería readxls
library(readxl)
CRUDL <- read_excel("D:/USUARIO/Downloads/CRUDL.xlsx", 
                    sheet = "CRUD.L")
View(CRUDL)
library(foreign)
library(tseries)
library(help='tseries')

as.xts(CRUDL)
TSCRUDL.xts <- as.xts(CRUDL, descr='Objeto de datos')
