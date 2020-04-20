# Importación de datos desde Excel

library(readxl)
CRUDL <- read_excel("D:/USUARIO/Downloads/CRUDL.xlsx", 
                    sheet = "CRUD.L")
View(CRUDL)