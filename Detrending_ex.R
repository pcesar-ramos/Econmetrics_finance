##################################
####  Detrending series in R #####
####    A briefly example     ####
##################################

# Como limpiar las variables
  rm(list = ls())

# Creamos variables residuo como una distribución normal
  et = rnorm(700, mean = 0, sd = 1)
  yt = matrix(nrow = 700, ncol = 1)

yt[1,1] = 0

t = seq(0:699)

for (i in 2:nrow(yt)) {
  yt[i] = 3 + 0.5*yt[i-1] + 0.03*t[i] + et[i]
}  

ts.plot(yt)

mod = lm(yt ~ t)
summary(mod)

y.res = mod$residuals 
ts.plot(y.res)

acf(y.res)  
pacf(y.res)  
