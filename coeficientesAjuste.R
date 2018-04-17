# Directorio de los datos
rm(list=ls())
setwd("~/Escritorio")
library(readxl)
install.packages("rootSolve")

# Recopilación de los datos
bidimesional <- read.csv("~/Escritorio/b2d.csv")
attach(bidimesional)

# Cálculo del periodo fundamental de la serie de Fourier
# Este era el problema de ayer
w <- 0.01729

# Cálculo de los coeficientes de la serie de Fourier
regfourier <- lm(J ~ cos(w*D) + sin(w*D) + cos(2*w*D) + sin(2*w*D) + cos(3*w*D) + sin(3*w*D)
                 + cos(4*w*D) + sin(4*w*D) + cos(5*w*D) + sin(5*w*D) + cos(6*w*D) + sin(6*w*D) 
                 + cos(7*w*D) + sin(7*w*D) + cos(8*w*D) + sin(8*w*D))
# Basta hacerlo con n = 8. 

# Creación del gráfico del ajuste de Fourier
rg <- diff(range(J))
plot(J~D, ylim=c(min(J)-0.1*rg, max(J)+0.1*rg))
lines(fitted(regfourier)~D, col=3) 

# Extraccion de coeficientes
coeficientes <- as.numeric(regfourier$coefficients)

# Definicion de la funcion de ajuste
curvaj <- function(x, coef = coeficientes, n=8) {
  suma <- coeficientes[1]
  #
  for (i in 1:n) {
    suma <- suma + coef[2*i]*cos(i*w*x) + coef[2*i+1]*sin(i*w*x) 
  }
  return(suma)
}

# Esta funcion encuentra el valor de theta dado J
# OJO: La función tiene dos valores de J para cada theta
datoD <- function(j, a = 100){
require(rootSolve)
  
fun1 <- function(x , y = j) {
  curvaj(x) - y
}

l1 <- c(min(D), a)
l2 <- c(a, max(D))

dato <- c(uniroot(fun1,l1)$root, uniroot(fun1,l2)$root)

return(dato)
}

# Prueba 
datoD(2)
datoD(-4)
