#------------------------------------------------------------------------------
# Cuestiones de evaluación
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Cuestión 1
#------------------------------------------------------------------------------

# Dibujamos la función y = x*sen(x) en [-2π, 2π] con 200 puntos
points = 200
x <- seq(-2*pi, 2*pi, length = points)
y <- x*sin(x)
plot(x,y, type='l', main = 'y = x*sin(x)')

#------------------------------------------------------------------------------
# Cuestión 2
#------------------------------------------------------------------------------

# Ajustamos los datos según un modelo linear
linear.model <- lm(y~x)
summary(linear.model)

# Representamos los datos junto a la recta de regresión
plot(x,y, type='l', main = 'y = x*sin(x) y recta de regresión')
abline(h=-1, col=2)

# Comprobamos las hipótesis del modelo
plot(linear.model,1)
plot(linear.model,2)

#------------------------------------------------------------------------------
# Cuestión 3
#------------------------------------------------------------------------------

# Cargamos la librería
library(nnet)

# Definimos función para ejecutar varias veces (hasta un máximo de 10) 
# el comando nnet y devuelve el ajuste  que tenga el error menor tenga
min.nn <- function(x,y, iter=10, neurals=5){
  min_value <- 10e6 # Infinito
  actual_fit <- NULL
  for(i in seq(1, iter)){
    fit.nn <- nnet(x,y, rang=0.1, size=neurals, linout=T, maxit=10000, trace = FALSE)
    if(fit.nn$value < min_value){
      min_value <- fit.nn$value
      actual_fit <- fit.nn
    }
  }
  return(actual_fit)
}

# Comparamos la función calculada con la función original según el número de
# neuronas en la capa oculta (desde 1 hasta 10)
nf<-layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,11,11,11,11), ncol=5, byrow=TRUE))
par(mai= rep(0.5, 4))
for(neurals in seq(1,10)){
  min <- min.nn(x, y, 10, neurals)
  
  # Curva ajustada con el menor error entre varias ejecuciones de la
  # función nnet
  predict.nn<-predict(min, as.matrix(x))
  
  # Representamos la función predicha junto con la función original
  if(neurals == 1) 
    neurals_plot <-  "neurona" 
  else 
    neurals_plot <- "neuronas"
  
  plot(x,y, col=4, lwd=2, main=paste(neurals, neurals_plot))
  points(x, predict.nn, type='l', col=2, lwd=2)
}

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", ncol=2,legend=c("Función predicha", "Función original"),
        title="Leyenda", lty = c(1, NA), pch = c(NA, 'O'), col=c('red', 'blue'))

# Cargamos la librería neuralnet
library(neuralnet)

# Representamos la red con 1 y 3 neuronas

par(mfrow=c(1,2))
for(neurals in c(1,3)){
  curve.nn <- neuralnet(y~x, cbind(x,y), hidden=neurals, rep=2, stepmax=10e6)
  plot.nn(curve.nn)
}


