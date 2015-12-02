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
plot(x,y, type='l')

#------------------------------------------------------------------------------
# Cuestión 2
#------------------------------------------------------------------------------

# Ajustamos los datos según un modelo linear
linear.model <- lm(y~x)
summary(linear.model)

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
min.nn <- function(x,y, iter=10){
  min_value <- 10e6 # Infinito
  actual_fit <- NULL
  for(i in seq(1, iter)){
    fit.nn <- nnet(x,y, rang=0.1, size=5, linout=T, maxit=10000, trace = FALSE)
    if(fit.nn$value < min_value){
      min_value <- fit.nn$value
      actual_fit <- fit.nn
    }
  }
  return(actual_fit)
}

# Curva ajustada con el menor error entre varias ejecuciones de la
# función nnet
min=min.nn(x,y)

predict.nn<-predict(min, as.matrix(x))

# Cargamos la librería neuralnet
neuralnet(y~x, cbind(x,y), hidden=5, rep=1)