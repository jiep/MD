#-------------------------------------------------------------
# Cuestiones de evaluación
#-------------------------------------------------------------

#-------------------------------------------------------------
# Cuestión 1
#-------------------------------------------------------------

# Cargamos la librería e1071
library(e1071)

# Cargamos el archivo de datos adamantasa.txt
adamantasa <- read.table(file.choose())

# Creamos variables propias para cada viga
pulgadas <- adamantasa[,1]
viga1 <- adamantasa[,2]
viga2 <- adamantasa[,3]

# Representamos la curva definida por los datos de densidades
plot(pulgadas, viga2, type='l')

# Entrenamos el SVM con 25 puntos elegidos al azar
ind.train <- sample(1:314, 25)
ind.train <- sort(ind.train)

# Variable con los cálculos de densidad de los 25 puntos
viga.train <- viga2[ind.train]

# Variable con los cálculos de densidad de los 289 puntos
viga.test <- viga2[-ind.train]

# Variable que será el punto del diámetro de la viga en la que 
# se calcula la densidad
x <- pulgadas[ind.train]

y <- viga.train

# Representamos la curva con los 25 datos de entrenamiento
plot(x,y, type='l')

# Kernel lineal
x.svm <- svm(x, y, type="eps-regression", kernel="linear", 
    epsilon=0.02, cost=100, scale=F)

x.test <- pulgadas[-ind.train]

# Predecimos los valores de la densidad
new <- predict(x.svm, x.test)

# Gráfico con la predicción
plot(x, y, type='l')
points(x.test, new, type='l', col=2)

# Kernel no lineal con epsilon = 2, C = 100, gamma = 40
x.svm2 <- svm(x,y, type="eps-regression", kernel="radial", 
    epsilon=0.02, gamma=40, cost=100, scale=F)

# Nueva predicción
new <- predict(x.svm2, x.test)
points(x.test, new, type='l', col=3)

# Media y desviación típica del modelo
mean(abs(viga.test-new))
sd(abs(viga.test-new))

# Datos para presentar al fabricante
ord <- sort(c(x.test,x), index.return=T)
ent.x <- ord$x
ent.ind <- ord$ix
ent.y <- c(new, viga.train)[ent.ind]
plot(ent.x, ent.y, pch=".", type="l")


#-------------------------------------------------------------
# Cuestión 2
#-------------------------------------------------------------

# Calculamos la media y la desviación típica del modelo
mean(abs(viga.test-new))
sd(abs(viga.test-new))


#-------------------------------------------------------------
# Cuestión 3
#-------------------------------------------------------------

# Calculamos el número mínimo de densidades para el que el 
# modelo siga siendo válido

densidades <- 2
densidades_totales <- 314
ESPECIFICACION <- 5 # Especificación de los ingenieros
esp_actual <- 1000
encontrado <- FALSE
media <- 100
while(esp_actual > ESPECIFICACION && densidades <= densidades_totales && !encontrado){
  
  # Seleccionamos una muestra como entrenamiento
  ind.train <- sample(1:densidades_totales, densidades)
  ind.train <- sort(ind.train)
  
  # Entrenamiento viga
  viga.train <- viga2[ind.train]
  
  # Test de viga
  viga.test <- viga2[-ind.train]
  
  # Variable que será el punto del diámetro de la viga en la que 
  # se calcula la densidad
  x <- pulgadas[ind.train]
  
  y <- viga.train
  
  x.svm2 <- svm(x,y, type="eps-regression", kernel="radial", 
      epsilon=0.02, gamma=40, cost=100, scale=F)
  
  x.test <- pulgadas[-ind.train]
  
  # Nueva predicción
  new <- predict(x.svm2, x.test)
  
  # Media y desviación típica del error
  media <- mean(abs(viga.test-new))
  desv  <- sd(abs(viga.test-new))
  
  # Objeto vacío que contendrá toda la información
  mejor <- NULL
  
  if(media < ESPECIFICACION){
    mejor$densidades <- densidades
    mejor$svm <- x.svm2
    mejor$media <- media
    mejor$desv <- desv
    mejor$train <- viga.train
    mejor$test <- x.test
    mejor$new <- new
    mejor$pulgadas <- x
    encontrado <- TRUE
    esp_actual <- media
  }
  
  densidades <- densidades + 1
}

ord <- sort(c(mejor$test,mejor$pulgadas), index.return=T)
ent.x <- ord$x
ent.ind <- ord$ix
ent.y <- c(new, viga.train)[ent.ind]
plot(ent.x, ent.y, pch=".", type="l")
points(pulgadas, viga2)


