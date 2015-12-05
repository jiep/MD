#-------------------------------------------------------------
# Cuestiones de evaluación
#-------------------------------------------------------------

#-------------------------------------------------------------
# Cuestión 1
#-------------------------------------------------------------

# Cargamos la librería bpca
library("bpca")

# Cargamos los datos gabriel1971
data(gabriel1971)

# Mostramos los primeros elementos de los datos
Show(gabriel1971)

# Mostramos un resumen de las variables
summary(gabriel1971)

# Realizamos el análisis de componentes principales 
# sin escalar las variables
prcomp(gabriel1971)

# Resumen del análisis de componentes principales
summary(prcomp(gabriel1971, scale=TRUE))

# Gráfico de la importancia de cada componente
plot(prcomp(gabriel1971, scale=TRUE), 
    main="Importancia de cada componente")

# Gráfico de dispersión de las dos primeras componentes
plot(prcomp(gabriel1971)$x[,1:2])

# Gráfico con las dos primeras componentes
plot(prcomp(gabriel1971)$x[,1:2], type='n')
text(prcomp(gabriel1971)$x[,1:2], rownames(gabriel1971))
abline(v=0)
abline(h=0)

# Usamos el biplot para incorporar la información de las 
# variables
biplot(prcomp(gabriel1971))

#-------------------------------------------------------------
# Cuestión 2
#-------------------------------------------------------------

# Borramos todas las variables anteriores
rm(list=ls())

# Cargamos la librería ca
library(ca)

# Cargamos los datos con los colores de ojos y de pelo
data("HairEyeColor")

# Vemos parte de los datos
show(HairEyeColor)

# Agrupamos hombres y mujeres
# Guardamos los datos en variables por sexos
table.global <- HairEyeColor[ , , 1] + HairEyeColor[, , 2]
table.female <- HairEyeColor[ , , 2]
table.male   <- HairEyeColor[ , , 1]

# Defimimos una función para representar el análisis 
# de correspondencias
cap = function(data, title){
  plot(ca(data), main=title)
}

# Llamamos a la función definida anteriormente con cada 
# una de las variables
cap(table.global, "Hombres y mujeres")
cap(table.male, "Hombres")
cap(table.female, "Mujeres")
