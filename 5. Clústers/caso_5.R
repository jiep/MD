#-------------------------------------------------------------
# Cuestiones de evaluación
#-------------------------------------------------------------

#-------------------------------------------------------------
# Cuestión 1
#-------------------------------------------------------------

# Cargamos los datos
datos <- read.csv2(file.choose(), header=TRUE, 
  row.names=1, dec=",")
datos <- as.matrix(datos)

# Número de países
n <- dim(datos)[1]

# Número de variables
p <- dim(datos)[2]

# Tomamos logaritmos de la variable GPB y 
# renombramos la variable a logGNP
datos[,6] <- log(datos[,6])
colnames(datos)[6] <- "logGNP"

# Estandarizamos los datos
datos.st <- scale(datos)

# Aplicamos el algoritmo de las k-medias para 5 clusters
clusters5.datos <- kmeans(datos.st, 5, nstart=25)

# Calculamos los centroides
centroides <- aggregate(datos.st, 
  by=list(clusters5.datos$cluster), FUN=mean)

# Vemos los centroides
t(centroides)

# Dibujamos las variables dos a dos
nk <- 5
pairs(datos.st, col=clusters5.datos$cluster, pch=19)
points(clusters5.datos$centers, col=1:nk, pch=19, cex=2)

# Cargamos la librería cluster
library(cluster)

datos.clusters5 <- clusters5.datos$cluster
clusplot(datos.st, datos.clusters5, color=TRUE, shade=TRUE, 
  labels=2, lines=0)

# Países por cluster
grupo_1 = which(clusters5.datos$cluster==1)
grupo_2 = which(clusters5.datos$cluster==2)
grupo_3 = which(clusters5.datos$cluster==3)
grupo_4 = which(clusters5.datos$cluster==4)
grupo_5 = which(clusters5.datos$cluster==5)

#-------------------------------------------------------------
# Cuestión 1
#-------------------------------------------------------------

# Cargamos la dase de datos iris del paquete datasets
library(datsets)

# Dimensiones de los datos. Se encuentran en la variable iris
dim(iris)

# Elegimos 50 datos al azar
ind <- sample(1:150, 50)
iris.cl <- iris[ind, 1:4]

# Guardamos las etiquetas de cada dato
etiquetas <- iris[ind, 5]

# Calculamos el dendograma

# Matriz de distancia de los datos
d <- dist(iris.cl, method="euclidean")

# Calculamos el dendograma con distintos métodos
metodos <- c("ward.D", "ward.D2", "single", "complete", 
  "average", "mcquitty", "median", "centroid")

for(metodo in 1:length(metodos)){
  # Usamos el método del vector ''metodos'' 
  # definido anteriormente
  fit <- hclust(d, method=metodos[metodo])
  
  # Representamos el dendograma
  plot(fit, labels=etiquetas, cex=0.7, 
    main=paste("Dendograma con el método ", metodos[metodo]))
  
  # Grupo de cada 
  groups <- cutree(fit, k=3)
  
  # Recuadramos los cluesters
  rect.hclust(fit, k=3, border="blue")
}

