#-------------------------------------------------------------
# Cuestiones de evaluación
#-------------------------------------------------------------

#-------------------------------------------------------------
# Cuestión 1
#-------------------------------------------------------------

# Cargamos los datos
datos <- read.csv2(file.choose(), header=TRUE, row.names=1, dec=",")
datos <- as.matrix(datos)

# Número de países
n <- dim(datos)[1]

# Número de variables
p <- dim(datos)[2]

# Tomamos logaritmos de la variable GPB y renombramos la variable a logGNP
datos[,6] <- log(datos[,6])
colnames(datos)[6] <- "logGNP"

# Estandarizamos los datos
datos.st <- scale(datos)

# Aplicamos el algoritmo de las k-medias
clusters2.datos <- kmeans(datos.st, centers=2, nstart=25)

# Calculamos los clusters desde 2 hasta 15 clusters
SSW <- vector(mode="numeric", length=15)

SSW[1] <- (n-1)*sum(apply(datos.st, 2, var))

for(i in 2:15){
  SSW[i] <- sum(kmeans(datos.st, centers=i, nstart=25)$withinss)
}

plot(1:15, SSW, type='b', xlab="Número de clusters", ylab="Suma de cuadrados dentro de los grupos", pch=19, col="steelblue4")

# Elegimos 4 como el número de clusters
clusters4.datos <- kmeans(datos.st, 4, nstart=25)

# Calculamos los centroides
centroides <- aggregate(datos.st, by=list(clusters4.datos$cluster), FUN=mean)

# Vemos los centroides
t(centroides)

# Dibujamos las variables dos a dos
nk <- 4
pairs(datos.st, col=clusters4.datos$cluster, pch=19)
points(clusters4.datos$centers, col=1:nk, pch=19, cex=2)

# Cargamos la librería cluster
library(cluster)

datos.clusters4 <- clusters4.datos$cluster
clusplot(datos.st, datos.clusters4, color=TRUE, shade=TRUE, labels=2, lines=0)

# Cluster jerárquico

# Matriz de distancia de los datos
d <- dist(datos, method="euclidean")

# Usamos el método ward
fit <- hclust(d, method="ward.D")

# Representamos el dendograma
plot(fit, labels=rownames(datos), cex=0.7)

# Grupo de cada país
groups <- cutree(fit, k=4)

# Recuadramos los cluesters
rect.hclust(fit, k=4, border="blue")
