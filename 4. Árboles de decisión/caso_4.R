#-------------------------------------------------------------
# Cuestiones de evaluación
#-------------------------------------------------------------

#-------------------------------------------------------------
# Cuestión 1
#-------------------------------------------------------------

# Cargamos las librerías
library(datasets)
library(rpart)

# Calculamos el árbol sin podar
iris.tree <- rpart(
  formula = Species ~ Sepal.Length + Sepal.Width + 
    Petal.Length + Petal.Width, method="class", data=iris)

# Representamos el árbol sin podar
plot(iris.tree)
text(iris.tree)

# Calculamos el árbol ajustando el coeficiente de complejidad
iris.tree2 <- rpart(
  formula = Species ~ Sepal.Length + Sepal.Width + 
    Petal.Length + Petal.Width, method="class", 
    data=iris, cp=0.0000001)

# Representamos el gráfico con el error, el número de nodos 
# y el coeficiente de complejidad
plotcp(iris.tree2)

# Calculamos el cp óptimo para del árbol, antes de podar
cpar <- iris.tree2$cptable[
  which.min(iris.tree2$cptable[,"xerror"]), "CP"]

# Podamos el árbol con el valor de cp que acabamos de calcular
iris.tree3 <- prune(iris.tree2, cp=cpar)

# Representamos el árbol
plot(iris.tree3, uniform=TRUE)
text(iris.tree3, cex=0.75)

#-------------------------------------------------------------
# Cuestión 2
#-------------------------------------------------------------

# Cargamos las librerías
library(tree)
library(DAAG)

# Calculamos el árbol
spam7.tr <- tree(yesno ~ .,  spam7)

# Representamos el árbol
plot(spam7.tr)
text(spam7.tr)

# Resumen del árbol calculado
summary(spam7.tr)

