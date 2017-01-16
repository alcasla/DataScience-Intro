#Leemos dataset (***PATH AL DATASET***) y asigno nombres de cada característica
wine <- read.delim("~/R/ICC_TrabajoFinal/wine/wine.dat", header = F, sep=",", skip=18, as.is=TRUE);
names(wine) <- c("Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", "TotalPhenols", "flavanoids", 
                 "NonflavanoidsPhenols", "Proanthocyanins", "ColorIntensity", "Hue", "OD280/OD315", 
                 "Proline", "Class");
attach(wine)

dim(wine)
summary(wine)
#Obtenemos el tipo de cada variable
apply(wine, 2, mode)

#Visión general de todas la variables
boxplot(wine, main="Boxplot Wine", las=2)
boxplot(wine-wine$Proline, main="Boxplot Wine", las=2)

#Función que hace el histograma editando etiquetas - Dataset WINE
histograma <- function (x) {
  hist(wine[,x], main=paste("Histograma ", names(wine)[x]), xlab=names(wine)[x])
}

#Distribución de cada variable
par(mfrow=c(4,4))
sapply(1:(dim(wine)[2]), histograma)
par(mfrow=c(1,1))

#Moda
library(modeest)
mlv(wine$Alcohol, method="mfv")
apply(wine, 2, mlv,  method = "mfv")

#Histograma + media + mediana + moda
hist(wine$Alcohol, main=paste("Histograma Alcohol"), xlab="Alcohol")
abline(v=mean(wine$Alcohol), col="Red", lwd=2)
abline(v=median(wine$Alcohol), col="Green", lwd=2)

#Función que hace el histograma editando etiquetas, pinta su media y mediana - Dataset WINE
histogramaAbline <- function (x) {
  hist(wine[,x], main=paste("Histograma ", names(wine)[x]), xlab=names(wine)[x])
  abline(v=mean(wine[,x]), col="Red", lwd=2)
  abline(v=median(wine[,x]), col="Green", lwd=2)
}

#Distribución con media y moda
par(mfrow=c(4,4))
sapply(1:(dim(wine)[2]), histogramaAbline)
par(mfrow=c(1,1))

#Summary respecto a la clase de todas las varibales
require(Hmisc)
require(mosaic)
summaryMosaic <- function(x) {
  print(names(wine)[x])
  summary(wine[,x] ~ Class, data=wine, fun=favstats)
}

sapply(1:(dim(wine)[2]-1), summaryMosaic, simplify=FALSE)


#Coeficiente de variación
coeficienteVariación <- function(x){
  print(names(wine)[x])
  summary(wine[,x] ~ Class, data=wine, fun=favstats)[,8]/summary(wine[,x] ~ Class, data=wine, fun=favstats)[,7]
}

sapply(1:(dim(wine)[2]-1), coeficienteVariación, simplify=FALSE)


#Plot variable contra la variable de clase y pinta las medias de cada clase para dicha variable
plotDifsMeans <- function(x) {
  plot(wine[,x], wine$Class, pch=16, col=wine$Class, ylab="Class", xlab=names(wine)[x], main=paste("Plot ", 
    names(wine)[x], " vs Class"))
  abline(v=summary(wine[,x] ~ Class, data=wine, fun=favstats)[[4*6+1]], col="black")
  abline(v=summary(wine[,x] ~ Class, data=wine, fun=favstats)[[4*6+2]], col="red")
  abline(v=summary(wine[,x] ~ Class, data=wine, fun=favstats)[[4*6+3]], col="green")
}

plotDifsMeans(1)
plotDifsMeans(3)
plotDifsMeans(6)
plotDifsMeans(7)
