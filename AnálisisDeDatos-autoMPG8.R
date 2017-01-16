#Leemos dataset (***PATH AL DATASET***)
autoMPG8 <- read.delim("~/R/ICC_TrabajoFinal/autoMPG8/autoMPG8.dat", header = F, sep=",", skip=12, as.is=TRUE);
#Nombres más adelante
names(autoMPG8) <- c("Cylinders", "Displacement", "Horse_power", "Weight", "Acceleration", "Model_year", "Origin", "Mpg");
attach(autoMPG8);

dim(autoMPG8)
str(autoMPG8)
summary(autoMPG8)

#Visión general de todas la variables
boxplot(autoMPG8, main="Boxplot autoMPG8", las=2)
boxplot(autoMPG8[,-c(4)], main="Boxplot autoMPG8", las=2)
boxplot(autoMPG8[,-c(2,3,4)], main="Boxplot autoMPG8", las=2)

#Función que hace el histograma editando etiquetas - Dataset autoMPG8
histograma <- function (x) {
  hist(autoMPG8[,x], main=paste("Histograma ", names(autoMPG8)[x]), xlab=names(autoMPG8)[x])
}

#Distribución de cada variable
par(mfrow=c(3,3))
sapply(1:(dim(autoMPG8)[2]), histograma)
par(mfrow=c(1,1))

#Función que hace el histograma editando etiquetas, pinta su media y mediana - Dataset WINE
histogramaAbline <- function (x) {
  hist(autoMPG8[,x], main=paste("Histograma ", names(autoMPG8)[x]), xlab=names(autoMPG8)[x])
  abline(v=mean(autoMPG8[,x]), col="Red", lwd=2)
  abline(v=median(autoMPG8[,x]), col="Green", lwd=2)
}

#Distribución con media y moda
par(mfrow=c(3,3))
sapply(1:(dim(autoMPG8)[2]), histogramaAbline)
par(mfrow=c(1,1))


#Normalización
normalize <- function(x) {
  (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
}
autoMPG8Norm <- as.data.frame(lapply(autoMPG8, normalize))


#Summary respecto a la clase de todas las varibales
require(Hmisc)
require(mosaic)
summaryMosaic <- function(x) {
  print(names(autoMPG8Norm)[x])
  summary(autoMPG8Norm[,x] ~ autoMPG8Norm[,dim(autoMPG8Norm)[2]], data=autoMPG8Norm, fun=favstats)
}

sapply(1:(dim(autoMPG8Norm)[2]-1), summaryMosaic, simplify=FALSE)

#Summary respecto a la clase de todas las varibales - Muestra SD
summaryMosaic <- function(x) {
  print(names(autoMPG8Norm)[x])
  summary(autoMPG8Norm[,x] ~ autoMPG8Norm[,dim(autoMPG8Norm)[2]], data=autoMPG8Norm, fun=favstats)
}

sapply(1:5, summaryMosaic, simplify=FALSE)

#Función que pinta del dataset 'autoMPG8' con un plot las dos clases que se especifiquen por parámetros
plotY <- function (x,y) {
  plot(autoMPG8Norm[,y]~autoMPG8Norm[,x], xlab=names(autoMPG8Norm)[x], ylab=names(autoMPG8Norm)[y])
}

#Agrupamos gráficas en rejilla (row*col) de 3x3
par(mfrow=c(3,3))
#Aplica la función anterior sobre todas las variables menos la clase a predecir contra la que visualizamos el resto de variables
sapply(1:(dim(autoMPG8Norm)[2]-1), plotY, dim(autoMPG8Norm)[2])
#Restrablece a la visualización por defecto
par(mfrow=c(1,1))


#[0.000,0.226)
#[0.226,0.372)
#[0.372,0.545)
#[0.545,1.000]
divide_class_ranges <- function(x) {
  if(autoMPG8Norm$class[x]<0.226)
    autoMPG8Norm$class[x]=0
  else
    if(autoMPG8Norm$class[x]<0.372)
      autoMPG8Norm$class[x]=1
  else
    if(autoMPG8Norm$class[x]<0.545)
      autoMPG8Norm$class[x]=2
  else
    autoMPG8Norm$class[x]=3
}
autoMPG8Norm$class <- autoMPG8Norm$V8
autoMPG8Norm$class <- sapply(1:dim(autoMPG8Norm)[1], divide_class_ranges)

par(mfrow=c(3,2))
plot(autoMPG8Norm$V8~autoMPG8Norm$V1, xlab=names(autoMPG8Norm)[1], ylab=names(autoMPG8Norm)[8], col=autoMPG8Norm$class+1)
plot(autoMPG8Norm$V8~autoMPG8Norm$V2, xlab=names(autoMPG8Norm)[2], ylab=names(autoMPG8Norm)[8], col=autoMPG8Norm$class+1)
plot(autoMPG8Norm$V8~autoMPG8Norm$V3, xlab=names(autoMPG8Norm)[3], ylab=names(autoMPG8Norm)[8], col=autoMPG8Norm$class+1)
plot(autoMPG8Norm$V8~autoMPG8Norm$V4, xlab=names(autoMPG8Norm)[4], ylab=names(autoMPG8Norm)[8], col=autoMPG8Norm$class+1)
plot(autoMPG8Norm$V8~autoMPG8Norm$V6, xlab=names(autoMPG8Norm)[6], ylab=names(autoMPG8Norm)[8], col=autoMPG8Norm$class+1)
plot(autoMPG8Norm$V8~autoMPG8Norm$V7, xlab=names(autoMPG8Norm)[7], ylab=names(autoMPG8Norm)[8], col=autoMPG8Norm$class+1)
par(mfrow=c(1,1))
