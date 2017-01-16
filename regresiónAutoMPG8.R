#Leemos dataset (***PATH AL DATASET***)
autoMPG8 <- read.delim("~/R/ICC_TrabajoFinal/autoMPG8/autoMPG8.dat", header = F, sep=",", skip=12, as.is=TRUE);
names(autoMPG8) <- c("Cylinders", "Displacement", "Horse_power", "Weight", "Acceleration", "Model_year", "Origin", "Mpg")

#Normalización
normalize <- function(x) {
  (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
}
autoMPG8Norm <- as.data.frame(lapply(autoMPG8, normalize))

cor(autoMPG8Norm)

#Ajutamos el modelo lineal
fit1=lm(autoMPG8Norm$Mpg~autoMPG8Norm$Cylinders, data=autoMPG8Norm)
fit2=lm(autoMPG8Norm$Mpg~autoMPG8Norm$Displacement, data=autoMPG8Norm)
fit3=lm(autoMPG8Norm$Mpg~autoMPG8Norm$Horse_power, data=autoMPG8Norm)
fit4=lm(autoMPG8Norm$Mpg~autoMPG8Norm$Weight, data=autoMPG8Norm)
fit5=lm(autoMPG8Norm$Mpg~autoMPG8Norm$Model_year, data=autoMPG8Norm)
#Visualizamos estadísticos básicos del modelo
summary(fit5)

#Modelos con regresión lienal simple
par(mfrow=c(3,2))
plot(Mpg~Cylinders, autoMPG8Norm, pch=19, cex=0.6, col="grey")
abline(fit1,col="red")
plot(Mpg~Displacement, autoMPG8Norm, pch=19, cex=0.6, col="grey")
abline(fit2,col="red")
plot(Mpg~Horse_power, autoMPG8Norm, pch=19, cex=0.6, col="grey")
abline(fit3,col="red")
plot(Mpg~Weight, autoMPG8Norm, pch=19, cex=0.6, col="grey")
abline(fit4,col="red")
plot(Mpg~Model_year, autoMPG8Norm, pch=19, cex=0.6, col="grey")
abline(fit5,col="red")
par(mfrow=c(1,1))

#modelo lineal múltiple
fitMulti1=lm(Mpg~Cylinders+Displacement+Horse_power+Weight+Model_year, data=autoMPG8Norm)
summary(fitMulti1)
fitMulti2=lm(Mpg~Weight+Model_year, data=autoMPG8Norm)
summary(fitMulti2)

fitMulti3=lm(Mpg~Cylinders*Displacement*Horse_power*Weight + Cylinders + Displacement + Horse_power + Weight + Model_year, 
             data=autoMPG8Norm)
summary(fitMulti3)
fitMulti3.1=lm(Mpg~ Displacement*Horse_power*Weight*Model_year + Cylinders + Displacement + Horse_power + Weight + Model_year, 
               data=autoMPG8Norm)
summary(fitMulti3.1)
fitMulti3.2=lm(Mpg~ I(Model_year^3) + I(Model_year^2) + Displacement*Horse_power*Weight*Model_year + Cylinders + Displacement + Horse_power + Weight + Model_year, 
             data=autoMPG8Norm)
summary(fitMulti3.2)

fitMulti4=lm(Mpg~ I(Weight^2) + I(Model_year^2) + Weight*Model_year + Weight + Model_year, data=autoMPG8Norm)
summary(fitMulti4)

#Knn
require(kknn)

fitknn <- kknn(Mpg~., autoMPG8, autoMPG8)
yprime = fitknn$fitted.values
sqrt(sum((autoMPG8$Mpg-yprime)^2)/length(yprime))

fitknn <- kknn(Mpg~I(Model_year^3) + I(Model_year^2) + Displacement*Horse_power*Weight*Model_year + Cylinders + Displacement + 
                 Horse_power + Weight + Model_year, autoMPG8, autoMPG8)
yprime = fitknn$fitted.values
sqrt(sum((autoMPG8$Mpg-yprime)^2)/length(yprime))


#COMPARATIVA
#K-fold CROSS VALIDATION
nombre <- "~/R/ICC_TrabajoFinal/autoMPG8/autoMPG8"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=lm(Y~.,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
mean(sapply(1:5,run_lm_fold,nombre,"train"))
mean(sapply(1:5,run_lm_fold,nombre,"test"))

run_knn_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=kknn(Y~.,x_tra,test)
  fitMulti
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
mean(sapply(1:5,run_knn_fold,nombre,"train"))
mean(sapply(1:5,run_knn_fold,nombre,"test"))


#Tabla resultados
tablatst <- data.frame(LM=11.400, KNN=8.107, m5p=8.350)
rownames(tablatst) <-"autoMPG8"

#Wilcoxon test
    #Normalizamos la diferencias entre algoritmos LM y KNN
difs<-(tablatst[,1] -tablatst[,2]) / tablatst[,1]
#Aplicamos el test de Wilcoxon, calculamos primeros los valores pertenecientes a R+ y R-
wilc_1_2 <-cbind(ifelse(difs<0, abs(difs)+0.1, 0+0.1), ifelse(difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <-c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

    #Obtenemos valores totales
LMvsKNNtst<-wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
LMvsKNNtst$statistic #R+
pvalue<-LMvsKNNtst$p.value
LMvsKNNtst<-wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
LMvsKNNtst$statistic #R-

confianza <- (1-pvalue)*100
paste(round(confianza, 2), "%")

#Friedman test
test_friedman<-friedman.test(as.matrix(tablatst))
test_friedman

#Post-hoc Holm test
groups <-rep(1:dim(tablatst)[2], each=dim(tablatst)[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust= "holm", paired = TRUE)
