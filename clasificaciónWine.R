#Leemos dataset (***PATH AL DATASET***) y asigno nombres de cada característica
wine <- read.delim("~/R/ICC_TrabajoFinal/wine/wine.dat", header = F, sep=",", skip=18, as.is=TRUE);
names(wine) <- c("Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", "TotalPhenols", "flavanoids", 
                 "NonflavanoidsPhenols", "Proanthocyanins", "ColorIntensity", "Hue", "OD280/OD315", 
                 "Proline", "Class");
attach(wine)

#Normalización
normalize <- function(x) {
  (x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
}
wineN<-as.data.frame(lapply(wine, normalize))

#Comprobamos no alteración de datos
plot(wine$AlcalinityOfAsh, wine$MalicAcid, main="Plot AlcalinityOfAsh vs MalicAcid pre-normalización", pch=17, 
     xlab="AlcalinityOfAsh", ylab="MalicAcid", col="darkgray")
plot(wineN$AlcalinityOfAsh, wineN$MalicAcid, main="Plot AlcalinityOfAsh vs MalicAcid pos-normalización", pch=17,
     xlab="AlcalinityOfAsh", ylab="MalicAcid", col="darkblue")

#Correlación entre variables
cor(wine)


############## KNN ###################
library(class)

#K-fold CROSS VALIDATION automático para WINE (normaliza los conjuntos de train y test)
run_knn_fold <- function(i, kn) {
  path="~/R/ICC_TrabajoFinal/wine/"
  file <- paste(path, "wine-10-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@", header=FALSE)
  x_tra<-as.data.frame(lapply(x_tra, normalize))
  file <- paste(path, "wine-10-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@", header=FALSE)
  x_tst<-as.data.frame(lapply(x_tst, normalize))
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  
  fitKNN = knn(train=x_tra[,-14], test=x_tst[,-14], cl=x_tra$Y, k=kn)
  accuracy = sum(table(fitKNN, x_tst[,14])[c(1,5,9)])*100 / nrow(x_tst)
}
mean(sapply(1:10,run_knn_fold,3))

accuracyKnn <-data.frame(k=c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21), 
                         pre=c(70.84967, 71.27451, 69.60784, 71.83007, 73.03922, 71.9281, 72.48366, 71.33987, 71.30719, 
                               69.60784, 71.30719, 72.97386, 70.75163, 70.22876, 69.08497, 71.33987, 70.75163, 70.78431, 70.22876), 
                         pos=c(95.52288, 96.07843, 97.18954, 97.7451, 97.18954, 96.07843, 96.07843, 96.07843, 96.63399, 96.07843, 
                               96.07843, 96.07843, 95.52288, 96.07843, 96.07843, 96.63399, 96.07843, 96.07843, 96.07843))

plot(accuracyKnn$k, accuracyKnn$pre, ylim=c(65,100))
plot(accuracyKnn$k, accuracyKnn$pos, ylim=c(65,100))

require(ggplot2)
ggplot(accuracyKnn, aes(x=accuracyKnn$k)) +
  geom_line(aes(y=accuracyKnn$pre), colour="red") +
  geom_line(aes(y=accuracyKnn$pos), colour="green") +
  ylab("Accuracy") + xlab("K") + labs(title="H")


############## LDA ###################
library(MASS)
library(klaR)
library(caret)

attach(wineSC)

#Escalamos y centramos los datos
wineSC = as.data.frame(scale(wine))
summary(wineSC)
#Comprobamos y eliminamos características nzv (near zero variance)
nearZeroVar(wineSC, saveMetrics = T)

#Modelo 1
lda.fit = lda(wineSC$Class ~ wineSC$flavanoids + wineN$OD280.OD315, data=wineSC)
plot(lda.fit, type="p")

lda.pred = predict(lda.fit, wineSC)

resultsLDA = table(lda.pred$class, wineSC$Class)
sum(table(lda.pred$class, wineSC$Class)[c(1,5,9)])*100 / nrow(wineSC)

partimat(Class~flavanoids+wineSC$`OD280/OD315`, data=wineSC ,method="lda")

#Modelo 2
lda.fit2 = lda(wineSC$Class ~ wineSC$flavanoids + wineSC$`OD280/OD315` + wineSC$Proline + wineSC$Hue + wineSC$TotalPhenols, data=wineSC)
plot(lda.fit, type="both")

lda.pred2 = predict(lda.fit2, wineSC)

resultsLDA = table(lda.pred2$class, wineSC$Class)
sum(table(lda.pred2$class, wineSC$Class)[c(1,5,9)])*100 / nrow(wineSC)

partimat(wineSC$Class~wineSC$flavanoids+wineSC$`OD280/OD315`+wineSC$Proline+wineSC$Hue+wineSC$TotalPhenols, data=wineSC ,method="lda")

#Modelo 3
lda.fit3 = lda(wineSC$Class ~ wineSC$flavanoids + wineSC$`OD280/OD315` + wineSC$Proline + wineSC$Hue + wineSC$TotalPhenols + wineSC$Alcohol, data=wineSC)
plot(lda.fit, type="both")

lda.pred3 = predict(lda.fit3, wineSC)

resultsLDA = table(lda.pred3$class, wineSC$Class)
sum(table(lda.pred3$class, wineSC$Class)[c(1,5,9)])*100 / nrow(wineSC)

partimat(wineSC$Class~wineSC$flavanoids+wineSC$`OD280/OD315`+wineSC$Proline+wineSC$Hue+wineSC$TotalPhenols+wineSC$Alcohol, data=wineSC ,method="lda")

############## QDA ###################
apply(wine,2,var)

#Modelo 1
qda.fit=qda(wineSC$Class~wineSC$flavanoids+wineSC$`OD280/OD315`, data=wineSC)
plot(qda.fit, type="both")

qda.pred=predict(qda.fit,wineSC)

resultsQDA = table(qda.pred$class, wineSC$Class)
sum(table(qda.pred$class, wineSC$Class)[c(1,5,9)])*100 / nrow(wineSC)

partimat(wineSC$Class~wineSC$flavanoids+wineSC$`OD280/OD315`, data=wineSC ,method="qda")

#Modelo 2
qda.fit2=qda(wineSC$Class~wineSC$flavanoids+wineSC$`OD280/OD315`+wineSC$Proline+wineSC$Hue+wineSC$TotalPhenols, data=wineSC)
plot(qda.fit2, type="both")

qda.pred2=predict(qda.fit2,wineSC)

resultsQDA = table(qda.pred2$class, wineSC$Class)
sum(table(qda.pred2$class, wineSC$Class)[c(1,5,9)])*100 / nrow(wineSC)

partimat(wineSC$Class~wineSC$flavanoids+wineSC$`OD280/OD315`+wineN$Proline+wineN$Hue+wineN$TotalPhenols, data=wineSC ,method="qda")

#Modelo 3
qda.fit3=qda(wineSC$Class~wineSC$flavanoids+wineSC$`OD280/OD315`+wineSC$Proline+wineSC$Hue+wineSC$TotalPhenols+wineSC$Alcohol, data=wineSC)
plot(qda.fit2, type="both")

qda.pred3=predict(qda.fit3,wineSC)

resultsQDA = table(qda.pred2$class, wineSC$Class)
sum(table(qda.pred3$class, wineSC$Class)[c(1,5,9)])*100 / nrow(wineSC)

partimat(wineSC$Class~wineSC$flavanoids+wineSC$`OD280/OD315`+wineN$Proline+wineN$Hue+wineN$TotalPhenols+wineSC$Alcohol, data=wineSC ,method="qda", nplots.vert=4)


############## COMPARACIÓN ###################
accuracies <-data.frame( knn=c(95.52288, 97.18954, 97.18954, 96.07843),
                          lda=c(83.70787, 94.38202, 94.94382, NA), 
                          qda=c(85.95506, 97.19101, 100, NA))

require(ggplot2)
ggplot(accuracies, aes(x=c(1,2,3,4))) +
  geom_line(aes(y=accuracies$knn), colour="blue") +
  geom_line(aes(y=accuracies$lda), colour="red") +
  geom_line(aes(y=accuracies$qda), colour="green") +
  ylab("Accuracy") + xlab("Model") + labs(title="Comparación Knn vs LDA vs QDA")



