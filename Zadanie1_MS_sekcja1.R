#Wczytanie danych z pliku .csv
wyniki <- read.csv("data/data.csv")

#Podział na grupy
grupa1 <- subset(wyniki,Group == 1)$Score
grupa2 <- subset(wyniki,Group == 2)$Score

#Wyznaczenie miar położenia:

#Średnia
srednia_grupa1 <- mean(grupa1)
srednia_grupa2 <- mean(grupa2)

#Mediana
mediana_grupa1 <- median(grupa1)
mediana_grupa2 <- median(grupa2)

#Moda
moda <- function(grupa){
  uniqv <-unique(grupa)
  uniqv[which.max(tabulate(match(grupa,uniqv)))]
}

#Kwantyle

kwantyle_grupa1 <- quantile(grupa1,c(0.25,0.75))
kwantyle_grupa2 <- quantile(grupa2,c(0.25,0.75))

#Wyznaczenie miar zróżnicowania:

#Wariancja obciążona s^2

wariancja_grupa1 <- var(grupa1)
wariancja_grupa2 <- var(grupa2)

#Odchylenie standardowe

od_grupa1 <- sd(grupa1)
od_grupa2 <- sd(grupa2)

#Wariancja nieobciążona

wariancja_nieobc_grupa1 <- var(grupa1)*(length(grupa1)/(length(grupa1)-1))
wariancja_nieobc_grupa2 <- var(grupa2)*(length(grupa2)/(length(grupa2)-1))

#Rozstęp

rozstep_grupa1 <- max(grupa1) - min(grupa1)
rozstep_grupa2 <- max(grupa2) - min(grupa2)

#Odchylenie standardowe s*
od_nieobc_grupa1 <- sqrt(wariancja_nieobc_grupa1)
od_nieobc_grupa2 <- sqrt(wariancja_nieobc_grupa2)

#Współczynnik zmienności klasyczny Vs
wsp_zmiennosci_grupa1 <- (sd(grupa1)/mean(grupa1))*100
wsp_zmiennosci_grupa2 <- (sd(grupa1)/mean(grupa2))*100

#Klasyczny przedział zmienności
klasyczny_przedział_grupa1 <- c(mean(grupa1) - 2*sd(grupa1), mean(grupa1) + 2*sd(grupa1))
klasyczny_przedział_grupa2 <- c(mean(grupa2) - 2*sd(grupa2), mean(grupa2) + 2*sd(grupa2))

#Miary zróżnicowania pozycyjne:


#Rozstęp międzykwartylowy

rozstep_mdzq_grupa1 <- IQR(grupa1)
rozstep_mdzq_grupa2 <- IQR(grupa2)

#Odchylenie ćwiartkowe

odch_cw_grupa1 <- (kwantyle_grupa1[2] - kwantyle_grupa1[1])/2 
odch_cw_grupa2 <- (kwantyle_grupa2[2] - kwantyle_grupa2[1])/2

#Pozycyjny przedział zmienności:
pozycyjny_prz_zm_grupa1 <- function(grupa1) {
  mediana <- median(grupa1)
  oq <- (kwantyle_grupa1[2] - kwantyle_grupa1[1])/2
  pozycyjny_przedzial <- c(mediana - oq,mediana + oq) 
  return(pozycyjny_przedzial)
}
pozycyjny_prz_zm_grupa2 <- function(grupa2) {
  mediana <- median(grupa2)
  oq <- (kwantyle_grupa2[2] - kwantyle_grupa2[1])/2
  pozycyjny_przedzial <- c(mediana - oq,mediana + oq) 
  return(pozycyjny_przedzial)
}
  
#Współczynnik zmienności (pozycyjny) 𝑉𝑞:
wsp_zmiennosci_pozycyjny1 <- function(grupa1) {
  oq <- (kwantyle_grupa1[2] - kwantyle_grupa1[1])/2
  mediana <- median(grupa1)
  wsp_zmiennosci <- (oq/mediana) * 100
  return(wsp_zmiennosci)
}

wsp_zmiennosci_pozycyjny2 <- function(grupa2) {
  oq <- (kwantyle_grupa2[2] - kwantyle_grupa2[1])/2
  mediana <- median(grupa2)
  wsp_zmiennosci <- (oq/mediana) * 100
  return(wsp_zmiennosci)
}

#Miary asymetrii:

#Klasyczny współczynnik asymetrii As
As_grupa1 <- function(grupa1){
  n <- length(grupa1)
  m <- mean(grupa1)
  s <- sd(grupa1)
  skewness <- sum((grupa1 - m)^3)/(n*s^3)
  return(skewness)
}
As_grupa2 <- function(grupa2){
  n <- length(grupa2)
  m <- mean(grupa2)
  s <- sd(grupa2)
  skewness <- sum((grupa2 - m)^3)/(n*s^3)
  return(skewness)
}

#Miary koncentracji:

#Kurtoza
library(moments)
kurtoza_grupa1 <- kurtosis(grupa1)
kurtoza_grupa2 <- kurtosis(grupa2)

#Wyznaczenie szeregów i histogramów dla grupy 1

#Szereg szczegółowy 

szereg_szcz_grupa1 <- table(grupa1)

# Liczba przedziałów
n <- 6

# Szereg rozdzielczy
szereg_rozdz_grupa1 <- cut(grupa1, breaks = n, right = FALSE, dig.lab = 4)

hist(rep(as.numeric(names(szereg_szcz_grupa1)), szereg_szcz_grupa1),col = "lightgreen", main = "Histogram wyników grupy 1 sz. szczegółowy",
     xlab = "Wyniki sprawdzianu", ylab = "Częstość")

# Wyznaczenie histogramu szeregu rozdzielczego
hist(grupa1, breaks = n, col = "lightyellow", main = "Histogram wyników grupy 1 sz. rozdzielczy",
     xlab = "Wyniki sprawdzianu", ylab = "Częstość")

#Przedstawienie szeregu rozdzielczego na wykresie
rug(as.numeric(szereg_rozdz_grupa1), col = "red")

#Wyznaczenie szeregów i histogramów dla grupy 2

#Szereg szczegółowy 

szereg_szcz_grupa2 <- table(grupa2)

# Liczba przedziałów
n <- 6

# Szereg rozdzielczy
szereg_rozdz_grupa2 <- cut(grupa2, breaks = n, right = FALSE, dig.lab = 4)

hist(rep(as.numeric(names(szereg_szcz_grupa2)), szereg_szcz_grupa2),col = "lightgreen", main = "Histogram wyników grupy 2 sz. szczegółowy",
     xlab = "Wyniki sprawdzianu", ylab = "Częstość")

# Wyznaczenie histogramu szeregu rozdzielczego
hist(grupa2, breaks = n, col = "lightyellow", main = "Histogram wyników grupy 2 sz. rozdzielczy",
     xlab = "Wyniki sprawdzianu", ylab = "Częstość")

#Przedstawienie szeregu rozdzielczego na wykresie
rug(as.numeric(szereg_rozdz_grupa2), col = "red")
