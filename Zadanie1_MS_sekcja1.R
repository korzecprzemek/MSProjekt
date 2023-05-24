# Zadanie 1
#Instalacja wymaganych bibliotek
#install.packages("moments")
#install.packages("tidyverse")
library(moments)
library(tidyverse)

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
#library(moments)
kurtoza_grupa1 <- kurtosis(grupa1)
kurtoza_grupa2 <- kurtosis(grupa2)

#Miary dla szeregu rozdzielczego przedziałowego
#library(tidyverse)
oblicz_kwantyl <- function(df, q_name, q_pos_formula){
  df %>%
    group_by(Group) %>%
    mutate(right_cum = cumsum(n),
           q_pos = eval(rlang::parse_expr(q_pos_formula)),
           left_cum = tidyr::replace_na(lag(right_cum,1),0) +1,
           cond = q_pos <= right_cum & q_pos >= left_cum,
           n_lag1 = lag(right_cum, 1),
           {{ q_name }} := bin_min + (q_pos - n_lag1)/n*bin_length) %>%
    filter(cond)
}
#Rysowanie histogramu dla szeregu rozdzielczego przedziałowego
draw_hist <- function(df, grupa, kolor){
  df %>%
    filter(Group == grupa) %>%
    ggplot(aes(x=bin, y=n)) + 
    geom_col(fill = kolor) +
    labs(title = "Histogram dla szeregu rozdzielczego",
         subtitle = paste0("Dane dla grupy ", grupa, "."),
         x="", y="Częstość") +
    theme_minimal()
}
# Definicja szeregu rozdzielczego
n <- 6
szereg_rozdzielczy <- wyniki %>%
  group_by(Group) %>%
  mutate(bin = cut(Score, breaks = n, right = FALSE, dig.lab = 4)) %>%
  count(Group, bin) %>%
  ungroup()

draw_hist(szereg_rozdzielczy, grupa = 1, kolor = "lightgreen") 
draw_hist(szereg_rozdzielczy, grupa = 2, kolor = "lightblue")

# dodanie informacji o przedzialach (bins)
szereg_rozdzielczy2 <- szereg_rozdzielczy %>%
  rowwise() %>%
  mutate(bins = bin %>% 
           as.character() %>% 
           stringr::str_remove_all(pattern = "\\[|\\)") %>% 
           stringr::str_split_fixed(pattern = ",", n=2) %>% 
           as.numeric() %>% list(),
         bin_min = unlist(bins) %>% min(),
         bin_max = unlist(bins) %>% max(),
         bin_length = bin_max - bin_min,
         bin_avg = unlist(bins) %>% mean())  
#srednia, mediana, wariancja, odchylenie, moment 4 i inne
szereg_rozdzielczy2 %>% 
  group_by(Group) %>%
  summarise(srednia_wazona = sum(n*bin_avg) / sum(n),
            wariancja = sum(((bin_avg- srednia_wazona)^2)*n) /sum(n),
            odch_std = sqrt(wariancja),
            moment_4 = sum(((bin_avg- srednia_wazona)^4)*n) /sum(n),
            kurtoza = moment_4 / wariancja^2,
            `skośność` = sum(((bin_avg- srednia_wazona)^3)*n) /sum(n),
            `rozstęp` =  max(bin_max) - min(bin_min),
            `wsp. zmienności` = odch_std/srednia_wazona) %>%
  left_join(
    oblicz_kwantyl(szereg_rozdzielczy2, q_name = "mediana", q_pos_formula = "(sum(n) +1)/2") %>%
      select(Group, mediana)
  ) %>% 
  left_join(
    oblicz_kwantyl(szereg_rozdzielczy2, q_name = "q1", q_pos_formula = "sum(n)/4") %>%
      select(Group, q1)
  ) %>%
  left_join(
    oblicz_kwantyl(szereg_rozdzielczy2, q_name = "q3", q_pos_formula = "3*sum(n)/4") %>%
      select(Group, q3)
  ) %>%
  left_join(
    szereg_rozdzielczy2 %>%
      group_by(Group) %>%
      mutate(n_prev = lag(n,1),
             n_next = lead(n, 1)) %>%
      filter(n == max(n)) %>%
      mutate(dominanta = bin_min + (n - n_prev)/(n - n_prev + n - n_prev)*bin_length) %>% 
      select(Group, Dominanta = dominanta)
  ) %>%
  mutate(`odchylenie ćwiartkowe` = (q3 - q1)/2,
         `pozycyjny wsp. zmiennosci` = `odchylenie ćwiartkowe` / mediana) %>%view() 
  #readr::write_csv2("data/zadanie_1_rozdzielczy_miary.csv")
