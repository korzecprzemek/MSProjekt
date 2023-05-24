#Zadanie 4

# Dane
wyniki <- read.csv("data/data.csv")
data <- subset(wyniki, Group == 2)$Score

n <- length(data)
srednia <- mean(data)
wariancja <- sum((data - srednia)^2) / (n-1)
p_rozklad_975 <- qchisq(1 - 0.05/ 2, n-1, lower.tail = TRUE)
p_rozklad_025 <- qchisq(0.05/ 2, n-1, lower.tail = TRUE)

# Statystyka testowa
u <- (n * wariancja) / (20^2)

# Sprawdzamy czy statystyka testowa nalezy do obszaru krytycznego
if (u > p_rozklad_025 & u < p_rozklad_975) {
  cat("Odrzucamy hipotezę zerową, odchylenie standardowe nie wynosi 20.")
}else{
  cat("Nie ma podstaw do odrzucenia hipotezy zerowej, odchylenie standardowe wynosi 20.")
}