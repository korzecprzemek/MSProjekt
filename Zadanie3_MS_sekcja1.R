# Wczytanie danych z pliku .csv
wyniki <- read.csv("data/data.csv")

# Podział na grupy
grupa1 <- subset(wyniki, Group == 1)$Score

# Test dwustronny t-Studenta
test <- t.test(grupa1, mu = 55, alternative = "two.sided", conf.level = 0.95)

# Wynik testu
print(test)

# Interpretacja wyniku
if (test$p.value < 0.05) {
  cat("Na poziomie istotności 0.05 można stwierdzić, że średnia liczba punktów uzyskanych przez studentów grupy 1 różni się istotnie od 55.")
} else {
  cat("Nie ma podstaw do odrzucenia hipotezy zerowej, czyli średnia liczba punktów uzyskanych przez studentów grupy 1 nie różni się istotnie od 55 na poziomie istotności 0.05.")
}