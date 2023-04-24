# Wczytanie danych z pliku .csv
wyniki <- read.csv("data/data.csv")

# Podział na grupy
grupa2 <- subset(wyniki, Group == 2)$Score

# Test dwustronny t-Studenta
test <- t.test(grupa2, sd = 20, alternative = "two.sided", conf.level = 0.95)

# Wynik testu
print(test)

# Interpretacja wyniku
if (test$p.value < 0.05) {
  cat("Na poziomie istotności 0.05 można stwierdzić, że odchylenie standardowe liczby punktów uzyskanych przez studentów grupy 2 wynosi 20.")
} else {
  cat("Nie ma podstaw do odrzucenia hipotezy zerowej, czyli odchylenie standardowe liczby punktów uzyskanych przez studentów grupy 2 różni się od 20")
}
