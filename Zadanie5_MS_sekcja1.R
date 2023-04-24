# Wczytanie danych z pliku .csv
wyniki <- read.csv("data/data.csv")

# Podział na grupy
grupa1 <- subset(wyniki, Group == 1)$Score
grupa2 <- subset(wyniki, Group == 2)$Score

# Test t-Studenta
ttest <- t.test(grupa1, grupa2, alternative = "greater", var.equal = TRUE, conf.level = 0.95)

# Sprawdzenie, czy istnieje istotna różnica między średnimi wynikami
if (ttest$p.value < 0.05) {
  cat("Studenci z grupy 1 przygotowali się lepiej do sprawdzianu niż studenci z grupy 2.\n")
} else {
  cat("Studenci z grupy 2 przygotowali się lepiej do sprawdzianu niż studenci z grupy 1 lub nie ma istotnej różnicy między średnimi wynikami.\n")
}
