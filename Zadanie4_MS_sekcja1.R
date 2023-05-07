# Dane
wyniki <- read.csv("data/data.csv")
data <- subset(wyniki, Group == 2)$Score

# Liczność próby
n <- length(data)

# Średnia arytmetyczna próby
x_mean <- mean(data)

# Estymator odchylenia standardowego próby
s <- sqrt((1/(n-1)) * sum((data - x_mean)^2))

# Wartość hipotetyczna
sigma <- 20

# Statystyka testowa
chi_square <- (n - 1) * s^2 / sigma^2

# Stopnie swobody
df <- n - 1
print(df)

# Wartość p
p_value <- 1 - pchisq(chi_square, df)
print(p_value)

# Wynik testu hipotezy
if(p_value > 0.05){
  cat("Odrzucamy hipotezę zerową, odchylenie standardowe nie wynosi 20.")
}else{
  cat("Nie ma podstaw do odrzucenia hipotezy zerowej, odchylenie standardowe wynosi 20.")
}


