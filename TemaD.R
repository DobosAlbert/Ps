#1D
data <- read.csv("probabilitati.csv")

punctaj_mediu <- mean(data$Punctaje)
dispersie <- var(data$Punctaje)

# nr obs
n <- length(data$Punctaje)

interval_95 <- c(punctaj_mediu - 1.96 * sqrt(dispersie/n), punctaj_mediu + 1.96 * sqrt(dispersie/n))

interval_99 <- c(punctaj_mediu - 2.576 * sqrt(dispersie/n), punctaj_mediu + 2.576 * sqrt(dispersie/n))

print("Intervalul de 95%:")
print(interval_95)
print("Intervalul de 99%:")
print(interval_99)

#2D
data <- read.csv("statistica.csv")

#punctajul mediu și deviația
punctaj_mediu <- mean(data$Punctaje)
deviatie_standard <- sd(data$Punctaje)

#nr obs
n <- length(data$Punctaje)

interval_95 <- c(punctaj_mediu - 1.96 * (deviatie_standard/sqrt(n)), punctaj_mediu + 1.96 * (deviatie_standard/sqrt(n)))

interval_99 <- c(punctaj_mediu - 2.576 * (deviatie_standard/sqrt(n)), punctaj_mediu + 2.576 * (deviatie_standard/sqrt(n)))

print("Intervalul de încredere de 95%:")
print(interval_95)
print("Intervalul de încredere de 99%:")
print(interval_99)

#3D

#proporții
z_test_proportions <- function(p1, p2, n1, n2) {
  p <- (p1 * n1 + p2 * n2) / (n1 + n2)
  se <- sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
  z <- (p1 - p2) / se
  return(z)
}

#  voloarea p bilaterala pentru testul Z pentru proporții
p_value_two_tailed <- function(z) {
  p <- 2 * pnorm(-abs(z))
  return(p)
}

n_initial <- 100

n_initial_not_solved <- 14

n_new_not_solved <- 14

#proporțiilor
p_initial <- n_initial_not_solved / n_initial
p_new <- n_new_not_solved / n_initial

# statistic Z
z_stat <- z_test_proportions(p_initial, p_new, n_initial, n_initial)

# valoarea p
p_value <- p_value_two_tailed(z_stat)

cat("Statisticul Z:", z_stat, "\n")
cat("Valoarea p:", p_value, "\n")

# niv semnificație
alpha <- c(0.01, 0.05)

for (a in alpha) {
  cat("nivelul de semnificație", a, ":\n")
  if (p_value < a) {
    cat("Schimbarea a avut un impact\n")
  } else {
    cat("Schimbarea nu a avut un impact semnificativ.\n")
  }
}

