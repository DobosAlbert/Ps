calculate_probabilities = function(lambda, p, n, m, k) {
  
  # Definim intervalul de valori de la k la m
  values = k:m
  
  # probabilitățile pentru distribuția Poisson 
  poisson_probs = dpois(values, lambda)
  
  #distribuția geometrică
  geom_probs = dgeom(values, p)
  
  # distribuția binomială
  binom_probs = dbinom(values, n, p)
  
  # Creăm o listă cu toate probabilitățile calculate
  result = list(
    Poisson = poisson_probs,
    Geometric = geom_probs,
    Binomial = binom_probs
  )
  
  return(result)
}
#graficul de bare pentru distribuția Poisson
Poisson = function(n, p) {
  x = 0:(n-1)
  
  #probabilitățile Poisson
  y = dpois(x, p)
  
  #graficul de bare
  barplot(y, space = 0, main = 'barplot', sub = "poisson", xlab = "axa x", ylab = "axa y")
}

#graficul de bare pentru distribuția geometrică
Geometric = function(n, p) {
  # Definim valorile x de la 0 la n-1
  x = 0:(n-1)
  
  # Calculăm probabilitățile geometrice pentru valorile x
  y = dgeom(x, p)
  
  # Afișăm graficul de bare
  barplot(y, space = 0, main = 'barplot', sub = "geometric", xlab = "axa x", ylab = "axa y")
}

#graficul de bare pentru distribuția binomială
Binomial = function(n, p) {
  # Definim valorile x de la 0 la n
  x = 0:n
  
  #probabilitățile binomiale
  y = dbinom(x, n, p)
  
  #graficul de bare
  barplot(y, space = 0, main = 'barplot', sub = "binomial", xlab = "Values", ylab = "Probability")
}

#valorii minime k0 pentru distribuția Poisson
minim = function(lambda) {
  
  k0 = 0
  
  # probabilitatea cumulativă a distribuției Poisson și creștem k0
  while (ppois(k0, lambda) <= 1 - 10^(-6)) {
    k0 = k0 + 1
  }
  return(k0)
}

