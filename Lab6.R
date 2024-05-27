
#II.6
selection_mean = function(filename)
  {
  alfa = 0.05
  x = scan(filename)

  medie = mean(x)
  n = length(x)

  sigma = 5
  critical_z = qnorm(1 - alfa / 2, 0, 1)
  
  a = medie - critical_z * sigma / sqrt(n)
  b = medie + critical_z * sigma / sqrt(n)
  rez = c(a, b)
  
  return(rez)
}
selection_mean("history.txt")


#III.4
calculate_confidence_intervals = function(filename) 
  {
  alfa1 = 0.05
  alfa2 = 0.01

  x = scan(filename)
  
  sample_mean = mean(x)
  s = sd(x)
  lg = length(x)
  se = s / sqrt(n)
  
  critical_t1 <- qt(1 - alfa1 / 2, lg - 1)
  critical_t2 <- qt(1 - alfa2 / 2, lg - 1)
  
  a1 = sample_mean - critical_t1 * se
  b1 = sample_mean + critical_t1 * se
  interval_95 <- c(a1, b1)

  a2 = sample_mean - critical_t2 * se
  b2 = sample_mean + critical_t2 * se
  interval_99 = c(a2, b2)
  
 interval_95
 interval_99
}
