estimate_torus_volume <- function(num_points, R, r) {
  count_inside = 0
  
  # num_points interiorul torului
  for (i in 1:num_points) {
    x1 <- runif(1, -R-r, R+r) 
    x2 <- runif(1, -R-r, R+r)
    x3 <- runif(1, -r, r) 
    
    if (is_point_inside_torus(x1, x2, x3, R, r)) {
      count_inside = count_inside + 1 #sa fie punctu in interior
    }
  }
  
  volume_box = 8 * (R + r)^2 * r # Volumul dreptunghiului torului
  volume_torus = (count_inside / num_points) * volume_box
  return(volume_torus)
}

R <- 10
r <- 3
num_points_values <- c(10000, 20000, 50000)


for (num_points in num_points_values) {
  estimated_volume <- estimate_torus_volume(num_points, R, r)
  exact_volume <- 2 * pi^2 * R * r^2 
  absolute_error <- abs(estimated_volume - exact_volume)
  relative_error <- absolute_error / exact_volume
  
  cat("Estimarea volumului pentru N =", num_points, ":", estimated_volume, "\n")
  cat("Volumul exact al torului:", exact_volume, "\n")
  cat("Eroarea absolută pentru N =", num_points, ":", absolute_error, "\n")
  cat("Eroarea relativă pentru N =", num_points, ":", relative_error, "\n")
}

#B2
estimate_triangle_area <- function(num_points) {
  count_inside = 0 
  
  #verificam sa fie in interiorul triunghiului
  for (i in 1:num_points) {
    x <- runif(1, -1, 0) 
    y <- runif(1, -1.9, 0)
    
    if (is_point_inside_triangle(x, y)) {
      count_inside = count_inside + 1 
    }
  }
  
  #aria triunghiulului
  area_box = 1 * 1.7 
  area_triangle = (count_inside / num_points) * area_box
  return(area_triangle)
}

estimated_area = estimate_triangle_area(10000)
print(estimated_area)

#B3 a
estimate_integral_a <- function(num_points) {
  sum_values = 0
  
  for (i in 1:num_points) {
    x <- runif(1, -1, 1) #aleator [-1, 1]
    sum_values = sum_values + (2*x - 1) / (x^2 - x - 6)
  }
  
  integral_value = 2 * sum_values / num_points
  return(integral_value)
}

num_points <- 20000

estimated_integral <- estimate_integral_a(num_points)
cat("Estimarea integralei:", estimated_integral, "\n")

exact_integral <- log(3) - log(2)
cat("Valoarea exactă a integralei:", exact_integral, "\n")

absolute_error <- abs(estimated_integral - exact_integral)
relative_error <- absolute_error / abs(exact_integral)
cat("Eroarea absolută:", absolute_error, "\n")
cat("Eroarea relativă:", relative_error, "\n")

#B3 b
# metoda Monte Carlo
estimate_integral_b <- function(num_points) {
  sum_values = 0
  
  for (i in 1:num_points) {
    x <- runif(1, 3, 11) #aleator [3, 11]
    sum_values = sum_values + (x + 4) / ((x - 3)^(1/3))
  }
  
  integral_value = 8 * sum_values / num_points #lungime 8 la intervall
  return(integral_value)
}

num_points <- 20000

estimated_integral <- estimate_integral_b(num_points)
cat("Estimarea integralei:", estimated_integral, "\n")

exact_integral <- 61.2
cat("Valoarea exactă a integralei:", exact_integral, "\n")

absolute_error <- abs(estimated_integral - exact_integral)
relative_error <- absolute_error / abs(exact_integral)
cat("Eroarea absolută:", absolute_error, "\n")
cat("Eroarea relativă:", relative_error, "\n")

#B3 c
f <- function(x) {
  return(x * exp(-x^2))
}

#distribuția exponențială
estimate_integral_c <- function(num_points) {
  sum_values = 0
  
  for (i in 1:num_points) {
    u <- runif(1, 0, 1) # [0, 1]
    x <- -log(1 - u) #punctul în coordonata x folosind distribuția exponențială
    sum_values = sum_values + f(x)
  }
  
  integral_value = sum_values / num_points
  return(integral_value)
}

num_points <- 20000

estimated_integral <- estimate_integral_c(num_points)
cat("Estimarea integralei:", estimated_integral, "\n")

exact_integral <- 1/2
cat("Valoarea exactă a integralei:", exact_integral, "\n")

absolute_error <- abs(estimated_integral - exact_integral)
relative_error <- absolute_error / abs(exact_integral)
cat("Eroarea absolută:", absolute_error, "\n")
cat("Eroarea relativă:", relative_error, "\n")

