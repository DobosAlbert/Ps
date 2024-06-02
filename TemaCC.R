generate_random_permutation <- function(n) {
  U <- runif(n)
  
  permutation <- order(U)
  
  return(permutation)
}

generate_and_sort_bitstrings <- function(n, k) {
  # n șiruri de biți de lungime k
  bitstrings <- replicate(n, paste0(sample(c(0, 1), k, replace = TRUE), collapse = ""))
  
  #sortare șirurile de biți
  sorted_bitstrings <- sort(bitstrings)
  
  return(sorted_bitstrings)
}

# Ex
set.seed(123) 
n <- 10
k <- 5
sorted_bitstrings <- generate_and_sort_bitstrings(n, k)
print(sorted_bitstrings)

b.
compare_lexicographically <- function(word1, word2) {
  min_length <- min(nchar(word1), nchar(word2))  # Lungimea minimă a cuvintelor
  
  for (l in 1:min_length) {
    if (substr(word1, 1, l) < substr(word2, 1, l)) {
      return("lexicografic strict mai mic")
    } else if (substr(word1, 1, l) == substr(word2, 1, l)) {
      # Generăm biți pana cand e unu strict mai mic
      while (TRUE) {
        random_bit1 <- sample(c(0, 1), 1, replace = TRUE)
        random_bit2 <- sample(c(0, 1), 1, replace = TRUE)
        word1 <- paste0(word1, random_bit1)
        word2 <- paste0(word2, random_bit2)
        if (substr(word1, 1, l+1) < substr(word2, 1, l+1)) {
          return("lexicografic strict mai mic")
        } else if (substr(word1, 1, l+1) > substr(word2, 1, l+1)) {
          return("lexicografic strict mai mare")
        }
      }
    }
  }
  
  return("egalitate")
}

# Ex
word1 <- "101"
word2 <- "1001"
result <- compare_lexicographically(word1, word2)
print(result)

c.
# Funcție pentru partajarea listei
partition <- function(arr, low, high) {
  pivot <- arr[[high]]
  i <- low - 1
  
  for (j in low:(high - 1)) {
    if (compare_lexicographically(arr[[j]], pivot) == "lexicografic strict mai mic" ||
        compare_lexicographically(arr[[j]], pivot) == "egalitate") {
      i <- i + 1
      temp <- arr[[i]]
      arr[[i]] <- arr[[j]]
      arr[[j]] <- temp
    }
  }
  temp <- arr[[i + 1]]
  arr[[i + 1]] <- arr[[high]]
  arr[[high]] <- temp
  
  return (i + 1)
}

# pivotul și sortarea recursivă a sublistelor
randomized_quick_sort <- function(arr, low, high) {
  if (low < high) {
    random_index <- sample(low:high, 1)
    
    #pivotul la final
    temp <- arr[[high]]
    arr[[high]] <- arr[[random_index]]
    arr[[random_index]] <- temp
    
    pivot_index <- partition(arr, low, high)
    
    # Sortam
    randomized_quick_sort(arr, low, pivot_index - 1)
    randomized_quick_sort(arr, pivot_index + 1, high)
  }
}

randomized_quick_sort_words <- function(words) {
  randomized_quick_sort(words, 1, length(words))
}

#ex
set.seed(123)  #reproductibilitate
words <- c("100", "011", "101", "001", "110")
randomized_quick_sort_words(words)
print(words)

d.
#cuvinte k
generate_words <- function(n, k) {
  words <- vector(mode = "list", length = n)
  for (i in 1:n) {
    word <- paste(sample(c(0, 1), k, replace = TRUE), collapse = "")
    words[[i]] <- word
  }
  return(words)
}

# l n
random_permutation <- function(n) {
  permutation <- sample(1:n)
  return(permutation)
}

sort_words_and_return_permutation <- function(words) {
  randomized_quick_sort_words(words)
  permutation <- order(words)
  return(permutation)
}

# l k cuv
random_permutation_of_words <- function(n, k) {
  # Generăm cuvintele
  words <- generate_words(n, k)
  permutation <- sort_words_and_return_permutation(words)
  return(permutation)
}

set.seed(123) 
n <- 5
k <- 3
permutation <- random_permutation_of_words(n, k)
print(permutation)
