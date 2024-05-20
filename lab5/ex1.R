
valoare = function(matrice)
  {
  suma= 0
  for(col in 1:ncol(matrice)) 
  {
    p = matrice[2, col]
    if(p < 0) 
      stop("Probabilitățile nu pot fi negative")
    suma= suma+ p
  }
  
  if(suma!= 1) 
    stop("Suma probabilitatilor trebuie sa fie 1")
  
  aleator = runif(1, 0, 1)
  rest = 0
  idex = -1
  
  for(col in 1:ncol(matrice)) 
    {
    rest = rest + matrice[2, col]
    if(aleator <= rest)
    {
      index = col
      break
    }
  }
  if (index == -1) 
    stop("Nu s-a gasit un indice valid.")
  
  return(matrice[1, index])
}

valori_probabilitati = c(1, 0.1, 2, 0.3, 3, 0.3, 4, 0.3)

rez = matrix(valori_probabilitati, 2, 4)
valoare(rez)
