analyze_grades = function(file_path) {
  
  data = read.csv("note_PS.csv", header = TRUE, sep = ',')
  
  P_grades = data[['P']]
  S_grades = data[['S']]
  
  #frecvențele absolute pt note
  abs_freq_P = table(P_grades)
  abs_freq_S = table(S_grades)
  
  #frecvențe relative note
  rel_freq_P = as.vector(abs_freq_P) / length(P_grades)
  rel_freq_S = as.vector(abs_freq_S) / length(S_grades)
  
  # medie
  mean_P = mean(P_grades)
  mean_S = mean(S_grades)
  
  result = list(
    abs_freq_P = abs_freq_P,
    abs_freq_S = abs_freq_S,
    rel_freq_P = rel_freq_P,
    rel_freq_S = rel_freq_S,
    mean_P = mean_P,
    mean_S = mean_S
  )

  return(result)
}

file_path = "note_PS.csv"

grades_analysis = analyze_grades(file_path)

print(grades_analysis)

#B
remove_outliers_and_plot = function(file_path, sample_name) {
  data = read.csv("note_PS.csv", header = TRUE, sep = ',')
  
  #coloana sample_name
  sample_data = data[[sample_name]]
  
  Q1 = quantile(sample_data, 0.25)
  Q3 = quantile(sample_data, 0.75)
  
  # Calcularea intervalului interquartilar (IQR)
  IQR = Q3 - Q1
  
  #limitele valorilor aberante
  lower_bound = Q1 - 1.5 * IQR
  upper_bound = Q3 + 1.5 * IQR
  
  #valori aberante
  outliers = sample_data[sample_data < lower_bound | sample_data > upper_bound]
  
  # Eliminarea valorilori
  clean_sample = sample_data[!(sample_data %in% outliers)]
  
  # Crearea histograme
  hist(clean_sample, breaks = seq(1, 11, by = 1), main = "Frequency", xlab = sample_name, ylab = "Frequency", col = "skyblue", border = "black")
}

file_path = "note_PS.csv"
sample_name = "P"
remove_outliers_and_plot(file_path, sample_name)
