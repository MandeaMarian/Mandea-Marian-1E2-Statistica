setwd('C:\\PS\\Tema A')
  AI = function(lambda,m,n,p,k){
  valori = k:m;
  prob_poisson = dpois(valori,lambda);
  prob_geometric = dgeom(valori - k,p);
  prob_binomial = dbinom(valori,n,p);
  print(prob_poisson);
  print(prob_geometric);
  print(prob_binomial);
}

AI(3,10,15,0.5,5)


AII = function(file) {
  if (!file.exists(file)) {
    stop("File does not exist.")
  }
  

  data = read.csv(file)
  
  
  sample_size = nrow(data) / 2
  sample1 = sample(data$P, sample_size)
  sample2 = sample(data$S, sample_size)
  
  
  freq_abs1 = table(sample1)
  freq_abs2 = table(sample2)
  
 
  freq_rel1 = as.vector(prop.table(freq_abs1))
  freq_rel2 = as.vector(prop.table(freq_abs2))
  
  
  mean1 = mean(sample1)
  mean2 = mean(sample2)
  

  print("Sample 1 (P):")
  print(sample1)
  print("Absolute Frequencies 1 (P):")
  print(freq_abs1)
  print("Relative Frequencies 1 (P):")
  print(freq_rel1)
  print("Mean 1 (P):")
  print(mean1)
  
  print("Sample 2 (S):")
  print(sample2)
  print("Absolute Frequencies 2 (S):")
  print(freq_abs2)
  print("Relative Frequencies 2 (S):")
  print(freq_rel2)
  print("Mean 2 (S):")
  print(mean2)
}

AII("note_PS.csv")


