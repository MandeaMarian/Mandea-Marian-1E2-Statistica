

AI = function(lambda,m,n,p,k){
  valori = k:m;
  prob_poisson = dpois(valori,lambda); #Distributa Poisson
  prob_geometric = dgeom(valori - k,p); #Distributia Geometrica
  prob_binomial = dbinom(valori,n,p); #Distributia Binomiala
  print(prob_poisson);
  print(prob_geometric);
  print(prob_binomial);
}

AI(3,10,15,0.5,5)

AII = function(file){
  
  values = read.csv(file);
  
  sample_size = length(values) / 2
  sample1 = sample(values, sample_size)
  sample2 = sample(values, sample_size)
  
  freq_abs1 = table(sample1)
  freq_abs2 = table(sample2)
  
  freq_rel1 = as.vector(prop.table(freq_abs1))
  freq_rel2 = as.vector(prop.table(freq_abs2))
  
  mean1 = mean(sample1)
  mean2 = mean(sample2)
  print(mean1);
  print(mean2);
}

print(file.exists("note_PS.csv"))

#A1c
find_k0 = function(lambda, threshold = 1 - 10^(-6)) {
  k0 = 0
  cumulative_prob = ppois(k0, lambda)
  
  while (cumulative_prob <= threshold) {
    k0 = k0 + 1
    cumulative_prob = ppois(k0, lambda)
  }
  
  return(k0)
}

lambda = 3.5
k0 = find_k0(lambda)
print(paste("The smallest value of k0 for which P(Y <= k0) > 1 - 10^-6 is:", k0))