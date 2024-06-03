
# D1
setwd('C:\\PS\\Tema D')
DI = function(file){
  
  data = read.csv(file);
  
  probabilitati = data$probabilitati
  
  mean_score = mean(probabilitati)
  
  n = length(probabilitati)
  
  sigma2 = 92.16
  sigma = sqrt(sigma2)
  
  
  alfa_95 = 0.05
  z_95 = qnorm(1 - alfa_95 / 2)
  margin_error_95 = z_95 * (sigma / sqrt(n))
  ci_lower_95 = mean_score - margin_error_95
  ci_upper_95 = mean_score + margin_error_95
  
  alfa_99 = 0.01
  z_99 = qnorm(1 - alfa_99 / 2)
  margin_error_99 = z_99 * (sigma / (sqrt(n)))
  ci_lower_99 = mean_score - margin_error_99
  ci_upper_99 = mean_score + margin_error_99
  
  cat("Interval de încredere de 95% pentru punctajul mediu probabilitati: ", ci_lower_95, ",", ci_upper_95, "\n")
  cat("Interval de încredere de 99% pentru punctajul mediu probabilitati: ", ci_lower_99, ",", ci_upper_99, "\n")
  
}

DI("probabilitati.csv")



# D2
DII = function(file){
  
  data = read.csv(file);
  
  statistica = data$statistica
  
  mean_score = mean(statistica)
  
  sd_score = sd(statistica)
  
  n = length(statistica)
  
  alfa_95 = 0.05
  z_95 = qnorm(1 - alfa_95 / 2)
  margin_error_95 = z_95 * (sd_score / sqrt(n))
  ci_lower_95 = mean_score - margin_error_95
  ci_upper_95 = mean_score + margin_error_95
  
  alfa_99 <- 0.01
  z_99 <- qnorm(1 - alfa_99 / 2)
  margin_error_99 = z_99 * (sd_score / sqrt(n))
  ci_lower_99 = mean_score - margin_error_99
  ci_upper_99 = mean_score + margin_error_99
  
  cat("Interval de încredere de 95% pentru punctajul mediu statistica: ", ci_lower_95, ",", ci_upper_95, "\n")
  cat("Interval de încredere de 99% pentru punctajul mediu statistica: ", ci_lower_99, ",", ci_upper_99, "\n")
  
}

DII("statistica.csv")


# D3

n = 100
xas = 14 # nr de studenti care nu pot rezolva temele dupa schimbare 
pxbs = 0.85 # procentul de studenti care nu pot rezolva temele inainte de schimbare 
semnificatie = 0.01

p_hat = xas / n
z = (p_hat - pxbs) / sqrt(pxbs * (1 - pxbs) / n)
z_crit = qnorm(1 - semnificatie)

if (z > z_crit) {
  cat("Schimbarea este utilă.\n")
} else {
  cat("Schimbarea nu este utilă.\n")
}

semnificatie = 0.05
z_crit = qnorm(1 - semnificatie)

if (z > z_crit) {
  cat("Schimbarea este utilă.\n")
} else {
  cat("Schimbarea nu este utilă.\n")
}




