variabilaDiscreta <- function(valori, probabilitati) 
{
  if(length(valori)!=length(probabilitati)) 
  {
    stop("Trebuie sa avem acelasi numar de valori si de probabilitati.")
  }
  
  if(any(probabilitati<0)||sum(probabilitati)!=1)
  {
    stop("Nu putem avea probabilitati negative, iar suma tuturor probabilitatilor trebuie sa fie 1")
  }
  nrRandom <- runif(1)
  probabilCumulate <- cumsum(probabilitati)
  indexSelectat <- match(TRUE, nrRandom <= probabilCumulate)
  return(valori[indexSelectat])
}
valori <- c(5, 8, 9)
probabilitati <- c(0.1, 0.7, 0.8)
valoareSimulata <- variabilaDiscreta(valori, probabilitati)
#Nu putem avea probabilitati negative, iar suma tuturor probabilitatilor trebuie sa fie 1
print(valoareSimulata);

valori <- c(4, 2, 3)
probabilitati <- c(0.1, 0.3, 0.6)
valoareSimulata <- variabilaDiscreta(valori, probabilitati)
print(valoareSimulata); #3