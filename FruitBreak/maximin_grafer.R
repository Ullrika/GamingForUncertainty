# Namn: maximin
# Senast ändrad: 2017-01-17

# Förklaring: Prioriterar åtgärderna utefter maximin-kriteriet. Returnerar vektor
# med alternativen presenterade från bäst till sämst. 


# Kräver en matris, P, med åtgärdernas alla performance-värden som kolonnvektorer. 

maximin_grafer <- function(P,alt){

  # Skapar vektor, minval, som innehåller min-performace för alternativen
  col <- nrow(P)
  minval <- matrix(c(rep(NA,col)), nrow = 1, ncol = col)   
    
  for (i in 1:col){
    minval[1,i] <- min(P[i,])
  }

  colnames(minval) <- alt  # Ger kolumnerna i min namn
  
  # Sortera alternativen utefter maximin-kriteriet och returnera alternativen från 
  # bäst till sämst. 
  
  minval <- t(apply(t(minval),2,sort,decreasing = TRUE))
  resultat_maximin <- minval
  # Vänder på matrisen så att alternativen presenteras från bäst till sämst.
  
  #resultat_maximin <-minval[,c(ncol(minval):1), drop = FALSE]
  resultat_maximin
}