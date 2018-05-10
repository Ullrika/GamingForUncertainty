# Namn: maximax
# Senast ändrad: 2017-11-26

# Förklaring: Prioriterar åtgärderna utefter maximax-kriteriet. Returnerar vektor
# med alternativen presenterade från bäst till sämst samt dess jämförda värden. 


# Kräver en matris, P, med åtgärdernas alla performance-värden som kolonnvektorer. 

maximax_grafer <- function(P,alt){
  
  # Skapar vektor, maxval, som innehåller max-performace för alternativen
  col <- nrow(P)
  maxval <- matrix(c(rep(NA,col)), nrow = 1, ncol = col)   
  
  for (i in 1:col){
    maxval[1,i] <- max(P[i,])
  }
  
  colnames(maxval) <- alt  # Ger kolumnerna i maxval namn
  
  # Sorterar alternativen utefter maximax-kriteriet och returnera alternativen från 
  # bäst till sämst - ändrat från "sämst till bäst" av ullrika. 
  
  maxval <- t(apply(t(maxval),2,sort,decreasing = TRUE))
  
  resultat_maximax <- maxval
  # Vänder på matrisen så att alternativen presenteras från bäst till sämst.
  
  #resultat_maximax <- maxval[,c(ncol(maxval):1), drop = FALSE]
  #resultat_maximax
}