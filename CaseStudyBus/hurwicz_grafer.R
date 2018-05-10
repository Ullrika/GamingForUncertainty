# Namn: hurwicz
# Senast ändrad: 2017-11-26

# Förklaring: Prioriterar åtgärderna utefter Hurwicz kriterie. Returnerar vektor
# med alternativen presenterade från bäst till sämst samt dess a-index. (högt 
# a-index = bra)

# Skapar funktion som kräver ett a-värde (0-1, default: 0.5) samt en matris P som 
# innehåller åtgärdens performance-intervall (kolonnvektorer = nedre resp. övre gräns) för 
# alternativen (radvektorer).

hurwicz_grafer <- function(a,P,alt){
  cols <- nrow(P)
  a_index <- matrix(c(rep(NA,cols)), nrow = 1, ncol = cols)   # Skapar en tom matris för a-index
  
# Beräknar a-index för respektive kolumn och tilldelar motsvarande plats i a_index-
# matrisen detta värde.
  
    for(i in 1:cols){                            
      a_index[1,i] <- a*min(P[i,])+(1-a)*max(P[i,])  
    }
  
  colnames(a_index) <- alt  # Ger kolumnerna i a-index namn
  
  # Sorterar a-index från högst till lägsta.  
  
  a_index <- t(apply(t(a_index),2,sort,decreasing = TRUE))
  resultat_hurwicz <-a_index
  # Vänder på matrisen så att alternativen presenteras från bäst till sämst.
  # Högst a-index = bäst.
  
  #resultat_hurwicz <-a_index[,c(ncol(a_index):1), drop = FALSE]
  resultat_hurwicz
}





