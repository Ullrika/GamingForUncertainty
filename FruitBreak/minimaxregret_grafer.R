# Namn: minimaxregret
# Senast ändrad: 2017-01-31

# Förklaring: Prioriterar åtgärderna utefter minimax regret-kriteriet. 
# Returnerar vektor med alternativen presenterade från bäst till sämst samt de 
# jämförda värdena. 


# Kräver en matris, P, med åtgärdernas alla performance-värden som kolonnvektorer. 

minimaxregret_grafer <- function(P,alt){
  R <- nrow(P)
  
  # Skapar vektor, maxscen, som innehåller max-performance under de olika 
  # scenarierna. 
  minVal <- c(rep(NA,R))
  
  for (i in 1:R){
    minVal[i] <- min(P[i,])
  }
  
  # Skapar regret-matris som beskriver skillnad mellan performace av ett alternativ
  # och de olika scenariernas högsta performance. 
  
  maxregret <- matrix(c(rep(NA,R)), nrow = 1, ncol = R)
  m <- max(P)
  
  for (j in 1:R){
      maxregret[1,j] <- m - minVal[j]
  }
  
  colnames(maxregret) <- alt  # Ger kolumnerna i maxval namn
  
  # Sorterar alternativen och presenterar dem från bästa (minst regret) till sämsta. 
  
  maxregret <- t(apply(t(maxregret),2,sort))
  maxregret
}

