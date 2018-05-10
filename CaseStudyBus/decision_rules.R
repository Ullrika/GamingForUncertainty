# Namn: rules
# Senasändrad: 2017-11-27

# Förklaring: Prioriterar åtgärderna utefter maximin-kriteriet. Returnerar vektor
# med alternativen presenterade från bäst till sämst. 


# Kräver en matris, P, med åtgärdernas alla performance-värden som kolonnvektorer. 

maximin_rule <- function(P){

  # Skapar vektor, minval, som innehåller min-performace för alternativen
  minval <- apply(P,1,min)
  order(minval, decreasing = TRUE)[1]
}
# Namn: maximax
# Senast ändrad: 2017-11-26

# Förklaring: Prioriterar åtgärderna utefter maximax-kriteriet. Returnerar vektor
# med det som ska maximeras 


# Kräver en matris, P, med åtgärdernas alla performance-värden som kolonnvektorer. 

maximax_rule <- function(P){
  
  maxval <- apply(P,1,max)
  
  order(maxval,decreasing = TRUE)[1]
}
# Namn: hurwicz
# Senast ändrad: 2017-11-26

# Förklaring: Prioriterar åtgärderna utefter Hurwicz kriterie. Returnerar vektor
# med alternativen presenterade från bäst till sämst samt dess a-index. (högt 
# a-index = bra)

# Skapar funktion som kräver ett a-värde (0-1, default: 0.5) samt en matris P som 
# innehåller åtgärdens performance-intervall (kolonnvektorer = nedre resp. övre gräns) för 
# alternativen (radvektorer).

h_rule <- function(P,a){
  cols <- nrow(P)
  a_index <- matrix(c(rep(NA,cols)), nrow = 1, ncol = cols)   # Skapar en tom matris för a-index
  
  # Beräknar a-index för respektive kolumn och tilldelar motsvarande plats i a_index-
  # matrisen detta värde.
  
  for(i in 1:cols){                            
    a_index[1,i] <- a*min(P[i,])+(1-a)*max(P[i,])  
  }
  
  order(a_index,decreasing = TRUE)[1]
}

# Namn: minimaxregret
# Senast ändrad: 2017-01-31

# Förklaring: Prioriterar åtgärderna utefter minimax regret-kriteriet. 
# Returnerar vektor med alternativen presenterade från bäst till sämst samt de 
# jämförda värdena. 


# Kräver en matris, P, med åtgärdernas alla performance-värden som kolonnvektorer. 

regret_rule <- function(P){
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
  
  
  # Sorterar alternativen och presenterar dem från bästa (minst regret) till sämsta. 
  
  order(maxregret)[1]
  
}





