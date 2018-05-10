grain_of_rice <- function(N){
 grains_of_rice_per_gm = 60
 weight_of_grain_of_rice_gm = 1/ grains_of_rice_per_gm 
 number_of_grains_per_kilo = 1000/weight_of_grain_of_rice_gm
 number_of_grams_in_N_grains = N*weight_of_grain_of_rice_gm 
 cubic_cm_assuming_0.9_in_N_grains =  number_of_grams_in_N_grains / 0.9
 litres_of_rice =  cubic_cm_assuming_0.9_in_N_grains / 1000
 length_of_bath = 170
 width_of_bath = 70
 depth_of_rice_in_bath = cubic_cm_assuming_0.9_in_N_grains /( length_of_bath * width_of_bath )
 if(round(litres_of_rice,1)>0.1){
 return(list(litres_of_rice = round(litres_of_rice,1), depth_of_rice_in_bath = round(depth_of_rice_in_bath,2)))
 }else{
   return(list(litres_of_rice = round(litres_of_rice,4), depth_of_rice_in_bath = round(depth_of_rice_in_bath,6)))
 }
}

heads_flipped_in_a_row <- function(N){round(log(N)/log(2))}

sixs_thrown_in_a_row <- function(N){round(log(N)/log(6))}

walk <- function(N){
  steps_per_meter = 2
  meters_walked = N / steps_per_meter
  kilometers_walked = meters_walked / 1000
  speed_per_kilometer_km_per_h = 4.8
  time_to_walk = kilometers_walked  / speed_per_kilometer_km_per_h 
  return(list( time_to_walk = round(time_to_walk,1), kilometers_walked = round(kilometers_walked,1)))
}

distance_from_paris <- function(kilometers_walked){
city = c('the cafe down the street','Versailles','Le Havre','Brussel','Berlin','Stockholm','Moscov','New Dehli',
         'Cape Town','the Moon', 'the Sun','Neptunus')
dist=c(0,20, 201, 283,990,1760,2761,7771, 12232,384400,149597870.7,4.4*10^9)
time=c(0,5, 41, 58,203,356, 562, 1580, 2438,80083,31166223,0)

ind = max(which(kilometers_walked >= dist))
return(list(ci=c(city[ind],city[ind+1]),ti=c(time[ind],time[ind+1])))

}



