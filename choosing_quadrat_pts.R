# FOR CHOOSING POINTS
getwd()
setwd("C:/Users/jensen/Desktop/mongolia/2019 interdisciplinary")

verts1=read.csv("LC1_Hay2_points_real.csv")


verts <- verts1 %>% select("lat", "lon")


#convert to x,y grid coordinates
lat_min=min(verts$lat)
lon_min=min(verts$lon)



Y_vec=(verts$lat-lat_min)*60
X_vec=(verts$lon-lon_min)*38.326

plot(X_vec,Y_vec)

Y_rand=runif(n=5, min = 0, max=max(Y_vec))
X_rand=runif(n=5, min = 0, max=max(X_vec))

points(X_rand,Y_rand, col="red")
FOR DIVERSITY ANALYSIS
library(dplyr)

data <- read.csv("C:/Users/jensen/Desktop/mongolia/2019 interdisciplinary/SZ_quadrats.csv")
View(data)


diversity <- data %>% 
  select(quadrat,square,species) %>% 
  #group_by(quadrat) %>% 
  summarize(species_count = n_distinct(species))

rarefaction=matrix(nrow =10, ncol =10)

for (i in 1:10)
{
  for (j in 1:10)
  {
    data_subset <-filter(data, quadrat == i |quadrat == j)
    diversity <- data_subset %>% 
      select(quadrat,square,species) %>% 
      summarize(species_count = n_distinct(species))
    rarefaction[i,j]=diversity$species_count
  }
  
}

Soilrandom=runif(n=5, min = 1, max=25)

diversity <- data %>% 
  select_if(quadrat==2) %>% 
  #group_by(quadrat) %>% 
  summarize(species_count = n_distinct(species))
