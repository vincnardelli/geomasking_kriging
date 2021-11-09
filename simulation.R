library(spdep)
library(gstat)
library(kriging)
library(ggplot2)
library(scales)
library(patchwork)
library(dplyr)
library(purrr)
set-seed(1234)
data(meuse)
data(meuse.grid)

x<-meuse$x
y<-meuse$y

model1<-kriging(x,y, meuse$zinc, model="spherical")
image(model1)

model1_map = st_as_sf(model1$map, coords = c("x", "y"))
map <- st_as_sf(meuse.grid, coords = c("x", "y")) %>% 
  st_join(model1_map, join = nngeo::st_nn, maxdist = 5000, k = 1, progress = FALSE) %>% 
  rename(prev_pred=pred)

map1 <- map %>% 
  mutate(x = st_coordinates(.)[,1], 
         y = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  select(x, y, prev_pred)

simulation <- function(id, perc=0.5){
  X<-cbind(x,y)
  dist<-dist(X)
  max<-max(dist)
  delta<-runif(155,0,360)
  thetastar<-perc*max
  theta<-runif(155,0,thetastar)
  xx<-x+theta*cos(delta)
  yy<-y+theta*sin(delta)
  
  model2<-kriging(xx,yy, meuse$zinc, model="spherical")
  map2 = st_as_sf(model2$map, coords = c("x", "y"))
  
  
  matched <- map %>% 
    st_join(map2, join = nngeo::st_nn, maxdist = 500000, k = 1, progress = FALSE) %>% 
    mutate(x = st_coordinates(.)[,1], 
           y = st_coordinates(.)[,2], 
           id=id, perc=perc) %>% 
    select(id, perc, x, y, pred) %>% 
    st_drop_geometry()
  
  return(matched)
}


simulations <- expand.grid(id = 1:500, 
            perc = c(0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5))
# simulation_list <- lapply(1:nrow(simulations), function(x)  simulation(id=simulations$id[x],
#                                                                         perc=simulations$perc[x]))

results <- map_dfr(1:nrow(simulations), 
                   function(x)  simulation(id=simulations$id[x], 
                                           perc=simulations$perc[x]))
rownames(results) <- 1:nrow(results)

# save(results, file="results.RData")
# load("results.RData")

results %>% 
  left_join(map1) %>% 
  mutate(residual = abs((prev_pred-pred))) %>% 
  group_by(perc) %>% 
  summarise(error = mean(residual)) %>% 
  ggplot(aes(x=perc, y=error)) +
  geom_line() +
  theme_minimal() +
  xlab(expression(paste(theta^"*"))) +
  ylab("Mean Absolute Error")

ggsave("simulation_error.png")


ggplot(data=meuse) + 
  geom_point(aes(x, y, size=zinc, color=zinc)) + 
  theme_void() +
  scale_color_continuous(low="gray90", high="gray30")
ggsave("map.png")  


results %>% 
  left_join(map1) %>% 
  filter(id==1) %>% 
  left_join( map %>% 
               mutate(x = st_coordinates(.)[,1], 
                      y = st_coordinates(.)[,2]) ) %>% 
  ggplot() +
  geom_tile(aes(x, y, fill=pred)) +
  facet_wrap(perc ~ . , nrow = 2) +
  coord_equal() +
  theme_void() +
  scale_fill_gradient2(mid="gray80", high="gray20", low="blue") +
  geom_point(data=meuse, aes(x, y, color=zinc), size=0.8) +
  scale_color_continuous(low="gray80", high="gray20") +
  geom_point(data=meuse, aes(x, y), shape=1, size=0.8)
ggsave("example.png")  


results %>% 
  left_join(map1) %>% 
  mutate(abs_error = prev_pred-pred) %>% 
  group_by(perc, x, y) %>% 
  summarise(abs_error = mean(abs_error)) %>% 
  left_join( map %>% 
               mutate(x = st_coordinates(.)[,1], 
                      y = st_coordinates(.)[,2]) ) %>% 
  ggplot() +
  geom_tile(aes(x, y, fill=abs_error)) +
  facet_wrap(perc ~ . , nrow = 2) +
  coord_equal() +
  theme_void() +
  scale_fill_gradient2(mid="white", high="red", low="blue", name="Error") +
  geom_point(data=meuse, aes(x, y, color=zinc), size=0.8) +
  scale_color_continuous(low="gray80", high="gray20", name="Zinc") +
  geom_point(data=meuse, aes(x, y), shape=1, size=0.8)
ggsave("comparison.png")  

