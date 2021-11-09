library(spdep)
library(gstat)
library(kriging)
library(ggplot2)
data(meuse)
coordinates(meuse) = ~x+y
x<-meuse$x
y<-meuse$y
X<-cbind(x,y)
dist<-dist(X)
max<-max(dist)
delta<-runif(155,0,360)
perc<-0.5
thetastar<-perc*max
theta<-runif(155,0,thetastar)
xx<-x+theta*cos(delta)
yy<-y+theta*sin(delta)


model1<-kriging(x,y, meuse$zinc, model="spherical")
model2<-kriging(xx,yy, meuse$zinc, model="spherical")


ggplot() +
  geom_point(data=model1$semivariogram, aes(distance, semivariance), color="gray70") + 
  geom_point(data=model2$semivariogram, aes(distance, semivariance), color="gray30") + 
  stat_function(fun= function(x) (model1[["sill"]] - model1[["nugget"]]) * (1.5 * (x/model1[["range"]]) - 0.5 * (x/model1[["range"]])^3) + 
                  model1[["nugget"]], lwd=0.5, linetype="dashed", color="gray70", xlim = c(0, max(model1$semivariogram[,1]))) +
  stat_function(fun= function(x) (model2[["sill"]] - model2[["nugget"]]) * (1.5 * (x/model2[["range"]]) - 0.5 * (x/model2[["range"]])^3) + 
                  model2[["nugget"]], lwd=0.5, color="gray30", xlim = c(0, max(model2$semivariogram[,1]))) +
  theme_minimal() +
  theme(legend.position = "none", 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

ggsave(paste0("semivariogram_", perc, ".png"))
