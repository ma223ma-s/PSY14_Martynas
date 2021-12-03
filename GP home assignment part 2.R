library(MVA)
library(MASS)
library(smacof)
library(vegan)
library(tidyverse)


#Basic NMDS and graph plotting

require(smacof) 
Nations_diss = sim2diss(Nations, method = 7, to.dist = TRUE) 

print(Nations_diss)

require(MASS) 


Nations_test <- metaMDS(Nations_diss, autotransform = F)
ordiplot(Nations_test)
ordiplot(Nations_test, type = "t", cex = 1.4)

print(Nations_test)

#Implementing the multidimensional scaling

Nations_mds1 = isoMDS(Nations_diss, k = 1)
Nations_mds2 = isoMDS(Nations_diss, k = 2)
Nations_mds3 = isoMDS(Nations_diss, k = 3)



print(Nations_mds1)
print(Nations_mds2)
print(Nations_mds3)



plot(Nations_mds1$points)
plot(Nations_mds2$points)



#Dimensionality of the data check

x <- Nations_mds1$points[,1]
y <- Nations_mds3$points[,3]
    plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
       main = "NMDS", type = "n")
           text(x, y, labels = row.names(Nations[, 1:12]), cex = 1)


#Graphic display of the stress level difference between dimensions

stress_value = c(Nations_mds1$stress, Nations_mds3$stress)
  dimensions = 1:2
     plot(dimensions, stress_value, type = "b", 
        xlab = "Dimension Number", 
           ylab = "Level of stress")


#Creating the stress plot

stressplot(Nations_mds3,Nations_diss, cex = 2)

fit_uni <- uniscale(Nations_diss)
fit_uni

Nations_test$stress

monoMDS(Nations_diss)
