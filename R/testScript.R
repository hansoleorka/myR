

source("R//kappa.R")

#Errormatrix 1
em1 <- matrix(c(36,4,6,11),2,2)
em2 <- matrix(c(39,1,2,15),2,2)

kappa(em1)
kappa(em2)
