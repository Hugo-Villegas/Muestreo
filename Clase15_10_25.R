rm(list=ls())
library("plyr")

## Estimaciones

dat     <- read.csv("datos_100_dormitorios.csv")
N       <- 100
n       <- 5
Mi      <- 4
M       <- N * Mi
yi      <- aggregate(GPA~Dormitorio,data=dat,sum)[,"GPA"]
ybar    <- mean(yi)
tau_hat <- N/n*sum(yi)
(mu_hat  <- tau_hat / M)
s2_u    <- sum((yi - ybar)^2)/(n-1)
(varhat_muhat <- 1/M^2 * (N* (N-n) *s2_u /n))
sqrt(varhat_muhat)

## Generacion de muestras

library(sampling)

dat_pop     <- data.frame(read.csv("datos_100_dormitorios.csv"))

dat_sample <- cluster(dat_pop,clustername="Dormitorio",size=5,method="srswor")

dat_sample
