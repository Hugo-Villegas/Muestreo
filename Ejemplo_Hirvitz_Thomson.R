rm(list=ls())

y<-c(164,524,63,866)

p_i<-c(0.1,0.28,0.04,0.58)

pi_ij<-matrix(c(0.28115,0.07,0.00861,0.20254,0.07,0.70944,0.02722,0.6122,0.00861,0.02722,0.11524,0.0794,0.20254,0.6122,0.0794,0.894166),4,4)

j<-2

k<-4

#Horvtiz-Thompson

install.packages("Frames2")

library(Frames2)

s<-c(y[j],y[k])

Frames2::HT(s,c(pi_ij[j,j],pi_ij[k,k]))

Frames2::VarHT(s,matrix(c(pi_ij[j,j],pi_ij[j,k],pi_ij[k,j],pi_ij[k,k]),2,2))

install.packages("sampling")

library(sampling)

sampling::varHT(s,matrix(c(pi_ij[j,j],pi_ij[j,k],pi_ij[k,j],pi_ij[k,k]),2,2),1)

sampling::varHT(s,matrix(c(pi_ij[j,j],pi_ij[j,k],pi_ij[k,j],pi_ij[k,k]),2,2),2)
