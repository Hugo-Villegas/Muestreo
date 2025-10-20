rm(list=ls())

y<-c(164,524,63,866)

p_i<-c(0.1,0.28,0.04,0.58)

j<-2

k<-4

#Estimador Hansen-Hurwitz

install.packages("TeachingSampling")

library(TeachingSampling)

s<-c(y[j],y[k])

estimator<-HH(s,c(p_i[j],p_i[k]))

estimator

#Intervalo de confianza

alpha<-0.10
t_val<-qt(1-alpha/2,2-1)

lim_inf<-c(estimator)[1]-t_val*c(estimator)[2]

lim_sup<-c(estimator)[1]+t_val*c(estimator)[2]

c(lim_inf,lim_sup)