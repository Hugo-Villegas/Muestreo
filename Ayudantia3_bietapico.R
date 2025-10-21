# Muestreo bietápico -> En la 1a etapa se seleccionan conglomerados (unidades primarias). 
#                       En la 2a etapa se seleccionan unidades elementales dentro de los conglomerados elegidos.
#                       Eje.: Seleccionas comunas -> luego colegios dentro de esas comunas.

# Muestro en dos fases -> Se seleccionan las mismas unidades elementales dos veces (en fases distintas), 
#                         pero con diferente información recolectada.
#                         Eje.: Fase 1: se eligen  1000 personas -> se mide edad y género.
#                               Fase 2: de esas 1000, se selecciona 200 -> se mide ingresos

# Bietápico -> dos niveles jerárquicos (conglomerados y unidades dentro de ellos)
# En dos fases -> la misma población, pero seleccionada dos veces para recolectar más información


#--------------
# EJEMPLO DE LA CLASE

# El director de finanzas de una gran consultora quiere estudiar la productividad de sus empleados en varias oficinas de la
# empresa y decide medir la productividad en terminos del numero de horas semanales cobradas a los clientes.

# En total, la empresa cuenta con 7.457 empleados repartidos en 30 oficinas.
# Se selecciono con MAS a 4 oficinas y, luego, se selecciono tambien con MAS a un 40% de los empleados de ellas


# Objetivo: estimación del total y la media de productividad de los empleados en toda la empresa, 
# basada en un muestreo bietápico con afijación proporcional (MAS en ambas etapas).

# Primera etapa (unidades primarias): selecciona 4 oficinas de las 30 (MAS).
# Segunda etapa (unidades secundarias): dentro de cada oficina seleccionada, selecciona aleatoriamente al 40% de 
#                                       los empleados.

rm(list=ls())
library("plyr")

N      <- 30   # total de oficinas (unidades primarias)
n      <- 4    # n° de oficinas seleccionadas 
M      <- 7457 # total de empleados
Mi     <- c(297,212,287,236)  # n° de empleados (unidades secundarias) totales en las oficinas seleccionadas 
mi     <- c(119,85,115,94)    # n° de empleados seelccionados en las oficinas seleccionadas (40%)
ybari  <- c(22.2269,22.5765,22.2957, 22.1489) # promedio muestral de productividad por oficina
sums2i <-c(2918.87,2258.75,3013.95,2263.91)   # varianza muestral dentro de cada oficina

#a) Estimador del total de productividad de cada oficina
yihat <- ybari*Mi

#b) 
tauhat <- N/n * sum(yihat) # Estimador del total de productividad en toda la empresa

mu1hat  <- tauhat/N          # Promedio estimado del total de productividad por oficina

s2u     <- sum((yihat - mu1hat)^2)/(n-1)  # varianza del estimador total: variabilidad entre las oficinas seleccionadas

# Varianza del estimador total del total poblacional: varianza del estimador del total
varhat_tauhat <- N* (N-n)/n * s2u +  # <- 1er término: varianza entre oficinas
  N/n*sum(Mi*(Mi - mi)/mi * (sums2i/(mi-1)))  # <- 2do término: varianza dentro de las oficinas

#c)
tauhat/M   # estimador de la media global de productividad por empleado
varhat_tauhat/M^2 # varianza estimada de esa media


#d) IC
mean_emp <- tauhat / M
var_mean_emp <- varhat_tauhat / M^2
sd_mean_emp <- sqrt(var_mean_emp)
sd_tau <- sqrt(varhat_tauhat)

z <- 1.96
CI_z_mean <- mean_emp + c(-1,1) * z * sd_mean_emp
CI_z_tau <- tauhat + c(-1,1) * z * sd_tau

round(c(mean_emp, CI_z_mean), 2)
round(c(tauhat, CI_z_tau), 2)



#####################
#RESUMEN

#1. MAS: sample, sample.size.mean, Smean, sample.size.prop, Sprop
#2. PPS_con: ppswr, HH
#3. PPS_sin: UPsystematic, HT, inclusionprobabilities, VarHT

#A. Estratificado: strata, stratasize, stratasamp
#B. Conglomerados: cluster