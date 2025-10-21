#########################
#MUESTRO POR CONGLOMERADOS
#########################

#Ejemplo: Se quiere estimar el promedio de computadores por colegio en una Región compuesta por 5 comunas

# Población: número de computadores por colegio en las 5 comunas
datos <- data.frame(
  comuna = rep(c("A", "B", "C", "D", "E"), 
               each = 4),
  computadores = c(25, 30, 28, 22,   
                   35, 40, 38, 36,   
                   15, 18, 12, 20,   
                   10, 12, 9, 11,    
                   22, 24, 20, 26))

#Unidad primaria: comuna
#Unidad secundaria: colegios de cada comuna
#variable de interes: número de computadores
#mu1:número promedio de computadores por comuna
#mu: numero promedio de computadores por colegio

N <- nrow(datos) # Nº de unidades primarias en la poblacion
n <- 2 # Nº de unidades primarias en la muestra.


# Como es costoso recorrer todas las comuneas se escogen solo 3 al azar
set.seed(123)
comunas_sel <- cluster(datos,clustername="comuna",size=3,method="srswor")#sample(datos$comuna, n, replace = FALSE)
muestra <- datos[comunas_sel$ID_unit, ]


# Estimación del promedio poblacional (computadores por colegio para toda la región)
# Como cada comuna tiene la misma probabilidad de ser seleccionada

promedios_muestra <- aggregate(computadores ~ comuna, data = muestra, mean) 
tau_hat <- (N / n) * mean(promedios_muestra$computadores)
tau_hat

# Comparamos
mean(datos$computadores)

# OJO!! No confundir por muestreo estratificado, en ese método se toma una muestra dentro de cada estrato.
# En muestreo por conglomerados, se eligen los conglomerados "enteros". 


# En este ejemplo:
# Estatificado: 
#     - Cada comuna es un estrato 
#     - Se toma una muestra de colegios dentro de cada comuna
#     - Todas las comunas están representadas -> estimador más preciso, porque consideras la variación entre comunas
#     - Desventaja: más costoso (hay que visitar todas las comunas)
# Conglomerado: 
#     - Cada comuna es un conglomerado
#     - Se toman algunas comunas completas y se muestrean todos los colegios dentro
#     - Es más económico (menos comunas que visitar)
#     - Desventaja: el estimador es menos preciso si las comunas son muy distintas entre sí


# Tenemos distintas maneras de seleccionar la unidades primarias (comunas)
# - MAS
# - Probabilidades Proporcionales al Tamaño (PPS con o sin reemplazo)



#----------------
# Ejemplo: Tenemos una población de 6 conglomerados (comunas).
# Cada conglomerado tiene diferente número de individuos (tamaño) y una media de ingresos.

library(TeachingSampling)
library(pps)
library(sampling)
library(Frames2)

#-------------------------------
# 1. Definición de la población

datos <- data.frame(
  comuna = 1:6,                                # Unidades primarias (comunas)
  Mi = c(80, 50, 40, 30, 20, 10),              # Tamaño del conglomerado (número de individuos)
  ybarra_i = c(100, 200, 150, 250, 300, 400))  # Promedio de la variable de interés (ingresos promedio por comuna)


datos$yi <- datos$Mi * datos$ybarra_i      
datos   # Total por conglomerado (y_i) : total real de ingreso por comuna

tau_real <- sum(datos$yi)                 
tau_real # Total real poblacional: ingreso total de las 6 comunas

N <- nrow(datos)  # Total de conglomerados
n <- 3  # Tamaño de muestra de conglomerados


#----------------------------------------------------
# 2. Unidades primarias seleccionadas con MAS 
set.seed(123)
mas_ids <- cluster(datos,clustername="comuna",size=n,method="srswor") # srswor = simple random sampling without replacement
mas_ids

# Estimador del total de la poblacion: tau^ = (N/n) * sum(y_i)
tau_hat_MAS <- (N / n) * sum(datos$yi[mas_ids$ID_unit]) 
tau_hat_MAS  

# Varianza estimada según clase:
# Var(τ̂) = N*(N - n)/n * s²_u
# s²_u = 1/(n-1) * sum( (y_i - ȳ)² )     
y_muestra <- datos$yi[mas_ids$ID_unit]  
s2u <- var(y_muestra)                # Varianza muestral entre los conglomerados seleccionados
Var_hat_MAS <- N * (N - n) * s2u / n # Varianza estimada del estimador total
Var_hat_MAS

#---------------------------------------------------------
# 3. Unidades primarias seleccionadas con PPT CON reemplazo: Estimador Hansen–Hurwitz
p_PPS <- datos$Mi / sum(datos$Mi)       # Probabilidades proporcionales al tamaño
datos$p_PPS <- p_PPS 
datos

set.seed(123)
pps_ids <- ppswr(p_PPS, n)  
pps_ids

# Estimador Hansen–Hurwitz
tau_hat_HH <- HH(y = datos$yi[pps_ids], p = p_PPS[pps_ids]) #mean(datos$yi[pps_ids] / pik_PPS[pps_ids])
tau_hat_HH

# Varianza estimada de Hansen–Hurwitz:
# Var_hat = (1/n(n-1)) * sum( (y_i/p_i - tau_hat)^2 )
Y_over_p <- datos$yi[pps_ids] / pr_PPS[pps_ids]   
Var_hat_HH <- sum( (Y_over_p - tau_hat_HH)^2 ) / (n * (n - 1))
Var_hat_HH


#---------------------------------------------------------
# 4. Unidades primarias seleccionadas con PPT SIN reemplazo: Estimador Horvitz–Thompson
pik_PPS <- inclusionprobabilities(datos$Mi, n)
datos$pik_PPS <-pik_PPS
datos

set.seed(123)
pps_sinrep <- UPsystematic(pik_PPS)
pps_sinrep

# Estimador Horvitz–Thompson (HT)
tau_hat_HT <- HT(y = datos$yi[pps_sinrep == 1], pik = pik_PPS[pps_sinrep == 1])
tau_hat_HT

pi_ij <- outer(pik_PPS, pik_PPS, function(a, b) pmin(a, b)) #matriz cuadrada de pi_i_j
# Identificar las unidades seleccionadas
sel <- as.logical(pps_sinrep)

# Subconjunto de datos seleccionados
y_s <- datos$yi[sel]
pi_ij_s <- pi_ij[sel, sel]

# Varianza HT (método 1 = fórmula clásica)
Var_HT_sampling <- sampling::varHT(y_s, pi_ij_s, 1)
Var_HT_sampling

# También podés usar Frames2::VarHT
Var_HT_Frames2 <- Frames2::VarHT(y_s, pi_ij_s)
Var_HT_Frames2

#----------------------------------------
# 5. Comparación de resultados
data.frame(
  Metodo = c("Total real",
             "MAS",
             "Hansen–Hurwitz (PPT con reemplazo)",
             "Horvitz–Thompson (PPT sin reemplazo)"),
  Estimacion = c(tau_real, tau_hat_MAS, tau_hat_HH[1,1], tau_hat_HT),
  Varianza_Estimada = c(NA, Var_hat_MAS, Var_hat_HH, Var_HT_Frames2))


#---------------------
# Los tres métodos: MAS, PPT_con y PPT_sin son usados para escoger
# Para muestro por conglomerado -> se escogen conglomerados 
# Para muestro por estratificacion -> se escogen las unidades dentro de los estratos previamente escogidos

# Entonces como se escogen los estratos y las unidades dentro de los conglomerados??
# Para el caso de los conglomerados: los podemos usar completos o podemos hacer una selección de las unidades con los mismo 
#                                    tres métodos.
# Para el caso de muestreo estratificado, los estratos están definidos, y lo que se selecciona son unidades dentro de cada 
# estrato; el tamaño de la muestra por estratos la calculamos con stratasize y stratasamp.

