#--------------------------
# MUESTREOS
#--------------------------
# 1.Muestreo aleatorio simple (MAS)
#   Es el tipo más básico de muestreo probabilístico. Cada elemento de la población tiene la misma probabilidad de 
#   ser seleccionado.
#   Ej. De una lista de 1000 trabajadores, se eligen al azar 100 sin ningún criterio adicional

# 2. Muestreo estratificado
#    La población se divide primero en estratos (subgrupos homogéneos) según una o más características relevantes 
#    (por ejemplo: sexo, edad, región, nivel educativo), y luego se realiza un muestreo aleatorio dentro de cada estrato
#    a. Proporcional: el tamaño de la muestra en cada estrato es proporcional al tamaño del estrato en la población.
#    b. No proporcional (óptima o igual): se puede dar más peso a estratos pequeños o más variables para mejorar la 
#       precisión
#    Ej. De una empresa con 60 % hombres y 40 % mujeres, para una muestre de 100 
#        a. se seleccionan aleatoriamente 60 hombres y 40 mujeres
#        b. se seleccionan aleatoriamente 50 hombres y 50 mujeres

# 3. Muestreo con probabilidades desiguales
#    Cada elemento o unidad de la población tiene una probabilidad distinta de ser seleccionado, normalmente 
#    proporcional a alguna medida de tamaño o importancia. Puede ser con o sin reemplazo
#    Ej. Si se seleccionan comunas para una encuesta nacional, las comunas con mayor población tienen más probabilidad 
#        de ser elegidas.


#Ejemplo: 
set.seed(123)

poblacion <- data.frame(
  id = 1:1000,
  faena = sample(c("A", "B", "C"), 1000, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  edad = round(rnorm(1000, mean = 40, sd = 10)))

# Supongamos que el salario depende del tipo de faena y de la edad
poblacion$salario <- round(1000 + 
                             ifelse(poblacion$faena == "A", 400, 
                                    ifelse(poblacion$faena == "B", 600, 800)) +
                             10 * poblacion$edad + rnorm(1000, 0, 300))
head(poblacion)

# MAS
n <- 120 # Tamaño de muestra
muestra_MAS <- poblacion[sample(1:nrow(poblacion), n), ]

# Estratificado

## probabilidades iguales
library(dplyr)
table(poblacion$faena) ##revisar como esta separada la población.
muestra_estrat <- poblacion %>% group_by(faena) %>% sample_frac(size = 0.1)  # proporción por estrato, 10% de cada faena
table(muestra_estrat$faena)

# Probabilidades desiguales
# Supongamos que queremos que las personas de mayor salario tengan más probabilidad de ser seleccionadas.
prob <- poblacion$salario / sum(poblacion$salario) # Probabilidad proporcional al salario
muestra_PPS <- poblacion[sample(1:nrow(poblacion), n, prob = prob), ]
?sample
head(muestra_PPS)

# Comparamos las medias...
mean(poblacion$salario)
mean(muestra_MAS$salario)
mean(muestra_estrat$salario)
mean(muestra_PPS$salario)

# El MAS suele reflejar bien el promedio poblacional (por azar).
# El estratificado puede reducir la variabilidad si hay diferencias entre faenas.
# El PPS puede sesgar el promedio hacia valores altos, ya que da más peso a quienes ganan más.


#--------------------------
# MUESTREO ALEATORIO ESTRATIFICADO
#--------------------------
# La población se divide en estratos homogéneos y se extrae una muestra de cada estrato

# EJEMPLO 
# Base de datos de empleados:queremos obtener una muestra estratificada por sexo, con el mismo porcentaje en cada grupo
set.seed(123)

empleados <- data.frame(
  id = 1:100,
  sexo = sample(c("Hombre", "Mujer"), 100, replace = TRUE, prob = c(0.6, 0.4)),
  edad = round(rnorm(100, mean = 35, sd = 10)),
  salario = round(rnorm(100, mean = 800000, sd = 100000)))

head(empleados)
table(empleados$sexo)

#install.packages("sampling")
library(sampling)

# Queremos una muestra del 30% de cada estrato
muestra_estratificada <- strata(
  data = empleados,
  stratanames = "sexo",
  size = c(0.3 * table(empleados$sexo)),  # 30% de cada grupo
  method = "srswor")  # muestreo aleatorio simple sin reemplazo

?sampling
# "muestra_estratificada" contiene las filas seleccionadas (con índices).
# Para obtener los datos originales de esas filas

muestra_final <- getdata(empleados, muestra_estratificada)
head(muestra_final)

# Comprobamos la proporción de la muestra
prop.table(table(muestra_final$sexo))
prop.table(table(empleados$sexo))


#-----------------
# Volvamos al ejemplo de la clase pasada "pre-censo". Queremos realizar un muestreo de manzanas estratificado por comuna.
# La idea es estimar la media de mujeres por manzana en el territorio, con un error menor o igual a 5%.

setwd("~/RStudio/2025/Clases 3")
base <- readxl::read_excel("Precenso-comunas.xlsx", skip = 1)
attach(base)

#############
# El ejercicios anterior: "se desea la proporción promedio de mujeres por manzana con un 95% de confianza y un error máximo del 5%.
# Estudios previos indican que aproximadamente un 60% de los habitantes son mujeres"

library(samplingbook)
set.seed(123)
N <- nrow(base)
n <- sample.size.prop(e = 0.05, P = 0.6, N = N, level = 0.95)$n
muestra <- sample(1:N, n, replace = FALSE)
base_muestra <- base[muestra, ]

mujeres_exitos <- sum(base_muestra$`Población-M`)
total_personas <- sum(base_muestra$`Población-T`)
Sprop(m = mujeres_exitos, n = total_personas, N =Inf , level = 0.95)  #comparamos con sum(base$Población_M) / sum(base$Población_T)

# Si quisieramos conocer la media muestral y hacer una estimación poblacional debe tener la sgte información:
N <- nrow(base)
S <- sd(base$Población_M) #viene como información de referencia
e <- 5
n2 <- sample.size.mean(e = e, S = S, N = N, level = 0.95)$n

muestra2 <- base[sample(1:N, n, replace = FALSE), ]

Smean(muestra2$Población_M) #media muestral
N * Smean(muestra2$Población_M)$mean #estimacion del total

#################

# Ahora...lo mismo pero por comuna

N <- nrow(base)          
nivel_confianza <- 0.95  
e <- 0.05    

# Necesitamos entonces calcular el tamaño de muestra total y por estrato en un muestreo estratificado, 
# según el error permitido (e), el tamaño poblacional de los estratos (Nh), y la desviación estándar por estrato (Sh).

# 1. Tamaño total de la muestra
# stratasize(e , Nh = c(), Sh = c(), type = "prop")
# type -> Tipo de asignación del tamaño de muestra, "prop": proporcional (también puedes usar "opt" (Neyman)).

# 2. Tamaño total por estrato
# stratasamp(tamaño_total_muestra, Nh = c(), Sh = c(), type = "prop")


# Simularemos que un experto nos entrega la informacion que necesitamos (estadísticas por estrato, Nh y Sh)
library(dplyr)
Sum <- base %>% group_by(Comuna_texto) %>% summarise(N  = n(), S = sd(base$`Población-M`)) 
Sum

# group_by(Comuna_texto): define los estratos
# N = n() -> cuenta cuántas observaciones hay en cada comuna (estrato)
# sd = sd(Población_M) -> calcula la desviación estándar del total de mujeres en cada comuna (estrato)

N_need <- stratasize(e=5, Nh = c(609, 1396, 1564), Sh = c(104, 139, 58), type = "prop")
N_need  #Tamaño total de la muestra

Num_estrat <- stratasamp(N_need$n, Nh = c(609, 1396, 1564), Sh = c(104, 139, 58), type = "prop")
Num_estrat

# OJO!! El orden de los estratos en stratasize() depende del orden en que tú ingresas los vectores Nh y Sh.
# No necesariamente es alfabético, ni el del data.frame original.
# El data.frame Sum queda ordenado según el orden alfabético de la variable de agrupación (Comuna_texto), por defecto.

# Ahora...realizamos el muestreo
Nh <- table(base$Comuna_texto)
Nh

n1 <- 194 #La Reina
muestra1 <- base %>% filter(Comuna_texto == "La Reina") %>% sample_n(n1, replace = FALSE)
nrow(muestra1)
table(muestra1$Comuna_texto)

n2 <- 446 #Las Condes
muestra2 <- base %>% filter(Comuna_texto == "Las Condes") %>% sample_n(n2, replace = FALSE)
nrow(muestra2)
table(muestra2$Comuna_texto)

n3 <- 499 #Pañalolén
muestra3 <- base %>% filter(Comuna_texto == "Peñalolén") %>% sample_n(n3, replace = FALSE)
nrow(muestra3)
table(muestra3$Comuna_texto)


# Juntamos las muestras
muestra_comunas <- muestra1 %>% bind_rows(muestra2) %>% bind_rows(muestra3)
head(muestra_comunas)
nrow(muestra_comunas)

nh <- table(muestra_comunas$Comuna_texto); nh
bind_rows(Nh, nh)

# Estimacion(estimar la media de mujeres por comuna)
res <- stratamean(muestra_comunas$`Población-M`, muestra_comunas$Comuna_texto, Nh, eae = TRUE)
res
# error estándar de estimación (eae): además de la media estratificada, devuelve su error estándar y error relativo

# La fila "overall" corresponde a la media poblacional estimada (ponderada entre los estratos)
# 80.41214= (N_ LaReina*86.60309+N_LasCondes*94.47982+N_Peñalolén*65.44489) /(N_LaReina+N_LasCondes+N_Peñalolén)

# SE: eror estándar de estimación
# CIu, CIo: Intervalos de confianza

# Para comparar:
media_real_total <- mean(base$Población_M, na.rm = TRUE)
media_real_total

media_real_por_comuna <- base %>% group_by(Comuna_texto) %>% summarise(media_real = mean(Población_M, na.rm = TRUE))
media_real_por_comuna



# Como se ve en un gráfico?
library(ggplot2)
df <- data.frame(
  Estrato = c("La Reina", "Las Condes", "Peñalolén", "Overall"),
  Media = c(87.06, 99.82, 66.51, 83.05),
  CIo = c(72.52, 87.48, 61.97, 77.27),
  CIu = c(101.60, 112.16, 71.05, 88.83)
)

ggplot(df, aes(x = Estrato, y = Media)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = CIo, ymax = CIu), width = 0.2) +
  geom_hline(yintercept = media_real_total, color = "red", linetype = "dashed") +
  labs(title = "Comparación entre medias estratificadas e índice real",
       y = "Media estimada ± IC95%", x = "Estrato") +
  theme_minimal()


#-----------------
# PRORPORCIONES: proporción de mujeres por comuna

# Creamos la variable de proporción de mujeres por comuna
muestra_comunas <- muestra_comunas %>% mutate(prop_mujeres = Población_M / Población_T)

# Aplicamos estratificación
# Estimar proporción promedio (como una media)
res_prop <- stratamean(muestra_comunas$prop_mujeres,muestra_comunas$Comuna_texto,Nh = c(609, 1396, 1564),eae = TRUE)

res_prop

# Comparamos
sum(base$Población_M) / sum(base$Población_T) # proporción real total

prop_real_por_comuna <- base %>% group_by(Comuna_texto) %>% summarise(prop_real = sum(Población_M) / sum(Población_T))
prop_real_por_comuna

# ¿Qué tamaño de muestra necesito para obtener la propocion de mujeres por comuna, sabiendo que tengo un valor de 
# referencia anterior al estudio?

n <- sample.size.prop(e = 0.05, P = 0.6, N = sum(Nh), level = 0.95)$n
n_h_prop <- round(n * Nh / sum(Nh))
n_h_prop  # tamaño de muestra por comuna sabiendo que la proporcion es de 0.6 (en total)



# Si tenemos un valor conocido de proporcion por comuna

P_h <- c(0.62, 0.58, 0.60) 
S_h <- sqrt(P_h * (1 - P_h)) #desviacion estandar por comuna (estrato)
# pesos de Neyman
w_h <- Nh * S_h
n_h_neyman <- round(n_total * w_h / sum(w_h))
n_h_neyman #tamaño de la muestra por comuna 


#---------------
# EJERCICIO DE LA CLASE (2 de octubre)

N <- 4000 # Población total
Nh <- c(0.15, 0.40, 0.45) * N   #% de praticipación ordenado: alto, medio, bajo
rangos <- data.frame(Estrato = c("Alto", "Medio", "Bajo"), 
                     Min = c(800, 300, 150),
                     Max = c(2000, 800, 300)) 
rangos$Sh <- (rangos$Max - rangos$Min) / 4 # Desviaciones estimadas (regla empírica: (max - min)/4)
rangos

# Tamaño total de muestra (tipo de asignación "neyman" = óptima)
N_total <- stratasize(e = 50, Nh = Nh, Sh = rangos$Sh, level = 0.95, type = "opt")
N_total

# Tamaño por estrato
Num_estrat <- stratasamp(N_total$n, Nh = Nh, Sh = rangos$Sh, type = "opt")
Num_estrat

# La asignación prioriza los estratos con mayor variabilidad (Sh) y mayor tamaño (Nh)
# Si quisieras una asignación proporcional (no óptima)
stratasamp(N_total$n, Nh = Nh, Sh = rangos$Sh, type = "prop")


#--------------------------
# MUESTREO CON PROBAILIDADES DESIGUALES
#--------------------------
# Queremos estimar el total de ventas de una población de empresas, donde cada empresa tiene distinto tamaño 
# (número de empleados).Usaremos un muestreo con probabilidades proporcionales al tamaño (PPS), sin reemplazo

set.seed(123)

# Población de 10 empresas
poblacion <- data.frame(
  empresa = paste0("E", 1:10),
  empleados = sample(50:500, 10),               # tamaño de la empresa
  ventas = round(runif(10, 1000, 10000)))       # ventas (en miles de pesos)


poblacion

# Probabilidades proporcionales al tamaño. Cada empresa tiene probabilidad proporcional a su número de empleados
poblacion$prob <- round(poblacion$empleados / sum(poblacion$empleados),2) 
poblacion



#-------
# Seleccionamos n=4 empresas SIN reemplazo, usando sus probabilidades como pesos

library(sampling)
pik <- inclusionprobabilities(poblacion$empleados, n = 4) #probailidad de que la unidad i este incluida en la muestra
poblacion$pi <- pik  
poblacion

muestra_ind <- UPsystematic(pik)
muestra <- poblacion[muestra_ind == 1, ]
muestra

# install.packages("Frames2")
library(Frames2)
HT_est <- HT(y = muestra$ventas, pi = muestra$pi)# Estimador Horvitz–Thompson
HT_est # Este valor es la estimación del total poblacional de ventas


# Comparamos con el valor real...
Y_real <- sum(poblacion$ventas)
cbind(Estimado = round(HT_est, 1),Total_Real = Y_real,Error = round(HT_est - Y_real, 1))

#-------
# Repitamos lo mismo pero esta vez CON reemplazo
n <- 4
idx <- sample(1:nrow(poblacion), size = n, replace = TRUE, prob = poblacion$prob)
muestra <- poblacion[idx, ]
muestra$seleccion_index <- idx  
muestra

#install.packages("TeachingSampling")
library(TeachingSampling)
Y_HH <- HH(y = muestra$ventas, p = muestra$prob) #Estimador Hansen–Hurwitz
Y_HH

# Comparamos con el valor real...
Y_real <- sum(poblacion$ventas)
cbind(Total_estimado = round(Y_HH[1,1], 1),Total_Real = Y_real,Error = round(Y_HH[1,1] - Y_real, 1))


# RESUMEN:
# En muestreo con reemplazo (Hansen–Hurwitz)
# - Se utiliza la probabilidad de selección
# - Probabilidad de que la unidad i sea elegida en una extracción individual
#   HH(y = muestra$ventas, p = muestra$prob)

# En muestreo sin reemplazo (Horvitz–Thompson)
# - Se utiliza la probabilidad de inclusión
# - Probabilidad de que la unidad i esté en la muestra final (considerando todas las extracciones)


# Probailidad de inclusion
# Supongamos una población de 3 unidades: A (grande), B (mediana), C (pequeña).
# Queremos seleccionar n=2 

# Si el muestreo es CON reemplazo, una unidad podría salir dos veces, y las probabilidades serían independientes.
# Si es SIN reemplazo, una vez que A entra, ya no puede volver, y eso cambia las probabilidades de inclusión de las demás.
# -> pi_A es la probabilidad de que A aparezca en la muestra (una vez, porque no pu