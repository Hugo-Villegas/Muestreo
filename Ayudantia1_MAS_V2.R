# Población
# Conjunto completo de individuos, elementos u objetos que comparten una característica de interés para el estudio.
#         Ejemplo: Todos los estudiantes de una universidad (10.000 estudiantes).

# Marco muestral
# Es la lista, registro o base de datos que contiene a todos los elementos de la población y desde donde se 
# seleccionará la muestra.
#         Ejemplo: La nómina oficial de estudiantes inscritos en la universidad ese semestre

# Muestra
# Subconjunto de elementos de la población seleccionados para ser estudiados. Debe ser representativa para poder 
# generalizar resultados.
#         Ejemplo: 200 estudiantes elegidos de la nómina.

# Muestreo probabilístico
# Es un tipo de selección de la muestra en el que todos los individuos de la población tienen una probabilidad 
# conocida y distinta de cero de ser elegidos. Esto asegura representatividad y permite hacer inferencias 
# estadísticas.
#         Ejemplo: Elegir 200 estudiantes al azar usando un generador de números aleatorios sobre la nómina oficial.


# OTRO EJEMPLO:
# Población: Todos los árboles de un parque (500 árboles).
# Marco muestral: Un plano numerado que identifica cada árbol del parque.
# Muestra: 50 árboles seleccionados para medir su altura.
# Muestreo probabilístico: Cada árbol tiene la misma oportunidad de ser elegido 


# Métodos de muestreo 
# 1. Aleatorio simple (M.A.S.)
# 3. Estratificado
# 4. Por conglomerados 
# 5. Multietápico



# Objetivos principales del muestreo probabilístico son:
# - Obtener estimaciones de parámetros poblacionales
# - Probar hipótesis (inferencia)
# - Minimizar el sesgo
# - Determinar el tamaño de la muestra



#install.packages("samplingbook")
library(samplingbook)
set.seed(123) # para reproducibilidad

#--------------------------
# MUESTREO ALEATORIO SIMPLE
#--------------------------
# Cada elemento tiene la misma probabilidad de ser elegido

#EJEMPLO (MEDIAS)
# Población: horas de estudio de 30 estudiantes
poblacion <- c(5,6,8,4,7,10,9,3,6,7,8,12,15,6,5,7,8,9,10,11,12,4,6,8,7,9,13,14,5,6)

mean(poblacion) # Media poblacional real

# Muestra pequeña (n = 5)
n <- 5
muestra5 <- sample(poblacion, size = n, replace = FALSE)

media_5 <- mean(muestra5)
N <-length(poblacion)

Z <- qnorm(0.975)
error_estandar <- sqrt((1-n/N)*var(muestra5)/n)
IC_inf <- media_5 - Z * error_estandar     
IC_sup <- media_5 + Z * error_estandar

media_5
IC_inf
IC_sup

Smean(muestra5, N, level = 0.95)

# Muestra mediana (n = 10)
muestra10 <- sample(poblacion, size = 10, replace = FALSE)
Smean(muestra10, N, level = 0.95)

# Muestra más grande (n = 15)
muestra15 <- sample(poblacion, size = 15, replace = FALSE)
Smean(muestra15, N, level = 0.95)

# ¿Cuál es el tamaño muestral mínimo?
# ->  ¿Qué desea estimar?
# ->  ¿Qué precisión?
# ->  ¿Nivel de confianza?
# ->  ¿Tamaño poblacional?


#sample.size.mean(e=error, S=desv.estándar, N=Inf, level=confianza)
S <- sd(poblacion)
error <- 1      
confianza <- 0.95

# Cálculo del tamaño de muestra
sample.size.mean(e = error, S = S, N = N, level = confianza)

muestra <- sample(poblacion, size = 17, replace = FALSE)
Smean(muestra, N, level = 0.95)

# En este "ejemplo" la desviación estándar la conocemos, pero cuando no, podemos usar el criterio del min-max.
N <- 30
mi <- min(poblacion)
ma <-max(poblacion)

S <- (ma-mi)/4
error <- 1 
confianza <- 0.95

sample.size.mean(e = error, S = S, N = N, level = confianza)


#--------------------------
# EJEMPLO (PROPORCIONES)
# Población: horas de estudio de 30 estudiantes 
poblacion <- c(5,6,8,4,7,10,9,3,6,7,8,12,15,6,5,7,8,9,10,11,12,4,6,8,7,9,13,14,5,6)

# evento: estudia más de 8 horas
evento <- as.integer(poblacion > 8) 
mean(evento)   # proporción poblacional real

# Muestra pequeña (n = 5)
muestra5 <- sample(poblacion, size = 5, replace = FALSE)
evento5 <- as.integer(muestra5> 8)  

Sprop(evento5,N=30,level=0.95)

# otra forma
m <- sum(muestra5 > 8)   # número de éxitos en la muestra
n <- length(muestra5)   
Sprop(m = m, n = n, N = N, level = 0.95)

# De que tanaño la muestra "ideal"?
error <- 0.1  #10% de error 
confianza <- 0.95
p <- mean(evento) 
sample.size.prop(e = error, P = p, N = N, level = confianza)

# OJO!, usamos la proporción real para el ejemplo, pero se debe usar la proporción esperada.

#--------------------------
#Ejemplo (de la clase)
# Determine el tamaño de muestra para estimar, con un 95% de confianza, el gasto semanal en almuerzo por parte de los
# funcionarios de nuestra empresa (N=1.000), con un error no mayor a $1,000 y basado en que un estudio mostró que el 
# gasto varía entre $7,500 y $35,000.
# ¿Qué pasa si reducimos el error a la mitad ($500)

sample.size.mean(e = 1000, S = (35000-7500)/4, N = 1000, level = 0.95)


#--------------------------
#EJERCICIO
set.seed(123)

N <- 1000  # tamaño de la población
poblacion <- data.frame(
  ID = 1:N,
  edad = round(rnorm(N, mean = 40, sd = 12), 1),   # edad promedio 40 años
  sexo = sample(c("Mujer", "Hombre"), N, replace = TRUE, prob = c(0.55, 0.45)))


n <- 100  # tamaño de muestra
muestra_id <- sample(1:N, size = n, replace = FALSE)
muestra <- poblacion[muestra_id, ]

#Queremos estimar la edad promedio poblacional y su intervalo de confianza
Smean(muestra$edad, N = N, level = 0.95)
Smean(muestra$edad, N = Inf, level = 0.95)
t.test(muestra$edad, conf.level = 0.95)

# Caso 1: - Estimación de la media poblacional considerando una población finita
#         - Estimación más eficiente y precisa cuando la población es finita y el tamaño muestral n es no despreciable 
#           respecto a N (por ejemplo, n/N > 0.05 o > 5%)
# Caso 2: - Estimación asumiendo población infinita
#         - Adecuado cuando n/N es muy pequeño (por ejemplo, <1%), como en encuestas nacionales o poblaciones grandes.
# Caso 3: - Prueba clásica de Student para la media muestral
#         - Equivalente al caso 2.


#Estimación de la proporción poblacional (mujeres)
m <- sum(muestra$sexo == "Mujer") # total de mujeres en la muestra
Sprop(m = m, n = n, N = N, level = 0.95)
Sprop(m = m, n = n, N = Inf, level = 0.95)
prop.test(x = m, n = n, conf.level = 0.95, correct = FALSE)

#Comparación con los valores reales de la población
mean(poblacion$edad)  # media real de edad
mean(poblacion$sexo == "Mujer")  # proporción real de mujeres


#--------------------------
#EJERCICIO
setwd("~/RStudio/2025/Clases 3")
bb <- readxl::read_excel("DatosPoblacion.xlsx",sheet="BioBio")
head(bb)
sum(bb$TOTAL)  # Num total de habitantes, lo que se desea estimar
mean(bb$TOTAL) # num promedio de hab x comunas

#Objetivo: Estimar el total de habitantes del Biobío (la población total) a partir de una muestra de comunas.

#Selección una muestra
set.seed(12345)   
n <- 10
N <- nrow(bb) #número total de comunas

muestra1 <- sample(1:N, size=n, replace=FALSE)
bb1 <- bb[muestra1,]

Smean(bb1$TOTAL, N=N) 

# pero necesitamos para el total, es decir N*promedio
N*Smean(bb1$TOTAL, N=N)$mean
N*Smean(bb1$TOTAL, N=N)$ci


#--------------------------
#EJERCICIO
base <- readxl::read_excel("Precenso-comunas.xlsx")

# Se dispone de información del precenso correspondiente a todas las manzanas de las comunas de Las Condes, La Reina 
# y Peñalolén.
# Cada registro indica la población total y el número de mujeres por manzana.

# Se desea estimar el total y  proporción promedio de mujeres por manzana con un 95% de confianza y un error máximo del 5%.
# Estudios previos indican que aproximadamente un 60% de los habitantes son mujeres


# Diseño muestral:
# - Tipo: muestreo aleatorio simple.
# - Unidad de muestreo: manzana.
# - Variable de interés: proporción de mujeres por manzana.
# - Población total (N): número total de manzanas.
# - Muestra (n): número de manzanas seleccionadas.


# Definimos parámetros
N <- nrow(base)          
nivel_confianza <- 0.95  # Nivel de confianza
error_deseado <- 0.05    # Error máximo permitido
P_esperado <- 0.6        # Proporción esperada de mujeres (según estudios previos)

# calculamos el tamaño de mínimo necesario (número de manzanas)
sample.size.prop(e = error_deseado, P = P_esperado, N = N, level = nivel_confianza)

# Generamos una muestra aleatoria simple
set.seed(123)
n <- 335
muestra <- sample(1:N, n, replace = FALSE)
base_muestra <- base[muestra, ]


# Tamaño total de mujeres
sum(base$Población_M)
N*mean(base$Población_M)

N*Smean(base_muestra$Población_M, N = N)$mean
N*mean(base_muestra$Población_M)


# Calculamos la proporción promedio de mujeres 
p_muestral <- sum(base_muestra$Población_M) / sum(base_muestra$Población_T)

mujeres_exitos <- sum(base_muestra$Población_M)
total_personas <- sum(base_muestra$Población_T)
Sprop(m = mujeres_exitos, n = total_personas, N =Inf , level = nivel_confianza)

# Validamos con población completa
poblacion_real <- sum(base$Población_M) / sum(base$Población_T)
poblacion_real




