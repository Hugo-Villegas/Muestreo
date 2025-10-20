# 1. Instalar y cargar el paquete 'sampling' (si es necesario)
# install.packages("sampling")
library(sampling)

# 2. Simulación de la Población de Estudio
# Creamos una población de 100 escuelas (UPM)
# con un número variable de estudiantes (USM) en cada una.
set.seed(42) # Para reproducibilidad

N_escuelas <- 100
# Número de estudiantes por escuela (entre 100 y 300)
M_i <- sample(100:300, N_escuelas, replace = TRUE) 

# Crear un dataframe de población
poblacion <- data.frame(
  ID_Escuela = rep(1:N_escuelas, times = M_i),
  ID_Estudiante = sequence(M_i),
  Total_Estudiantes_Escuela = rep(M_i, times = M_i),
  Rendimiento = rnorm(sum(M_i), mean = 70, sd = 10) # Variable a medir
)

# 3. Muestreo Bietápico (Dos Etapas)

# Parámetros del Muestreo:
n_escuelas_muestra <- 10  # Número de UPM a seleccionar (Etapa 1)
m_estudiantes_por_escuela <- 15 # Número de USM a seleccionar en cada UPM (Etapa 2)

# ETAPA 1: Seleccionar 10 escuelas (UPM) por Muestreo Aleatorio Simple (MAS)
# Agrupamos la población por escuela para seleccionar UPMs
escuelas_upm <- unique(poblacion[, c("ID_Escuela", "Total_Estudiantes_Escuela")])

# La función 'sample' de R base puede usarse para la selección
# Seleccionamos las IDs de las UPMs
UPM_seleccionadas <- sample(escuelas_upm$ID_Escuela, 
                            size = n_escuelas_muestra, 
                            replace = FALSE) 

# Filtrar el dataframe de población para quedarnos solo con las UPM seleccionadas
data_upm_seleccionadas <- poblacion[poblacion$ID_Escuela %in% UPM_seleccionadas, ]

# ETAPA 2: Seleccionar 15 estudiantes (USM) dentro de CADA UPM seleccionada

# Creamos una función para seleccionar USM por escuela
seleccionar_usm <- function(df_escuela) {
  # Selecciona 'm_estudiantes_por_escuela' estudiantes de esta escuela
  muestra_usm <- df_escuela[sample(nrow(df_escuela), 
                                   size = m_estudiantes_por_escuela, 
                                   replace = FALSE), ]
  return(muestra_usm)
}

# Aplicar la función a cada grupo (escuela) de las UPM seleccionadas
muestra_final <- do.call(rbind, 
                         by(data_upm_seleccionadas, 
                            data_upm_seleccionadas$ID_Escuela, 
                            seleccionar_usm))

# 4. Resultado
cat("Total de escuelas seleccionadas (UPM):", length(unique(muestra_final$ID_Escuela)), "\n")
cat("Total de estudiantes seleccionados (USM):", nrow(muestra_final), "\n")
print(head(muestra_final))
