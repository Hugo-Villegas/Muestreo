

# Establecer la semilla para reproducibilidad
set.seed(42)

# Definir el tamaño total de la población (N)
N <- 1000

# Crear la población de costos (valores en miles de dólares)
poblacion_costos <- round(rnorm(N, mean = 500, sd = 150), 2)

# El tamaño de la población (N) que usarías en las fórmulas
cat("Tamaño total de la población (N):", N, "\n")


# Definir el tamaño de la muestra (n). Ej: vamos a revisar el 10% de los costos.
n <- 100
cat("Tamaño de la muestra a seleccionar (n):", n, "\n")


# Seleccionar la muestra de costos
muestra_costos <- sample(poblacion_costos, size = n, replace = FALSE)

# Ver los primeros costos de la muestra
head(muestra_costos)


# 1. Media Muestral (Estimación del Costo Medio Poblacional)
media_muestral <- mean(muestra_costos)

# 2. Desviación Estándar Muestral (s)
s <- sd(muestra_costos)

cat("\nEstimación del Costo Medio (Media Muestral): $", round(media_muestral, 2), "\n")
cat("Desviación Estándar Muestral (s): $", round(s, 2), "\n")

# T-Test realiza la estimación de la media y el IC automáticamente
resultado_ttest <- t.test(muestra_costos, conf.level = 0.95)

cat("\n--- Resultados (T-Test) ---\n")
cat("Intervalo de Confianza del 95%: $", round(resultado_ttest$conf.int[1], 2), " a $", round(resultado_ttest$conf.int[2], 2), "\n")
