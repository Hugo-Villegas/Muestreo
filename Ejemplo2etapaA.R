## MUETREO CONGLOMERADO EN DOS ETAPAS-



library(sampling)
library(Frames2)
library(dplyr)
# Población
datos <- data.frame(
  comuna = rep(c("A", "B", "C", "D", "E"), each = 4),
  computadores = c(25, 30, 28, 22,   
                   35, 40, 38, 36,   
                   15, 18, 12, 20,   
                   10, 12, 9, 11,    
                   22, 24, 20, 26),
  ID_colegio = 1:20 # ID único para cada colegio
)

N_comunas <- 5 # Total de conglomerados (comunas)
n_comunas <- 2 # Número de conglomerados a seleccionar

# Calcular el tamaño de cada comuna (M_i)

## Vemos cuantos datos tiene cada comuna (4 filas cada comuna)
Mi <- datos %>% 
  group_by(comuna) %>% 
  summarise(Mi = n()) %>% 
  pull(Mi)


# Probabilidades de inclusión de primer orden (π_i)
# $\pi_i$ (Probabilidad de Inclusión de Primer Orden): La probabilidad de que el elemento $i$ 
# (por ejemplo, una persona, una empresa, una parcela, etc.) sea elegido para formar parte de 
# la muestra.

?inclusionprobabilities
pik_PPT_sinrep <- inclusionprobabilities(Mi, n_comunas)
# Muestra de conglomerados (comunas)
set.seed(456)
?UPsystematic
cong_sel_idx <- UPsystematic(pik_PPT_sinrep) == 1 ##PARA QUE SALGA UN VECTOR BOOLEANO
## con este cong_sel_ide da TRUE FALSE TRUE FALSE FALSE
?unique
comunas_seleccionadas <- unique(datos$comuna)[cong_sel_idx]


## HASTA AQUI ELEGIMOS DOS COMUNAS "A" Y "C".

#####################################################################
##### AHORA LA SEGUNDA ETAPA DE SUBMUESTREO DE COLEGIO CON M.A.S..
#####################################################################

# Seleccionamos $m_i$ colegios dentro de cada comuna seleccionada. 
# Para simplificar, elegiremos $m_i = 2$ colegios en cada una.

m_i <- 2 # Número de colegios a submuestrear en cada comuna seleccionada

muestra_cong_2etapas <- data.frame()
for (comuna_i in comunas_seleccionadas) {
  # Colegios de la comuna seleccionada
  colegios_comuna <- datos %>% filter(comuna == comuna_i)
  Mi_i <- nrow(colegios_comuna)
  
  # Selección MAS de m_i colegios
  set.seed(match(comuna_i, c("A", "B", "C", "D", "E")) + 100) # Semilla diferente por comuna
  colegios_sel_idx <- sample(1:Mi_i, m_i, replace = FALSE)
  
  # Agregar a la muestra final
  muestra_cong_2etapas <- bind_rows(muestra_cong_2etapas, 
                                    colegios_comuna[colegios_sel_idx, ])
}

# La muestra final:
print("Muestra por Conglomerados en Dos Etapas:")
print(muestra_cong_2etapas)

# Resultado del muestreo: Dos comunas fueron elegidas, y de cada una, dos colegios.

##############################################################################

# A CONTINUACION LA ESTIMACION DE LA MEDIA POBLACIONAL
##############################################################################



# Parámetros para el cálculo
Mi_sel <- datos %>% 
  filter(comuna %in% comunas_seleccionadas) %>% 
  group_by(comuna) %>% 
  summarise(Mi = n()) %>% 
  pull(Mi)

pi_i_sel <- pik_PPT_sinrep[cong_sel_idx]

# Total estimado de computadores (τ̂)
tau_hat_num_i <- muestra_cong_2etapas %>%
  group_by(comuna) %>%
  summarise(
    sum_yij = sum(computadores),
    Mi = n(), # En este caso, Mi es el tamaño del conglomerado, pero el submuestreo es m_i=2
    m_i = n(), # m_i es el número de colegios submuestreados
    .groups = 'drop'
  ) %>%
  mutate(
    Mi_real = Mi_sel[match(comuna, comunas_seleccionadas)],
    pi_i = pi_i_sel[match(comuna, comunas_seleccionadas)],
    # Componente del numerador: (M_i / (pi_i * m_i)) * sum(y_ij)
    term_i = (Mi_real / (pi_i * m_i)) * sum_yij 
  )
tau_hat <- sum(tau_hat_num_i$term_i)

# Total estimado de colegios (M̂)
M_hat_den_i <- Mi_sel / pi_i_sel
M_hat <- sum(M_hat_den_i)

# Promedio estimado (μ̂)
mu_hat_cong_2etapas <- tau_hat / M_hat

print(paste("Estimación del Promedio de Computadores por Colegio (Conglomerados 2 Etapas):", round(mu_hat_cong_2etapas, 2)))
print(paste("Promedio Real Poblacional:", round(mean(datos$computadores), 2)))
