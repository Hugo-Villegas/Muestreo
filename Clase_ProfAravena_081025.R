#install.packages("samplingbook")
library(samplingbook)

## libreria "samplingbook" ya esta cargada
# tamaño de muestra "total" - estratificado
stratasize(e=25,Nh=c(.15*4000,.4*4000,.45*4000),
           Sh=c((2000-800)/4,(800-300)/4,(300-150)/4), type="prop") 

## Tamaño necesario = 122
stratasamp(122,Nh=c(.15*4000,.4*4000,.45*4000),
           Sh=c((2000-800)/4,(800-300)/4,(300-150)/4), type="prop")


##Muestreo optimizante
stratasize(e=20,Nh=c(.15*4000,.4*4000,.45*4000),
           Sh=c((2000-800)/4,(800-300)/4,(300-150)/4), type="opt") 

## Tamaño necesario = 115
stratasamp(115,Nh=c(.15*4000,.4*4000,.45*4000),
           Sh=c((2000-800)/4,(800-300)/4,(300-150)/4), type="opt")





library(dplyr)
library(readxl)
library(samplingbook)

# lectura de los datos
base = readxl::read_excel("Precenso-comunas.xlsx", skip=1)
head(base)
tail(base)

# corregir la lectura
base = readxl::read_excel("Precenso-comunas.xlsx", skip = 1)
head(base)
tail(base)
names(base)

names(base) <- stringr::str_replace_all(names(base), " |-", "_")
names(base)
head(base)
nrow(base)

# Muestreo Estratificado #

# Vamos a realizar un muestreo de manzanas por estratificado por comuna
# La idea es estimar la media de mujeres por manzana en el territorio
# con un error menor o igual a 5
# Conocimiento experto:

# Simularemos que un experto nos entrega la informacion que necesitamos #
# base %>% 
#   group_by(Comuna_texto) %>% 
#   summarise(N  = n(),
#             sd = sd(Población_M)) -> Sum; Sum

# Comuna_texto     N    sd
# <chr>        <int>   <dbl>
# 1 La Reina       609  104 
# 2 Las Condes    1396  139 
# 3 Peñalolén     1564  58


N_need <- stratasize(e=5, Nh = c(609, 1396, 1564), Sh = c(104, 139, 58), type = "prop")
N_need <- N_need$n; N_need

Num_estrat <- stratasamp(N_need, Nh = c(609, 1396, 1564), Sh = c(104, 139, 58), type = "prop")
Num_estrat


# Realizar el muestreo #
head(base)
Nh = table(base$Comuna_texto)
Nh

# Definir semilla
set.seed(08102025)
n1 = 194
muestra1 <- base %>% filter(Comuna_texto == "La Reina") %>% sample_n(n1, replace = FALSE)
nrow(muestra1)
table(muestra1$Comuna_texto)
head(muestra1)
tail(muestra1)



n2 = 446
muestra2 <- base %>% filter(Comuna_texto == "Las Condes") %>% sample_n(n2, replace = FALSE)
nrow(muestra2)
table(muestra2$Comuna_texto)

n3 = 499
muestra3 <- base %>% filter(Comuna_texto == "Peñalolén") %>% sample_n(n3, replace = FALSE)
nrow(muestra3)
table(muestra3$Comuna_texto)

# juntar las muestras
muestra_comunas <- muestra1 %>% bind_rows(muestra2) %>% bind_rows(muestra3)
head(muestra_comunas)
nrow(muestra_comunas)
table(muestra_comunas$Comuna_texto)

stratamean(muestra_comunas$Población_M, muestra_comunas$Comuna_texto, Nh, eae = TRUE)

