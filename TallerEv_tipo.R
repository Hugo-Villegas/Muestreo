
## Tamaño de muestras MAS
## a1) Media cn un error del 3 ptos y S=30
## a2) proporción con un error del 2% y P=6%



# Diplomado
## Muestreo
base <- rio::import("Establecimientos.xlsx")  # BASE ESTABLECIMIENTOS
head(base)
library(samplingbook)
# M.A.S.
Ntot <- nrow(base) # Asigna a NTOT el numero total de datos
table(base$Region) -> t1  # num de casos por region
sample.size.mean(e=3, S=30, N=Ntot)-> n1
sample.size.prop(e=0.02, P=0.06, N=Ntot) -> n2
## Se indicó S=30 y el P=0.06 
n = max(n1$n, n2$n)
n1$n
n2$n

# el mayor tamaño garantiza cumplir ambos criterios.
## se necesitan 542 establecimiento..
# se seleccionan 542 "lineas" la base de establecimientos
# mediante un m.a.s



set.seed(13102025) # fecha de nacimiento (todos tendrán diferente)
casos<-sample(Ntot, n, replace=F)
muestra = base[casos,] #selecciona las linea que estan seleccionada con los casos
summary(muestra)
addmargins(table(muestra$Region)) # numero establecimiento por region y total
muestra$riesgo <- ifelse(muestra$MAT < 200, 1, 0)
Smean(muestra$MAT, N=Ntot)
Sprop(muestra$riesgo, N=Ntot)
# Estimaciones por Region (estratos)
# ver como están los nombres...
Valp <- muestra[muestra$Region=="DE VALPARA?SO",]
Smean(Valp$MAT,N=t1[1])
Sprop(Valp$riesgo,N=t1[1])
Biob <- muestra[muestra$Region=="DEL BIOB?O",]
Smean(Biob$MAT,N=t1[2])
Sprop(Biob$riesgo,N=t1[2])
Metrop <- muestra[muestra$Region=="METROPOLITANA DE SANTIAGO",]
Smean(Metrop$MAT,N=t1[3])
Sprop(Metrop$riesgo,N=t1[3])
###########################
# M.Estratificado


Nh=c(t1)
Sh=c(25,35,30)
Ph=c(.054, 0.102, .038)
stratasize(e=3, Sh=Sh, Nh=Nh, type="opt")->n1

PQ <- sqrt(Ph*(1-Ph))
stratasize(e=.02, Sh=PQ, Nh=Nh, type="opt")->n2
n=max(n1$n,n2$n)
n

# Afijación por estratos..
#el tamaño optimo es obtenido con PQ, asi que
stratasamp(n=n, Sh=PQ, Nh=Nh, type="opt")->nopt
set.seed(18102025)
m1<-sample(Nh[1],nopt[2,1], replace=F)
muestra1 = base[base$Region=="DE VALPARA?SO",][m1,]
head(muestra1)
dim(muestra1)

m2<-sample(Nh[2],nopt[2,2], replace=F)
muestra2 = base[base$Region=="DEL BIOB?O",][m2,]
dim(muestra2)

m3<-sample(Nh[3],nopt[2,3], replace=F)
muestra3 = base[base$Region=="METROPOLITANA DE SANTIAGO",][m3,]
dim(muestra3)

muestraR = rbind(muestra1, muestra2, muestra3)
table(muestraR$Region)
muestraR$riesgo <- ifelse(muestraR$MAT < 200, 1, 0)
summary(muestraR)
stratamean(muestraR$MAT, muestraR$Region, Nh, eae=TRUE)
stratamean(muestraR$riesgo, muestraR$Region, Nh, eae=TRUE)*100
######### FIN ##