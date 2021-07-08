# Machine-learning-avanzado

Este proyecto es un diseño de detección de outliers para identificar hectáreas de un terreno, si contienen malezas o cultivo. 
El modelo se entrena con datos de cosecha 2021 para ser usado en cosechas posteriores.
Partimos de un dataset de 37 variables y 4435 observaciones que contiene información satelital de un terreno dividido según hectáreas. La información incorpora una variable con evidencia sobre la situación de algunas de las hectáreas (5%), por lo que se trata de un modelo semi-supervisado. El objetivo es encontrar outliers en los datos para investigar estos posteriormente in situ, ahorrando significativamente los costos de esta tarea. 

rm(list=ls())

install.packages("outliers")
library(outliers)


############################### ETAPA 1 #######################################

setwd("C:/Users/Lenovo Flex/Downloads/Obligatorio-20210501")


# cargar datos
x = read.csv('campo.csv')

# inspeccionamos primeras filas
head(x)

# cantidad de casos
n = nrow(x)

# evidencia # verificamos cuantos son respecto al total
evid = sum(x[,37])
(n - evid)/n


#### PCA ######

# aplicamos PCA para analizar si se puede simplificar el dataset
# tambien porque nos permite visualizar en dos dimensiones 
# lo que es mejor que en 37 para interpretar

# hacer PCA con toda la base menos la ultima columna que es la evidencia (V37)
# escalar dentro del comando PRCOMP (mas sencillo)
p = prcomp(x[,-37], scale. = TRUE)

# resumen del elemento obtenido con PCA
# indica que porcion de la varianza total explica cada componente
summary(p)

# en el resumen vemos que al segundo componente hay 85% de varianza acumulada 
# por lo tanto PCA funciona para el objetivo de simplicar las variables

# omitimos hacer los loadings porque las columnas no estan etiquetadas
# solo tenemos numeros, entonces no podemos interpretar que es cada una
# y la dinamica entre ellas

# screeplot: grafico de varianza 
plot(p)


###### SCORES PCA ######

### se obtienen los scores dentro de los componentes
# todos los datos reales tienen un semejante en terminos de componente
# primeros 2 scores (2 componentes mas importantes ya que eran los que tendran 
# mas % de varianza acumulada)
p$x[,1:2]


###### VISUALIZACION #########

# separamos la variable "evidencia" para identificarla en el grafico
h = x[,37]

################# GRAFICO 1: DATOS y EVIDENCIA

# ploteamos los primeros dos componentes de PCA junto con la evidencia
plot(p$x[,1:2], col = h + 2, pch = 16)
# color sera h + 2, por lo tanto cuando h es cero sera 2 y sino sera 3

# se puede identificar visualmente en que zona estan los outliers (centro)




############################### ETAPA 2 #######################################

# identificacion de outliers univariados en cada una de las 37 variables
# partimos nuevamente del dataset original
# busca outliers en las observaciones que tienen outliers en al menos 1 variable

##### Z score #####
z = scores(x[,-37], type = "z") #excluyendo la V37 (evidencia)

##### MAD score #####
m = scores(x[,-37], type = "mad") #excluyendo la V37 (evidencia)
 

##### identificando outliers ####

# definimos k en 3 (que son 3 sigmas de desviaciÃ³n)
k = 3

##### para Z score #####
# creamos vector logico con outliers (de todas las dimensiones y observaciones)
v1 = (abs(z) > k)
# contamos la cantidad de outliers por variable
colSums(v1)

##### para MAD score #####
# creamos vector logico con outliers (de todas las dimensiones y observaciones)
v2 = (abs(m) > k)
# contamos la cantidad de outliers por variable
colSums(v2)

# el MAD score encuentra un numero mas importante de outliers que el Z score


###### VISUALIZACION ######

### Z score
#### observamos las observaciones que son outliers segun lo definido antes
r1 = rowSums(v1) > 0
sum(r1)

### MAD score
#### observamos las observaciones que son outliers segun lo definido antes
r2 = rowSums(v2) > 0
sum(r2)

# El MAD identifica mas observaciones como outliers


########### GRAFICO 2: DATOS (componentes) y OUTLIERS UNIVARIADOS

# Z 
# visualizacion sobre Scores PCA
plot(p$x[,1:2], col = r1*1+3, pch = 16)


# Mad
# visualizacion sobre Scores PCA
plot(p$x[,1:2], col = r2*1+3, pch = 16)

# identificamos los outliers en la data original con todas sus variables
# usamos PCA para poder visualizarlo en 2 dimensiones que simplifican 
# la data original
# el mad pareciera un mejor trabajo identificando los outliers

# boxplot de PCA
boxplot(p$x[,1:2])

#### comparar con la evidencia brindada
 
# Z
table(univariado = r1, evidencia = x$evidence)
# eso significa que usando Z scores de los 218 que la evidencia indica que 
# son normales 8 fueron identificados como outliers erroneamente
# por lo tanto el metodo no fue muy efectivo


# MAD
table(univariado = r2, evidencia = x$evidence)
# aunque parecia que MAD performaba mejor finalmente en la tabla vemos que 
# identifica erroneamente mas casos que el Z score

# conclusion:
# Es necesario buscar otro metodo mas eficiente


############################### ETAPA 3 #####################################

#### distancias^2 euclideas al origen #####

#### calcular distancias: elevamos todos los z al cuadrado y sumamos por fila
# obtenemos la distancia de cada observacion al origen (en las 37 variables)
e = rowSums(z^2)

### graficamos en un boxplot (solo para visualizar)
boxplot(e)

### identificando outliers: TEST DE HIPOTESIS CHI2

### definir k: formula de chi2 indicando probabilidad y grados de libertad
pr = 0.995
d = ncol(z)
k1 = qchisq(pr , d)

###### outliers encontrados con test chi2
o = e > k1 #### vector logico (falsos y verdaderos)
sum(o) ##### suma solo los verdaderos

######### VISUALIZACION ######################

plot(p$x[,1:2], col = o*1 +3, pch = 16)

###### TABLA COMPARATIVA CON EVIDENCIA #######

table(multivariado1 = o, evidencia = x$evidence)

# comparado con los resultados anteriores parece tener peor resultado 
# ya que estÃ¡n quedando mÃ¡s valores que son normales (por evidencia) como outliers


########################### ETAPA 4 ################################


######## distancias Mahalanobis al origen ########

#### mahalanobis
m = mahalanobis(x[,-37], colMeans(x[,-37]), cov(x[,-37]))

##### identificando outliers
prob = 0.99
df = ncol(z)
k2 = qchisq(prob , df)

#---outliers con test chi2
om = m > k2
sum(om)

########## VISUALIZACION  ###################

plot(p$x[,1:2], col = om*1 + 3, pch = 16)

# en la visualizacion parece que los outliers se encuentran en la zona correcta

########## TABLA COMPARATIVA CON EVIDENCIA #########

table(multivariado2 = om, evidencia = x$evidence)

# sigue identificando parcialmente los datos de la evidencia




########################### ETAPA 5 #####################################

### aplicamos un metodo mas adecuado para distribuciones no normales
###### dbscan

# usamos los datos escalados almacenados en Z

install.packages("dbscan")
library(dbscan)

####### densidad

# fijamos MinPts = cantidad de variables + 1 ###
# es el umbral de decision sobre la cantidad de vecinos a considerar para cada punto
minpts = 37

# plot: identificamos elbow
kNNdistplot(z, minpts)

##### fijar eps con calibracion
# mirando el grafico en el entorno de 3 se despega la linea
eps = 3
  
###### dbscan
q = dbscan(z, eps = eps, minPts = minpts)

#### identificando outliers

#### outliers DBSCAN # Regla (vector logico)
odbscan = q$cluster < 1

#### cantidad de outliers
sum(odbscan)

# encontramos un numero un poco menor de clusters al metodo anterior


############# VISUALIZACION #####################

# grafica segun los clusters asignados por tipo de punto
plot(p$x[,1:2], col = q$cluster+2, pch = 16)

####### tabla comparativa con evidencia ##########

table(dbscan = odbscan, evidencia = x$evidence)

## mejoró la tabla de contingencia
# el DBScan encontró 215 outliers, de los cuales solo 2 teníamos evidencia que eran normales

table(q$cluster)

################ PROBAMOS CON OTRA CALIBRACIÓN DE EPS ########################
##############################################################################

####### densidad

# fijamos MinPts = cantidad de variables + 1 ###
# es el umbral de decision sobre la cantidad de vecinos a considerar para cada punto
minpts = 37

# plot: identificamos elbow
kNNdistplot(z, minpts)

##### fijar eps con calibracion
# probamos con una calibracion un poco mayor al anterior
eps = 3.7

###### dbscan
q = dbscan(z, eps = eps, minPts = minpts)

#### identificando outliers

#### outliers DBSCAN # Regla (vector logico)
odbscan = q$cluster < 1

#### cantidad de outliers
sum(odbscan)

# encontró mucho menos outliers que la calibración anterior


############# VISUALIZACION #####################

# grafica segun los clusters asignados por tipo de punto
plot(p$x[,1:2], col = q$cluster+2, pch = 16)

####### tabla comparativa con evidencia ##########

table(dbscan = odbscan, evidencia = x$evidence)

# luego de analizar otra calibración concluimos que esta última se ajustó mejor

