# 1. Cargar las librerías necesarias y sino instalarlas 

se pueden instalar con el siguiente comando install.packages("libreria") desde Rstudio

las librerias son las siguietes:

1. tidyverse
2. arules

se debe de tener en consideracion que para las reglas FP-GROWTH se tiene que instalar por medio de un archivo comprimido (https://borgelt.net/fim4r.html) utilizando el instalador de RStudio 

# 2. Descomprimir el archivo dataset en la misma carpeta donde se encuentre en archivo R

# 3. Se pueden consultar los diccionarios de datos en la carpeta "Diccionarios"

# 4. Conformacion del archivo R

El archivo R esta conformado de chunks siendo el primero la carga de datos los siguientes 4 las reglas apriori y los siguientes 4 las reglas FP-Growth

# 5. Consideraciones extras

Cada regla Apriori y FP-Growth tiene un cluster K-Means por si se desea graficar o indicar en cada chunk



# Cargar bibliotecas necesarias
library(ggplot2)
library(arules)
library(fim4r)

# Cargar y preparar datos de violencia
data <- read.csv('violenciav2.csv', sep=",")
data <- subset(data, VIOLENCIA_ID != 99999)  # Eliminar filas con VIOLENCIA_ID igual a 99999

# Mostrar datos
data

# Cargar datos de gastos
datagastos <- read.csv('gastos.csv', sep=",")

# Cargar datos de migración
datamigracion <- read.csv('migracion.csv', sep=",")
datamigracion2 <- read.csv('migracion2.csv', sep=",")

# REGLA 1 APRIORI

# Filtrar columnas innecesarias en el dataset de gastos
datagastos <- datagastos[, !(names(datagastos) %in% c("PEA", "POCUPA", "PEI"))]

# Filtrar datos específicos para análisis
datamsc <- subset(datagastos, P13B05 == 1 & DOMINIO == 3 & POBREZA == 1)

# Seleccionar columnas de interés para la regla apriori
datamsc2 <- datamsc[, c(5,10,13)]

# Aplicar algoritmo apriori para generar reglas
reglas <- apriori(datamsc2, parameter = list(support=0.3, confidence=0.3))

# Mostrar las reglas generadas ordenadas por lift
inspect(head(sort(reglas, by = "lift", decreasing = TRUE), 50))

# Eliminar valores NA en el dataset
datamsc2 <- na.omit(datamsc2)

# Clustering con k-means
cluster <- kmeans(datamsc2, centers=2)

# Graficar los resultados del clustering
ggplot(datamsc2, aes(x=ID_GASTOSMP ,y=P13B06, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=ID_GASTOSMP ,y=P13B06), color="black", size=4, shape=17)+
  labs(title = "ID GASTO VS ¿Cuánto gastaron en total durante el mes pasado en (...)?")+
  theme_minimal()

# REGLA 2 APRIORI

# Filtrar columnas innecesarias en el dataset de gastos
datagastos <- datagastos[, !(names(datagastos) %in% c("PEA", "POCUPA", "PEI"))]

# Filtrar datos específicos para análisis
datamsc <- subset(datagastos, P13B05 == 1 & DOMINIO == 1 & POBREZA == 3)

# Seleccionar columnas de interés para la regla apriori
datamsc2 <- datamsc[, c(5,10,13)]

# Aplicar algoritmo apriori para generar reglas
reglas <- apriori(datamsc2, parameter = list(support=0.3, confidence=0.3))

# Mostrar las reglas generadas ordenadas por lift
inspect(head(sort(reglas, by = "lift", decreasing = TRUE), 20))

# Eliminar valores NA en el dataset
datamsc2 <- na.omit(datamsc2)

# Clustering con k-means
cluster <- kmeans(datamsc2, centers=2)

# Graficar los resultados del clustering
ggplot(datamsc2, aes(x=ID_GASTOSMP ,y=P13B06, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=ID_GASTOSMP ,y=P13B06), color="black", size=4, shape=17)+
  labs(title = "ID GASTO VS ¿Cuánto gastaron en total durante el mes pasado en (...)?")+
  theme_minimal()

# REGLA 3 APRIORI

# Filtrar columnas innecesarias en el dataset de migración
datamigracion <- datamigracion[, !(names(datamigracion) %in% c("PEA", "POCUPA", "PEI"))]

# Filtrar datos específicos para análisis
datamsc <- subset(datamigracion, P01I01A == 1)

# Seleccionar columnas de interés para la regla apriori
datamsc2 <- datamsc[, c(1,10)]

# Aplicar algoritmo apriori para generar reglas
reglas <- apriori(datamsc2, parameter = list(support=0.3, confidence=0.3))

# Mostrar las reglas generadas ordenadas por lift
inspect(head(sort(reglas, by = "lift", decreasing = TRUE), 20))

# Eliminar valores NA en el dataset
datamsc2 <- na.omit(datamsc2)

# Clustering con k-means
cluster <- kmeans(datamsc2, centers=3)

# Graficar los resultados del clustering
ggplot(datamsc2, aes(x=P01I01B ,y=DEPTO, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=P01I01B ,y=DEPTO), color="black", size=4, shape=17)+
  labs(title = "Cuántas personas del hogar viven actualmente en otro país vs Departamento")+
  theme_minimal()

# REGLA 4 APRIORI

# Filtrar columnas innecesarias en el dataset de migración
datamigracion2 <- datamigracion2[, !(names(datamigracion2) %in% c("PEA", "POCUPA", "PEI"))]

# Filtrar datos específicos para análisis
datamsc <- subset(datamigracion2, DOMINIO == 1)
datamsc3 <- subset(datamsc, POBREZA == 3)

# Seleccionar columnas de interés para la regla apriori
datamsc2 <- datamsc3[, c(10, 11,13)]

# Aplicar algoritmo apriori para generar reglas
reglas <- apriori(datamsc2, parameter = list(support=0.3, confidence=0.3))

# Mostrar las reglas generadas ordenadas por lift
inspect(head(sort(reglas, by = "lift", decreasing = TRUE), 60))

# Eliminar valores NA en el dataset
datamsc3 <- na.omit(datamsc3)

# Clustering con k-means
cluster <- kmeans(datamsc3, centers=3)

# Graficar los resultados del clustering
ggplot(datamsc3, aes(x=P01I03 ,y=P01I05, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=P01I03 ,y=P01I05), color="black", size=4, shape=17)+
  labs(title = "DEPTO vs ¿Cuántas personas del hogar viven actualmente en otro país?")+
  theme_minimal()

# REGLA 1 FPGROTH

# Filtrar datos para análisis
datamsc <- subset(data, P02A04B == 6)

# Seleccionar columnas de interés para la regla FPGrowth
datamsc2 <- datamsc[, c(10,11,23)]

# Eliminar datos faltantes
datamsc3 <- datamsc[, c(10,23)]

# Aplicar algoritmo FPGrowth para generar reglas
reglas <- fim4r(datamsc2, method = "fpgrowth", target = "rules", supp = .2, conf = .2)

# Mostrar las reglas generadas
inspect(head(sort(reglas, by = "lift"), 20))

# Eliminar valores NA en el dataset
datamsc3 <- na.omit(datamsc3)

# Clustering con k-means
cluster <- kmeans(datamsc3, centers=3)

# Graficar los resultados del clustering
ggplot(datamsc3, aes(x=ID_SEGURIDAD ,y=P02A07., color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=ID_SEGURIDAD ,y=P02A07.), color="black", size=4, shape=17)+
  labs(title = "ID Seguridad VS Cuál fue la razón principal para no presentar la denuncia")+
  theme_minimal()

# REGLA 2 FPGROTH

# Filtrar datos para análisis
datamsc <- subset(data, P02A02 >= 10)

# Seleccionar columnas de interés para la regla FPGrowth
datamsc2 <- datamsc[, c(10,15, 16)]

# Aplicar algoritmo FPGrowth para generar reglas
reglas <- fim4r(datamsc2, method = "fpgrowth", target = "rules", supp = .2, conf = .2)

# Mostrar reglas generadas
inspect(reglas[])

# Eliminar valores NA en el dataset
datamsc2 <- na.omit(datamsc2)

# Clustering con k-means
cluster <- kmeans(datamsc2, centers=3)

# Graficar los resultados del clustering
ggplot(datamsc2, aes(x=ID_SEGURIDAD ,y=P02A04B, color = as.factor(cluster$cluster)))+
  geom_point()+
  geom_point(data = as.data.frame(cluster$centers), aes(x=ID_SEGURIDAD ,y=P02A04B), color="black", size=4, shape=17)+
  labs(title = "ID Seguridad VS Cuál fue la razón principal para no presentar la denuncia")+
  theme_minimal()

