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
