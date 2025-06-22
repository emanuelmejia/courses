# Poner el propio directorio
setwd("TU CÓDIGO AQUÍ")

## Lectura de archivo
CBE <- read.table("../data/cbe.dat", header = T)

# Creamos objetos ts para cada una de las variables
# Cada una se las series se refiere a la producción mensual
# En Australia para los periodos de 1958 a 1990 de:

# Electricidad (millones de kWh)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)  
# cerveza (millones de litros)
Beer.ts <- ts(CBE[, 2], start = 1958, freq = 12)  
# Chocolate (toneladas)
Choc.ts <- ts(CBE[, 1], start = 1958, freq = 12) 

# Elabora un gráfico de la serie de tiempo
# TU CÓDIGO AQUÍ

# Elabora un gráfico que muestre más claramente la tendencia
# TU CÓDIGO AQUÍ

# Elabora un gráfico que muestre más claramente la estacionalidad
# TU CÓDIGO AQUÍ

# ¿Qué observas?
# TU RESPUESTA AQUÍ