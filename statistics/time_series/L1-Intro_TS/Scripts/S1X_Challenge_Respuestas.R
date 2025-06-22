# Poner el propio directorio
setwd("C:/Users/emanu/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L1-Intro_TS/Scripts")

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
plot(cbind(Elec.ts, Beer.ts, Choc.ts))

layout(1:1)
plot(Elec.ts, 
     xlab = "Tiempo (Años)")

layout(1:2)
# Elabora un gráfico que muestre más claramente la tendencia
plot(aggregate(Elec.ts),
     xlab = 'Tiempo')

# Elabora un gráfico que muestre más claramente la estacionalidad
boxplot(Elec.ts~cycle(Elec.ts))

# ¿Qué observas?
