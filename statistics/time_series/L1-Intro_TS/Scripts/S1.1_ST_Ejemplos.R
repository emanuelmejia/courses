# Poner el propio directorio
setwd("C:/Users/emanu/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L1-Intro_TS/Scripts")

# Número de pasajeros por mes en una aerolinea
pas <- AirPassengers
pas

# Tipo de objeto
class(pas)

# Funciones específicas de los objetos ts
start(pas); end(pas); frequency(pas)

# Creando un gráfico de esta variable
# ¿Qué notamos en este gráfico?
plot(pas, ylab = 'Pasajeros (miles)')

layout(1:2)

# Obtener una visión más clara de la tendencia
plot(aggregate(pas),
     ylab = 'Pasajeros (miles)',
     xlab = 'Fecha')

# Resumen de los valores para cada estación
boxplot(pas~cycle(pas))

## Segundo ejemplo Tipo de cambio GBP a NZ Dollar
Z <- read.table("../data/pounds_nz.dat", header = T)
class(Z)

# Utilizar función ts para convertir en una serie de tiempo
# Esta serie inicia en 1991 y son periodos trimestrales
Z.ts <- ts(Z, st = 1991, fr = 4) 
class(Z.ts)

# Gráfico de la serie de tiempo
layout(1:1)
plot(Z.ts, 
     xlab = "Tiempo (Años)",  
     ylab = "Tipo de cambio trimestral $NZ/ libra")

# Utilizamos función window para partir en 2 series
Z.92.96 <- window(Z.ts, start = c(1992, 1), end = c(1996, 1))
Z.96.98<- window(Z.ts, start = c(1996, 1), end = c(1998, 1))
# ¿Qué tipo de objeto tenemos?
class(Z.92.96)

# Veamos las subseries
layout (1:2)
plot(Z.92.96, ylab = "Tipo de cambio trimestral $NZ/ libra",  xlab = "Tiempo (Años)")  
plot(Z.96.98, ylab = "Tipo de cambio trimestral $NZ/ libra",  xlab = "Tiempo (Años)")

# Podemos hacer los mismos gráficos que aprendimos sobre las subseries
plot(aggregate(Z.92.96),
     ylab = 'Tipo de cambio trimestral $NZ/ libra',
     xlab = 'Tiempo')

# Resumen de los valores para cada estación
boxplot(Z.92.96~cycle(Z.92.96))
