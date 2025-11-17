setwd("C:/Users/EmanuelMejia/OneDrive - Firedrop/Github/Public/courses/statistics/time_series/L4-Transform_Suavizamiento/Scripts")

# Cargar datos y convertir en ts
motor <- read.table("../data/motororg.dat", header = T)
quejas.ts <- ts(motor$complaints,
                start = c(1996,1),
                freq = 12)

# Gráfico Inicial
plot(quejas.ts,
     xlab = "Fecha",
     ylab = "Quejas",
     main = "Cartas de queja a organización automovilística")

# El suavizamiento exponencial es un caso especial
# Del algoritmo Holt-Winters

# Ponemos otros parámetors en FALSE
quejas.manual <- HoltWinters(quejas.ts,
                             alpha = 0.3,
                             beta = FALSE,
                             gamma = FALSE)

# Observemos parámetros
quejas.manual

# Si removemos el valor de alpha
# R lo calculará en automático
quejas.auto <- HoltWinters(quejas.ts,
                           beta = FALSE,
                           gamma = FALSE)

quejas.auto

# Grafiquemos ambos escenarios
par(mfrow = c(2, 1))

# Alpha ingresado manual
plot(quejas.manual, 
     lwd = 2,
     main = "Suavizamiento exponencial - Alfa Manual")

# Alpha ingresado automático
plot(quejas.auto, 
     lwd = 2,
     main = "Suavizamiento exponencial - Alfa Automática")

### Holt-Winters
# Cargamos Datos
vinoAus <- read.table("../data/aus_wine.dat", header = T)
head(vinoAus)

# Serie de tiempo de ventas del vino dulce blanco 
blanDulce <- ts(vinoAus$sweetw, 
             start = c(1980,1),
             freq = 12)

par(mfrow = c(1, 1))
plot(blanDulce,
     xlab = "Fecha",
     ylab = "Ventas (miles de Litros)",
     main = "Ventas de Vino Dulce Blanco")

hwadit.manual <- HoltWinters(blanDulce,
                             alpha = 0.2,
                             beta = 0.2,
                             gamma = 0.2)

hwadit.manual

hwadit.auto <- HoltWinters(blanDulce)
hwadit.auto


# Grafiquemos ambos escenarios
par(mfrow = c(2, 1))

# Parámetros Manuales
plot(hwadit.manual, 
     lwd = 2,
     main = "Holt-Winters - Param Manual")

# Alpha ingresado automático
plot(hwadit.auto, 
     lwd = 2,
     main = "Holt-Winters - Param Auto")

# Holt-Winters con estacional multiplicativo
hwmult.auto <- HoltWinters(blanDulce, seasonal = "mult")
hwmult.auto

# Comparando aditivo con multiplicativo
plot(hwadit.auto, 
     lwd = 2,
     main = "Holt-Winters - Estacional Aditivo")

plot(hwmult.auto, 
     lwd = 2,
     main = "Holt-Winters - Estacional Multiplicativo")

# Predicción de 4 años
blanDulce.pred <- predict(hwmult.auto, n.ahead = 48)

# Gráficamente
par(mfrow = c(1, 1))
ts.plot(blanDulce, 
        blanDulce.pred, 
        lty = c(2,1),
        col = c("black","red"),
        xlab = "Fecha",
        ylab = "Ventas (miles de Litros)",
        main = "Ventas de Vino Dulce Blanco")

# Datos de pasajeros!!! 
pas <- AirPassengers
pas.HW <- HoltWinters(pas, seasonal = "mult")

plot(pas.HW, 
     lwd = 2,
     main = "Reservaciones de Pasajeros - Holt Winters")

# ¿Una predicción?
pas.pred <- predict(pas.HW, n.ahead = 60)
ts.plot(pas, 
        pas.pred, 
        lty = c(2,1),
        col = c("black","red"),
        xlab = "Fecha",
        ylab = "Pasajeros (1000s)",
        main = "Reservaciones de Pasajeros")
