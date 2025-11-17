### RUIDO BLANCO

# Crear 1000 puntos aleatorios
set.seed(100)
w <- rnorm(1000) # Normal estándar

# Observar las gráficas de ruido blanco, ACF y PACF
par(mfrow = c(2, 2)) # Función para dividir el visualizador de gráficos
plot_wn <- plot(w, type = 'l', col = 'cornflowerblue', main='Ruido Blanco', xlab = 't', ylab='x_t')
acf_wn <- acf(w, col = 'cornflowerblue', main='ACF Ruido Blanco')
pacf_wn <- pacf(w, col = 'cornflowerblue', main='PACF Ruido Blanco', ylim = c(-1, 1))


### CAMINATA ALEATORIA

# Crear un vector conteniendo los mismos componentes de ruido blanco
walk <- cumsum(w)

# Observar las gráficas de la caminata, ACF y PACF
par(mfrow = c(2,2))
plot(walk,type = 'l', col = 'darkgreen', main='Caminata Aleatoria', xlab = 't', ylab='x_t')
acf(walk, col = 'darkgreen', main='ACF Caminata Aleatoria')
pacf(walk, col = 'darkgreen', main='PACF Caminata Aleatoria')

### CAMINATA ALEATORIA DRIFT

# Crear un vector conteniendo el ruido blanco más un drift
drift <- w + .05
# Suma acumulativa para hacer la caminata
walk_drift <- cumsum(drift)

# Observar las gráficas de la caminata, ACF y PACF
par(mfrow = c(2,2))
plot(walk_drift,type = 'l', col = 'orange', main='Caminata Aleatoria', xlab = 't', ylab='x_t')
acf(walk_drift, col = 'orange', main='ACF Caminata Aleatoria')
pacf(walk_drift, col = 'orange', main='PACF Caminata Aleatoria')

### COMPARACIONES

# Series
par(mfrow = c(3,3))
plot(w,type = 'l', xlab = 't', ylab='x_t')
plot(walk,type = 'l', xlab = 't', ylab='x_t')
plot(walk_drift,type = 'l', xlab = 't', ylab='x_t')

# ACF
acf(w)
acf(walk)
acf(walk_drift)

# PACF
pacf(w, ylim = c(-1, 1))
pacf(walk)
pacf(walk_drift)
