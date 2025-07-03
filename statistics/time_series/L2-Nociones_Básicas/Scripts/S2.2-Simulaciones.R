library(tidyverse)
set.seed(100)

### Simulaciones de Ruido Blanco

# Función de creación de una serie de ruido blanco
w <-function(rep=100){
  w=rnorm(n=rep,mean=0,sd=1)
  return(w)
}

# Crear un DF con índice que replique 100 veces la función
sim_w <- data.frame(
  t = seq(from=1,to=100,by=1),
  replicate(w(),n=100))

# Convertir en DF de columna
sim_w <- sim_w%>% pivot_longer(
  cols= starts_with("x"),
  names_to ="sim",
  values_to="value"
)

# Gráfica de las 100 simulaciones
ggplot(sim_w,aes(x=t, y=value, col=sim))+
  geom_line()+
  labs(title = "Ruido Blanco",
       subtitle = "100 Simulaciones",
       x = "t",
       y = "wt") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8",
                              size = 17,
                              face = "bold"),
    plot.subtitle = element_text(color = "#969696", size = 13, face = "italic"),
    axis.title = element_text(color = "#969696",
                              size = 10,
                              face = "bold"),
    axis.text = element_text(color = "#969696", size = 10),
    axis.line = element_line(color = "#969696"),
    legend.position="none"
  )+
  xlab("t")+ylab("wt")+
  geom_hline(aes(yintercept =-1.96), color = "blue4") +
  geom_hline(aes(yintercept = 1.96), color = "blue4")



### Simulaciones Caminata Aleatoria

# Función de creación de una caminata aleatoria
rw_no_drift<-function(rep=100){
  w = rnorm(rep,0,1)
  rwnd = cumsum(w)
  return(rwnd)
}

# Crear un DF con índice que replique 100 veces la función
sim_rwnd <- data.frame(
  t=seq(from=1, to=100,by=1),
  replicate(rw_no_drift(),n=100)
  )

# Convertir en DF de columna
sim_rwnd <- sim_rwnd%>% pivot_longer(
  cols=starts_with("x"),
  names_to="sim",
  values_to="value"
)

# Gráfica de las 100 simulaciones
ggplot(sim_rwnd,aes(x=t,y=value, col=sim))+
  geom_line()+
  labs(title = "Caminata Aleatoria sin Drift",
       subtitle = "100 Simulaciones",
       x = "t",
       y = "xt") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8",
                              size = 17,
                              face = "bold"),
    plot.subtitle = element_text(color = "#969696", size = 13, face = "italic"),
    axis.title = element_text(color = "#969696",
                              size = 10,
                              face = "bold"),
    axis.text = element_text(color = "#969696", size = 10),
    axis.line = element_line(color = "#969696"),
    legend.position="none"
  )

# Simulaciones Caminata con Drift

# Función de creación de una caminata aleatoria con drift
rw_drift<-function(rep=100){
  w = rnorm(rep,0,1)
  wd = 0.3 + w
  rwd = cumsum(wd)
  return(rwd)
}

# Crear un DF con índice que replique 100 veces la función
sim_rwd <- data.frame(
  t=seq(from=1, to=100,by=1),
  replicate(rw_drift(),n=100)
  )

# Convertir en DF de columna
sim_rwd <- sim_rwd%>% pivot_longer(
  cols = starts_with("x"),
  names_to="sim",
  values_to="value"
)

# Gráfica de las 100 simulaciones
ggplot(sim_rwd,aes(x=t, y=value, col=sim))+
  geom_line()+
  labs(title = "Caminata Aleatoria con Drift",
       subtitle = "100 Simulaciones",
       x = "t",
       y = "xt") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "#0099F8",
                              size = 17,
                              face = "bold"),
    plot.subtitle = element_text(color = "#969696", size = 13, face = "italic"),
    axis.title = element_text(color = "#969696",
                              size = 10,
                              face = "bold"),
    axis.text = element_text(color = "#969696", size = 10),
    axis.line = element_line(color = "#969696"),
    legend.position="none"
  )
