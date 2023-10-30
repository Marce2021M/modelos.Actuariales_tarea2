library(copula)
library(lattice)
library(actuar)

graficaDensidadCopulas <- function(myCopulaP,myCopulaN, n=10000, seed=191654){

# Generar muestras de la cópula
set.seed(seed)
samples <- rCopula(n, myCopulaP) # caso positivo
samples2 <- rCopula(n, myCopulaN) # caso negativo

# Convertir las muestras a distribuciones exponenciales

## caso positivo
samples[,1] <- qexp(samples[,1], rate = params1)
samples[,2] <- qexp(samples[,2], rate = params2)

## caso negativo
samples2[,1] <- qexp(samples2[,1], rate = params1)
samples2[,2] <- qexp(samples2[,2], rate = params2)


library(ks)

# Estimación de la densidad
dens <- kde(as.matrix(samples)) # caso positivo
dens2 <- kde(as.matrix(samples2)) # caso negativo

library(plotly)

# Convertir la estimación de densidad a un formato adecuado

## caso positivo
x <- dens$eval.points[[1]] 
y <- dens$eval.points[[2]]
z <- matrix(dens$estimate, nrow = length(x), ncol = length(y))
## caso negativo
x2 <- dens2$eval.points[[1]] 
y2 <- dens2$eval.points[[2]]
z2 <- matrix(dens2$estimate, nrow = length(x2), ncol = length(y2))


# Crear gráficos interactivos con plotly
p <- plot_ly(x = ~x, y = ~y, z = ~z) %>% add_surface()
p2 <- plot_ly(x = ~x2, y = ~y2, z = ~z2) %>% add_surface()

return(list(p,p2))
}

graficaDensidadCopulas2 <- function(myCopulaP,myCopulaN, n=10000, seed=191654){

# Generar muestras de la cópula
set.seed(seed)
samples <- rCopula(n, myCopulaP) # caso positivo
samples2 <- rCopula(n, myCopulaN) # caso negativo

# Convertir las muestras a distribuciones exponenciales

## caso positivo
samples[,1] <- qpareto(samples[,1], shape = alpha, scale = pareto_params1)
samples[,2] <- qpareto(samples[,2], shape = alpha, scale = pareto_params2)

## caso negativo
samples2[,1] <- qpareto(samples2[,1], shape = alpha, scale = pareto_params1)
samples2[,2] <- qpareto(samples2[,2], shape = alpha, scale = pareto_params2)


library(ks)

# Estimación de la densidad
dens <- kde(as.matrix(samples)) # caso positivo
dens2 <- kde(as.matrix(samples2)) # caso negativo

library(plotly)

# Convertir la estimación de densidad a un formato adecuado

## caso positivo
x <- dens$eval.points[[1]] 
y <- dens$eval.points[[2]]
z <- matrix(dens$estimate, nrow = length(x), ncol = length(y))
## caso negativo
x2 <- dens2$eval.points[[1]] 
y2 <- dens2$eval.points[[2]]
z2 <- matrix(dens2$estimate, nrow = length(x2), ncol = length(y2))


# Crear gráficos interactivos con plotly
p <- plot_ly(x = ~x, y = ~y, z = ~z) %>% add_surface()
p2 <- plot_ly(x = ~x2, y = ~y2, z = ~z2) %>% add_surface()

return(list(p,p2))
}

graficaDependenciaCopulas2D <- function(myCopulaP, myCopulaN, n = 10000, seed = 123, alpha = 1, pareto_params1 = 1, pareto_params2 = 1) {
  # Cargar librerías necesarias
  library(copula)
  library(ggplot2)

  # Configurar la semilla para reproducibilidad
  set.seed(seed)

  # Generar muestras de la cópula
  samplesP <- rCopula(n, myCopulaP) # Caso de dependencia positiva
  samplesN <- rCopula(n, myCopulaN) # Caso de dependencia negativa

  # Convertir las muestras a distribuciones Pareto
  # Asumiendo que qpareto es una función adecuada para generar cuantiles de Pareto
  samplesP <- apply(samplesP, 2, function(x) qpareto(x, shape = alpha, scale = pareto_params1))
  samplesN <- apply(samplesN, 2, function(x) qpareto(x, shape = alpha, scale = pareto_params2))

  # Crear data frames para gráficos
  dfP <- data.frame(X = samplesP[, 1], Y = samplesP[, 2], Case = 'Positive Dependence')
  dfN <- data.frame(X = samplesN[, 1], Y = samplesN[, 2], Case = 'Negative Dependence')
  df <- rbind(dfP, dfN)

  # Gráfico de dispersión con ggplot2
  p <- ggplot(df, aes(x = X, y = Y, color = Case)) +
    geom_point(alpha = 0.5) +
    theme_minimal() +
    ggtitle("Dependence between Variables Using Copulas") +
    labs(x = "Variable 1", y = "Variable 2", color = "Dependence Type")

  return(p)
}
# graficaDependenciaCopulas2D(myCopulaP, myCopulaN)
