library(copula)
library(lattice)

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