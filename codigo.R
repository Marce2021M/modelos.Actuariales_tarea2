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


##-----------------------------------------------------------------------------------
## RESULTADOS

library(knitr)

simulacionSamples <- function(myCopula1,myCopula2, n=10000, seed=191654){
    # Generar muestras de la cópula
    set.seed(seed)
    samples <- rCopula(n, myCopula1) # caso positivo
    samples2 <- rCopula(n, myCopula2) # caso negativo

    # Convertir las muestras a distribuciones exponenciales

    ## caso positivo
    samples[,1] <- qexp(samples[,1], rate = params1)
    samples[,2] <- qexp(samples[,2], rate = params2)

    ## caso negativo
    samples2[,1] <- qexp(samples2[,1], rate = params1)
    samples2[,2] <- qexp(samples2[,2], rate = params2)


    return(list(samples, samples2))
}

simulacionSamples2 <- function(myCopula1,myCopula2, n=10000, seed=191654){
    # Generar muestras de la cópula
    set.seed(seed)
    samples <- rCopula(n, myCopula1) # caso positivo
    samples2 <- rCopula(n, myCopula2) # caso negativo

    # Convertir las muestras a distribuciones exponenciales

    ## caso positivo
    samples[,1] <- qpareto(samples[,1], shape = alpha, scale = pareto_params1)
    samples[,2] <- qpareto(samples[,2], shape = alpha, scale = pareto_params2)

    ## caso negativo
    samples2[,1] <- qpareto(samples2[,1], shape = alpha, scale = pareto_params1)
    samples2[,2] <- qpareto(samples2[,2], shape = alpha, scale = pareto_params2)

    return(list(samples, samples2))
}


imprimirResultadosTabla <- function(samples1, samples2) {
    
    # INICIALIZAMOS VARIABLES
    ## Caso positivo
    X_Positivo <- samples1[,1]
    Y_Positivo <- samples1[,2]
    Sum_XY_Positivo <- X_Positivo + Y_Positivo

    ## Caso negativo
    X_Negative <- samples2[,1]
    Y_Negative <- samples2[,2]
    Sum_XY_Negative <- X_Negative + Y_Negative

    # Varianza
    var_X <- (1/params1)^2
    var_Y <- (1/params2)^2
    ## Caso Positivo
    var_Sum_XY_Positivo <- var(Sum_XY_Positivo)
    ## Caso Negativo
    var_Sum_XY_Negative <- var(Sum_XY_Negative)

    # Desviación estándar
    sd_X <- sqrt(var_X)
    sd_Y <- sqrt(var_Y)
    ## Caso Positivo
    sd_Sum_XY_Positivo <- sd(Sum_XY_Positivo)
    ## Caso Negativo
    sd_Sum_XY_Negative <- sd(Sum_XY_Negative)

    # Cuantil al 95%
    quantile_X_95 <- -(1/params1)*log(1-0.95)
    quantile_Y_95 <- -(1/params2)*log(1-0.95)
    ## Caso positivo
    quantile_Sum_XY_95_Positivo <- quantile(Sum_XY_Positivo, 0.95)
    ## Caso Negativo
    quantile_Sum_XY_95_Negative <- quantile(Sum_XY_Negative, 0.95)

    # Valor esperado condicional de la cola al 95%
    ## Caso positivo
    tvar_95_X <- quantile_X_95 + (1/params1)*exp(-params1*quantile_X_95)/(1-.95)
    tvar_95_Y <- quantile_Y_95 + (1/params2)*exp(-params2*quantile_Y_95)/(1-.95)
    mean_Sum_XY_Tail_95_Positivo <- mean(Sum_XY_Positivo[Sum_XY_Positivo > quantile_Sum_XY_95_Positivo])
    ## Caso Negativo
    mean_Sum_XY_Tail_95_Negative <- mean(Sum_XY_Negative[Sum_XY_Negative > quantile_Sum_XY_95_Negative])

  # Crear data frames para los resultados
  resultadosNegativos <- data.frame(
    Metrica = c("Varianzas", "Desviaciones Estándar", "Cuantiles al 95%", "Valor esperado condicional de la cola al 95%"),
    X = c(var_X, sd_X, quantile_X_95, tvar_95_X),  # Suponiendo que el valor 2 es un ejemplo
    Y = c(var_Y, sd_Y, quantile_Y_95, tvar_95_Y),
    `sum(X,Y)` = c(var_Sum_XY_Negative, sd_Sum_XY_Negative, quantile_Sum_XY_95_Negative, mean_Sum_XY_Tail_95_Negative)
  )

  resultadosPositivos <- data.frame(
    Metrica = c("Varianzas", "Desviaciones Estándar", "Cuantiles al 95%", "Valor esperado condicional de la cola al 95%"),
    X = c(var_X, sd_X, quantile_X_95, tvar_95_X),
    Y = c(var_Y, sd_Y, quantile_Y_95, tvar_95_Y),
    `sum(X,Y)` = c(var_Sum_XY_Positivo, sd_Sum_XY_Positivo, quantile_Sum_XY_95_Positivo, mean_Sum_XY_Tail_95_Positivo)
  )

  #  resultados con kable
  tabla1 <- resultadosNegativos %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

  tabla2 <- resultadosPositivos %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
  return(list(tabla1, tabla2))
}

# Ejemplo de uso
# imprimirResultadosTabla(var_X, var_Y, var_Sum_XY_Negative, var_Sum_XY_Positivo, sd_X, sd_Y, sd_Sum_XY_Negative, sd_Sum_XY_Positivo, quantile_X_95, quantile_Y_95, quantile_Sum_XY_95_Negative, quantile_Sum_XY_95_Positivo, mean_Sum_XY_Tail_95_Negative, mean_Sum_XY_Tail_95_Positivo)

imprimirResultadosTabla2 <- function(samples1, samples2) {
    
    # INICIALIZAMOS VARIABLES
    ## Caso positivo
    X_Positivo <- samples1[,1]
    Y_Positivo <- samples1[,2]
    Sum_XY_Positivo <- X_Positivo + Y_Positivo

    ## Caso negativo
    X_Negative <- samples2[,1]
    Y_Negative <- samples2[,2]
    Sum_XY_Negative <- X_Negative + Y_Negative

    # Varianza
    var_X <- (alpha*pareto_params1^2)/(((alpha-1)^2)*(alpha-2))
    var_Y <- (alpha*pareto_params2^2)/(((alpha-1)^2)*(alpha-2))
    ## Caso Positivo
    var_Sum_XY_Positivo <- var(Sum_XY_Positivo)
    ## Caso Negativo
    var_Sum_XY_Negative <- var(Sum_XY_Negative)

    # Desviación estándar
    sd_X <- sqrt(var_X)
    sd_Y <- sqrt(var_Y)
    ## Caso Positivo
    sd_Sum_XY_Positivo <- sd(Sum_XY_Positivo)
    ## Caso Negativo
    sd_Sum_XY_Negative <- sd(Sum_XY_Negative)

    # Cuantil al 95%
    quantile_X_95 <- pareto_params1*((1-0.95)^(-1/alpha)-1)
    quantile_Y_95 <- pareto_params2*((1-0.95)^(-1/alpha)-1)
    ## Caso positivo
    quantile_Sum_XY_95_Positivo <- quantile(Sum_XY_Positivo, 0.95)
    ## Caso Negativo
    quantile_Sum_XY_95_Negative <- quantile(Sum_XY_Negative, 0.95)

    # Valor esperado condicional de la cola al 95%
    ## Caso positivo
    tvar_95_X <- quantile_X_95 + (pareto_params1/(alpha-1))*((pareto_params1/(quantile_X_95+pareto_params1))^(alpha-1))/(1-.95)
    tvar_95_Y <- quantile_Y_95 + (pareto_params2/(alpha-1))*((pareto_params2/(quantile_Y_95+pareto_params2))^(alpha-1))/(1-.95) 
    mean_Sum_XY_Tail_95_Positivo <- mean(Sum_XY_Positivo[Sum_XY_Positivo > quantile_Sum_XY_95_Positivo])
    ## Caso Negativo
    mean_Sum_XY_Tail_95_Negative <- mean(Sum_XY_Negative[Sum_XY_Negative > quantile_Sum_XY_95_Negative])

  # Crear data frames para los resultados
  resultadosNegativos <- data.frame(
    Metrica = c("Varianzas", "Desviaciones Estándar", "Cuantiles al 95%", "Valor esperado condicional de la cola al 95%"),
    X = c(var_X, sd_X, quantile_X_95, tvar_95_X),  # Suponiendo que el valor 2 es un ejemplo
    Y = c(var_Y, sd_Y, quantile_Y_95, tvar_95_Y),
    `sum(X,Y)` = c(var_Sum_XY_Negative, sd_Sum_XY_Negative, quantile_Sum_XY_95_Negative, mean_Sum_XY_Tail_95_Negative)
  )

  resultadosPositivos <- data.frame(
    Metrica = c("Varianzas", "Desviaciones Estándar", "Cuantiles al 95%", "Valor esperado condicional de la cola al 95%"),
    X = c(var_X, sd_X, quantile_X_95, tvar_95_X),
    Y = c(var_Y, sd_Y, quantile_Y_95, tvar_95_Y),
    `sum(X,Y)` = c(var_Sum_XY_Positivo, sd_Sum_XY_Positivo, quantile_Sum_XY_95_Positivo, mean_Sum_XY_Tail_95_Positivo)
  )

  # resultados con kable
  tabla1 <- resultadosNegativos %>%
  kable("html") %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))

  tabla2 <- resultadosPositivos %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
  return(list(tabla1, tabla2))
}

# Ejemplo de uso
# imprimirResultadosTabla(var_X, var_Y, var_Sum_XY_Negative, var_Sum_XY_Positivo, sd_X, sd_Y, sd_Sum_XY_Negative, sd_Sum_XY_Positivo, quantile_X_95, quantile_Y_95, quantile_Sum_XY_95_Negative, quantile_Sum_XY_95_Positivo, mean_Sum_XY_Tail_95_Negative, mean_Sum_XY_Tail_95_Positivo)
