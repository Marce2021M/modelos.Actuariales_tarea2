# Datos de ejemplo
x <- c(-9, 4, 10, 6, 8,12,15,9,26)  # Reemplaza esto con tus datos para x
y <- c(0,3,20,2,8,29,27,16,30) # Reemplaza esto con tus datos para y

# Calcula el coeficiente de correlación de Pearson
correlacion_pearson <- cor(x, y, method="spearman")

# Imprime el resultado
print(correlacion_pearson)
