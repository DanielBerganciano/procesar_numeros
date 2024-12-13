#Funcion para leer el archivo y devolver un vector de números
leer_numeros <- function(nombre_archivo) {
  if (!file.exists(nombre_archivo)) {
    stop("El archivo no existe: ", nombre_archivo)
  }
  
  numeros <- as.integer(readLines(nombre_archivo))
  return(numeros)
}

#Funcion principal del script
procesar_numeros <- function() {
  archivo_entrada <- "numeros.txt"
  numeros <- leer_numeros(archivo_entrada)
  
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion_estandar <- sd(numeros)
  
  if (desviacion_estandar > 10) {
    cat("Alta variabilidad detectada en los datos.\n")
  }
  
  cuadrados <- sapply(numeros, function(x) x^2)
  
  archivo_salida <- "resultados.txt"
  
  cat("# Resultados del análisis\n", file = archivo_salida)
  cat("Media: ", media, "\n", file = archivo_salida, append = TRUE)
  cat("Mediana: ", mediana, "\n", file = archivo_salida, append = TRUE)
  cat("Desviación estándar: ", desviacion_estandar, "\n", file = archivo_salida, append = TRUE)
  if (desviacion_estandar > 10) {
    cat("Alta variabilidad detectada.\n", file = archivo_salida, append = TRUE)
  }
  cat("\n# Cuadrados de los números\n", file = archivo_salida, append = TRUE)
  cat(cuadrados, sep = "\n", file = archivo_salida, append = TRUE)
  
  cat("Los resultados se han guardado en el archivo resultados.txt.\n")
}

procesar_numeros()