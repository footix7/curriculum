# Esta función es la que confecciona el área destinada al nombre.
# Última modificación: 12 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$mensaje <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  dataGrafica <- data.frame(
    x = 0.5,
    y = 0.5,
    label = paste0(
      "Para consultar el código\nque ha generado este\ncurrículum, acceder a\n",
      "la siguiente página web o\nescanear el código QR" 
    )
  )
  
  dataGrafica$x <- (dataGrafica$x * (x[2] - x[1])) + x[1]
  dataGrafica$y <- (dataGrafica$y * (y[2] - y[1])) + y[1]
  
  grafica <- grafica + 
    geom_text(
      data = dataGrafica,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      size = 12,
      fontface = "bold",
      col = colores$detalles,
      lineheight = 1/3
    )
  
  return(grafica)
} 