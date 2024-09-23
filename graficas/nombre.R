# Esta función es la que confecciona el área destinada al nombre.
# Última modificación: 12 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$nombre <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica  <- list()
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = 0,
    y = 0.5,
    label = "Ignacio Valero"
  )
  
  dataGrafica$texto$x <- (dataGrafica$texto$x * (x[2] - x[1])) + x[1]
  dataGrafica$texto$y <- (dataGrafica$texto$y * (y[2] - y[1])) + y[1]
  ##### Texto #####
  
  ##### Segmento #####
  dataGrafica$segmento <- data.frame(
    x = 0.1, xend = 0.875,
    y = 0.125, yend = 0.125
  )
  
  dataGrafica$segmento$x <- (dataGrafica$segmento$x * (x[2] - x[1])) + x[1]
  dataGrafica$segmento$y <- (dataGrafica$segmento$y * (y[2] - y[1])) + y[1]
  dataGrafica$segmento$xend <-
    (dataGrafica$segmento$xend * (x[2] - x[1])) + x[1]
  dataGrafica$segmento$yend <-
    (dataGrafica$segmento$yend * (y[2] - y[1])) + y[1]
  ##### Segmento #####
  #### Adecuación de datos ####
  
  #### Gŕafica ####
  grafica <- grafica +
    geom_segment(
      data = dataGrafica$segmento,
      mapping = aes(
        x = x, xend = xend,
        y = y, yend = yend
      ),
      col = colores$principal,
      linewidth = 1/4
    ) + 
    geom_text(
      data = dataGrafica$texto,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      family = "tipografia",
      hjust = -0.05,
      size = 40,
      col = colores$secundario
    )
  #### Gŕafica ####
  
  return(grafica)
} 