# Esta función es la que confecciona el área destinada al nombre.
# Última modificación: 12 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$creatividad <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Punto #####
  dataGrafica$punto <- data.frame(
    x = 0.5,
    y = 0.8
  )
  
  dataGrafica$punto$x <- (dataGrafica$punto$x * (x[2] - x[1])) + x[1]
  dataGrafica$punto$y <- (dataGrafica$punto$y * (y[2] - y[1])) + y[1]
  ##### Punto #####
  
  ##### Título #####
  dataGrafica$titulo <- data.frame(
    x = 0.5,
    y = 0.54,
    label = "Creatividad"
  )
  
  dataGrafica$titulo$x <- (dataGrafica$titulo$x * (x[2] - x[1])) + x[1]
  dataGrafica$titulo$y <- (dataGrafica$titulo$y * (y[2] - y[1])) + y[1]
  ##### Título #####
  
  ##### Segmento #####
  dataGrafica$segmento <- data.frame(
    x = 0.175, xend = 0.825,
    y = 0.50, yend = 0.50
  )
  
  
  dataGrafica$segmento$x <- (dataGrafica$segmento$x * (x[2] - x[1])) + x[1]
  dataGrafica$segmento$y <- (dataGrafica$segmento$y * (y[2] - y[1])) + y[1]
  dataGrafica$segmento$xend <-
    (dataGrafica$segmento$xend * (x[2] - x[1])) + x[1]
  dataGrafica$segmento$yend <-
    (dataGrafica$segmento$yend * (y[2] - y[1])) + y[1]
  ##### Segmento #####
  
  ##### Desarrollo #####
  dataGrafica$desarrollo <- data.frame(
    x = 0.5,
    y = 0.3,
    label = paste0(
      "Suelo encontrar soluciones\ndiferentes, aunque me\ngustaría acompañarla",
      "s\ncon algo más de estética." 
    )
  )
  
  dataGrafica$desarrollo$x <- (dataGrafica$desarrollo$x * (x[2] - x[1])) + x[1]
  dataGrafica$desarrollo$y <- (dataGrafica$desarrollo$y * (y[2] - y[1])) + y[1]
  ##### Título #####
  #### Adecuación de datos ####
  
  #### Gráfica ####
  grafica <- grafica +
    geom_point(
      data = dataGrafica$punto,
      mapping = aes(
        x = x,
        y = y
      ),
      size = 8,
      col = colores$principal
    ) %>%
    ggfx::with_shadow(
      sigma = 10
    ) +
    geom_point(
      data = dataGrafica$punto,
      mapping = aes(
        x = x - 1/250,
        y = y + 1/250
      ),
      size = 2.5,
      col = "white"
    ) %>%
    ggfx::with_blur(
      sigma = 12
    ) + 
    geom_text(
      data = dataGrafica$titulo,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      family = "tipografia",
      size = 15,
      col = colores$secundario,
      fontface = "bold",
      vjust = 0
    ) + 
    geom_segment(
      data = dataGrafica$segmento,
      mapping = aes(
        x = x, xend = xend,
        y = y, yend = yend
      ),
      col = colores$detalle
    ) +
    geom_text(
      data = dataGrafica$desarrollo,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      family = "tipografia",
      size = 8.5,
      col = colores$secundario,
      lineheight = 1/3
    )
  #### Gráfica ####
  
  return(grafica)
} 