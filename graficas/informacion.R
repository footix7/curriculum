# Esta función es la que confecciona el área destinada a resumir en qué ha
  # consistido mi trabajo en los últimos dos años.
# Última modificación: 1 de octubre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$informacion <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Título #####
  dataGrafica$titulo <- data.frame(
    x = 0,
    y = 1,
    label = "En los últimos dos años"
  )
  
  dataGrafica$titulo$x <- (dataGrafica$titulo$x * (x[2] - x[1])) + x[1]
  dataGrafica$titulo$y <- (dataGrafica$titulo$y * (y[2] - y[1])) + y[1]
  ##### Título #####
  
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = c(0.02,  0.46, 0.46, 0.46, 0.02),
    y = c(0.85, 0.95, 0.6, 0.225, 0.375),
    label = c(
      paste0(
        "He desarrollado un nuevo\nprocedimiento que permite el\ncálculo del R",
        "OI en campañas\nde márquetin." 
      ),
      paste0(
        "He participado activamente en la\nelaboración de un nuevo proceso de ",
         "\nla segmentación de públicos."
      ),
      paste0(
        "He reducido a minutos el trabajo de las\nsegmentaciones recurrentes q",
        "ue antes\nduraba semanas."  
      ),
      paste0(
        "He creado paquetes de funciones en R\npara agilizar el análisis de da",
        "tos que se\nhace particularmente en Consum."
      ),
      paste0(
        "He impartido un curso de SQL\npara dotar a mis compañeras\nde auto",
        "nomía en las consultas\nde extracción de datos."  
      )
    )
  )
  
  dataGrafica$texto$x <- (dataGrafica$texto$x * (x[2] - x[1])) + x[1]
  dataGrafica$texto$y <- (dataGrafica$texto$y * (y[2] - y[1])) + y[1]
  ##### Texto #####
  
  ##### Segmentos #####
  dataGrafica$segmentos <- data.frame(
    x = c(0.05, 0.49, 0.49, 0.44),
    xend = c(0.35, 0.9, 0.9, 0.44),
    y = c(0.43, 0.64, 0.26, 0.1),
    yend = c(0.43, 0.64, 0.26, 0.9)
  )
  
  dataGrafica$segmentos$x <- (dataGrafica$segmento$x * (x[2] - x[1])) + x[1]
  dataGrafica$segmentos$xend <-
    (dataGrafica$segmentos$xend* (x[2] - x[1])) + x[1]
  dataGrafica$segmentos$y <- (dataGrafica$segmento$y * (y[2] - y[1])) + y[1]
  dataGrafica$segmentos$yend <-
    (dataGrafica$segmentos$yend * (y[2] - y[1])) + y[1]
  ##### Segmentos #####
  #### Adecuación de datos ####
  
  #### Gráfica ####
  grafica <- grafica +
    geom_polygon(
      data = dataGrafica$puntos,
      mapping = aes(
        x = x,
        y = y
      ),
      fill = colores$principal
    ) + 
    geom_text(
      data = dataGrafica$titulo,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      size = 12,
      fontface = "bold",
      family = "tipografia",
      hjust = 0,
      vjust = 1,
      col = colores$secundario
    ) + 
    geom_text(
      data = dataGrafica$texto,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      size = 12,
      hjust = 0,
      vjust = 1,
      family = "tipografia",
      col = colores$secundario,
      lineheight = 1/3
    ) +
    geom_segment(
      data = dataGrafica$segmentos,
      mapping = aes(
        x = x, xend = xend,
        y = y, yend = yend
      ),
      col = colores$principal,
      linewidth = 0.15
    )
  #### Gráfica ####
  
  return(grafica)
} 