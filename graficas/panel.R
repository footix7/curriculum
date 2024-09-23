# Esta función es la que confecciona el área destinada al nombre.
# Última modificación: 12 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$panel <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  
  ##### Pared #####
  dataGrafica$pared <- data.frame(
    x = 0, xend = 1,
    y = 0, yend = 1
  )
  
  dataGrafica$pared$x <-
    (dataGrafica$pared$x * (x[2] - x[1])) + x[1]
  dataGrafica$pared$xend <-
    (dataGrafica$pared$xend * (x[2] - x[1])) + x[1]
  dataGrafica$pared$y <-
    (dataGrafica$pared$y * (y[2] - y[1])) + y[1]
  dataGrafica$pared$yend <-
    (dataGrafica$pared$yend * (y[2] - y[1])) + y[1]
  ##### Pared #####
  #### Adecuación de datos ####
  
  #### Gráfica ####
  grafica <- grafica +
    geom_polygon(
      mapping = aes(
        x = c(
          dataGrafica$pared$x, dataGrafica$pared$xend, dataGrafica$pared$xend,
          dataGrafica$pared$x
        ),
        y = c(
          dataGrafica$pared$y, dataGrafica$pared$y, dataGrafica$pared$yend,
          dataGrafica$pared$yend
        )
      ),
      fill = colores$principal
    ) +
    geom_segment(
      mapping = aes(
        x = x[1], xend = x[1],
        y = 0, yend = 1
      ),
      col = colores$detalles
    ) %>%
    ggfx::with_shadow(
      sigma = 10,
      x_offset = -10
    )
  #### Gráfica ####
  
  return(grafica)
} 