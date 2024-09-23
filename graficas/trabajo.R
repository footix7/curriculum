# Esta función es la que confecciona el área destinada al nombre.
# Última modificación: 12 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$trabajo <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = c(0.05, 0.555, 0.07, 0.05, 0.61, 0.07),
    y = c(0.65, 0.65, 0.55, 0.35, 0.35, 0.25),
    label = c(
      "Becario en Mercados", "en el", "departamiento de SCyMk (2021)",
      "Técnico en Fidelización", "en el", "departamento de SCyMk (2022 - )"
    ),
    col = c(
      colores$detalles, colores$secundario,colores$secundario,
      colores$detalles, colores$secundario, colores$secundario
    ),
    fontface = c(
      "bold", "plain", "plain", "bold", "plain", "plain"
    )
  )
  
  dataGrafica$texto$x <- (dataGrafica$texto$x * (x[2] - x[1])) + x[1]
  dataGrafica$texto$y <- (dataGrafica$texto$y * (y[2] - y[1])) + y[1]
  ##### Texto #####
  
  ##### Polígonos #####
  dataGrafica$poligonos <- rbind(
    data.frame(
      x = c(0.04, 0.55, 0.55, 0.04),
      y = c(0.595, 0.595, 0.705, 0.705),
      group = "becario"
    ),
    data.frame(
      x = c(0.04, 0.6, 0.6, 0.04),
      y = c(0.295, 0.295, 0.405, 0.405),
      group = "tecnico"
    )
  )
  
  dataGrafica$poligonos$x <- (dataGrafica$poligonos$x * (x[2] - x[1])) + x[1]
  dataGrafica$poligonos$y <- (dataGrafica$poligonos$y * (y[2] - y[1])) + y[1]
  ##### Polígonos #####
  
  ##### Segmento #####
  dataGrafica$segmento <- data.frame(
    x = 0, xend = 0,
    y = 0.1, yend = 0.9
  )
  
  dataGrafica$segmento$x <- (dataGrafica$segmento$x * (x[2] - x[1])) + x[1]
  dataGrafica$segmento$y <- (dataGrafica$segmento$y * (y[2] - y[1])) + y[1]
  dataGrafica$segmento$xend <- (dataGrafica$segmento$xend * (x[2] - x[1])) + x[1]
  dataGrafica$segmento$yend <- (dataGrafica$segmento$yend * (y[2] - y[1])) + y[1]
  ##### Segmento #####
  #### Adecuación de datos ####
  
  #### Gráfica ####
  grafica <- grafica +
    geom_polygon(
      data = dataGrafica$poligonos,
      mapping = aes(
        x = x,
        y = y,
        group = group
      ),
      fill = colores$principal,
      alpha = 0.4
    ) +
    geom_segment(
      data = dataGrafica$segmento,
      mapping = aes(
        x = x, xend = xend,
        y = y, yend = yend
      ),
      col = colores$principal,
      linewidth = 0.15
    ) +
    geom_text(
      data = dataGrafica$texto,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      hjust = 0,
      family = "tipografia",
      size = 10.5,
      col = dataGrafica$texto$col,
      fontface = dataGrafica$texto$fontface,
      lineheight = 1/3
    )
  #### Gráfica ####
  
  return(grafica)
} 