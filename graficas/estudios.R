# Esta función es la que confecciona el área destinada al historial de estudios.
# Última modificación: 12 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$estudios <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = c(0.02, 0.87, 0.04, 0.02, 0.66, 0.04, 0.02, 0.50, 0.04),
    y = c( 0.8,  0.8,  0.7, 0.55, 0.55, 0.45, 0.30, 0.30, 0.20),
    label = c(
      "Bachillerato Científico-Tecnológico",  "en", 
      "Salesianas Valencia (2016 - 18)", "Grado en Ciencia de Datos", 
      "en la", "Universitat de València (2018 - 22)",
      "Curso de Javascript", "en la", "Universitat O. de Catalunya (2023 - 24)"
    ),
    col = c(
      colores$detalles, colores$secundario, colores$secundario,
      colores$detalles, colores$secundario, colores$secundario,
      colores$detalles, colores$secundario, colores$secundario
    ),
    fontface = c(
      "bold", "plain", "plain", "bold", "plain", "plain", "bold", "plain",
      "plain"
    )
  )
  
  dataGrafica$texto$x <- (dataGrafica$texto$x * (x[2] - x[1])) + x[1]
  dataGrafica$texto$y <- (dataGrafica$texto$y * (y[2] - y[1])) + y[1]
  ##### Texto #####
  
  ##### Polígonos #####
  dataGrafica$poligonos <- rbind(
    data.frame(
      x = c(0.01, 0.86, 0.86, 0.01),
      y = c(0.745, 0.745, 0.855, 0.855),
      group = "bachiller"
    ),
    data.frame(
      x = c(0.01, 0.65, 0.65, 0.01),
      y = c(0.495, 0.495, 0.605, 0.605),
      group = "grado"
    ),
    data.frame(
      x = c(0.01, 0.49, 0.49, 0.01),
      y = c(0.245, 0.245, 0.355, 0.355),
      group = "curso"
    )
  )
  
  dataGrafica$poligonos$x <- (dataGrafica$poligonos$x * (x[2] - x[1])) + x[1]
  dataGrafica$poligonos$y <- (dataGrafica$poligonos$y * (y[2] - y[1])) + y[1]
  ##### Polígonos #####
  
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