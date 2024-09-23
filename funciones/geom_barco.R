# Esta función devuelve el dibujo de un barco que ocupa el espacio dado como
  # argumento en la función.
# Última modificación: 22 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # x (list): contiene la coordenada máxima y mínima sobre el eje x del
    # cuadrilatero que formará la base de la gráfica.
  # y (list): contiene la coordenada máxima y mínima sobre el eje y del
    # cuadrilatero que formará la base de la gráfica.
# Return: Una variable que contiene una gráfica ggplot.

geom_barco <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Poligonos #####
  dataGrafica$poligonos <- rbind(
    data.frame(
      x = c(0, 1, 0.8, 0.2),
      y = c(0.2, 0.2, 0, 0),
      group = "casco",
      fill = "#4B2D0B"
    ),
    data.frame(
      x = c(0, 1, 1, 0),
      y = c(0.2, 0.2, 0.25, 0.25),
      group = "barandilla",
      fill = "#331800"
    ),
    data.frame(
      x = c(0.475, 0.525, 0.525, 0.475),
      y = c(0.25, 0.25, 1, 1),
      group = "mastil",
      fill = "#331800"
    ),
    data.frame(
      x = c(0.6, 1, 0.525),
      y = c(0.25, 0.33, 1),
      group = "vela01",
      fill = "white"
    ),
    data.frame(
      x = c(0.4, 0, 0.475),
      y = c(0.25, 0.33, 1),
      group = "vela02",
      fill = "white"
    )
  )
  
  dataGrafica$poligonos$x <- (dataGrafica$poligonos$x * (x[2] - x[1])) + x[1]
  dataGrafica$poligonos$y <- (dataGrafica$poligonos$y * (y[2] - y[1])) + y[1]
  ##### Poligonos #####
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
      fill = dataGrafica$poligonos$fill
    )
  #### Gráfica ####
  
  return(grafica)
}