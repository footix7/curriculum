# Esta función devuelve el dibujo de una sombrilla que ocupa el espacio dado
  # como argumento en la función.
# Última modificación: 22 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # x (list): contiene la coordenada máxima y mínima sobre el eje x del
    # cuadrilatero que formará la base de la gráfica.
  # y (list): contiene la coordenada máxima y mínima sobre el eje y del
    # cuadrilatero que formará la base de la gráfica.
# Return: Una variable que contiene una gráfica ggplot.

geom_sombrilla <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Poligonos #####
  dataGrafica$poligonos <- rbind(
    data.frame(
      x = c(0, 0.6, 0.75),
      y = c(1, 0.7, 1),
      group = "02tela01",
      fill = "#C67400"
    ),
    data.frame(
      x = c(0, 0.6, 0.3),
      y = c(1, 0.7, 0.4),
      group = "02tela02",
      fill = "#ED8B00"
    ),
    data.frame(
      x = c(0, 0, 0.3),
      y = c(1, 0.25, 0.4),
      group = "02tela03",
      fill = "#C67400"
    ),
    data.frame(
      x = c(0.925,     1, 0.075,    0),
      y = c(    0, 0.075,     1, 0.925),
      group = "01palo",
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