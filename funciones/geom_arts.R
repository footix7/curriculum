# Esta función devuelve el dibujo del Reina Sofia que ocupa el espacio dado como
  # argumento en la función.
# Última modificación: 22 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # x (list): contiene la coordenada máxima y mínima sobre el eje x del
    # cuadrilatero que formará la base de la gráfica.
  # y (list): contiene la coordenada máxima y mínima sobre el eje y del
    # cuadrilatero que formará la base de la gráfica.
# Return: Una variable que contiene una gráfica ggplot.

geom_arts <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Poligonos #####
  dataGrafica$poligonos <- rbind(
    data.frame(
      x = c(  0, 0.5,   1, 0.5,   0, 0.3,  0.5, 0.7,  0.5, 0.3),
      y = c(0.5, 0.8, 0.5, 0.2, 0.5, 0.5, 0.275, 0.5, 0.725, 0.5),
      group = "ojo",
      fill = "white"
    ),
    data.frame(
      x = c(0.1, 0.5, 0.9, 0.9, 0.5, 0.1),
      y = c(0.65, 0.825, 0.65, 0.675, 0.85, 0.675),
      group = "ceja",
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