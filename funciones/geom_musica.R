# Esta función devuelve el dibujo de una batería que ocupa el espacio dado como
  # argumento en la función.
# Última modificación: 12 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # x (list): contiene la coordenada máxima y mínima sobre el eje x del
    # cuadrilatero que formará la base de la gráfica.
  # y (list): contiene la coordenada máxima y mínima sobre el eje y del
    # cuadrilatero que formará la base de la gráfica.
# Return: Una variable que contiene una gráfica ggplot.

geom_musica <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Poligonos #####
  dataGrafica$poligonos <- rbind(
    data.frame(
      x = c(0.09, 0.11, 0.11, 0.09),
      y = c(0, 0, 0.975, 0.975),
      group = "basePlatilloIzquierda",
      fill = "black"
    ),
    data.frame(
      x = c(0.27, 0.23, 0, 0.04),
      y = c(0.8, 0.8, 1, 1),
      group = "platilloIzquierda",
      fill = "#B89E14"
    ),
    data.frame(
      x = c(0.91, 0.89, 0.89, 0.91),
      y = c(0, 0, 0.975, 0.975),
      group = "basePlatilloDerecha",
      fill = "black"
    ),
    data.frame(
      x = c(0.73, 0.77, 1, 0.96),
      y = c(0.8, 0.8, 1, 1),
      group = "platilloDerecha",
      fill = "#B89E14"
    ),
    data.frame(
      x = c(0.15, 0.425, 0.425, 0.15),
      y = c(0.375, 0.375, 0.7, 0.7),
      group = "cajaIzquierda",
      fill = "black"
    ),
    data.frame(
      x = c(0.235, 0.215, 0.215, 0.235),
      y = c(0, 0, 0.375, 0.375),
      group = "baseCajaIzquierda",
      fill = "black"
    ),
    data.frame(
      x = c(0.15, 0.425, 0.425, 0.15),
      y = c(0.425, 0.425, 0.65, 0.65),
      group = "cajaIzquierdaolor",
      fill = "darkred"
    ),
    data.frame(
      x = c(0.85, 0.575, 0.575, 0.85),
      y = c(0.375, 0.375, 0.7, 0.7),
      group = "cajaDerecha",
      fill = "black"
    ),
    data.frame(
      x = c(0.765, 0.785, 0.785, 0.765),
      y = c(0, 0, 0.375, 0.375),
      group = "baseCajaDerecha",
      fill = "black"
    ),
    data.frame(
      x = c(0.85, 0.575, 0.575, 0.85),
      y = c(0.425, 0.425, 0.65, 0.65),
      group = "cajaDerechaColor",
      fill = "darkred"
    ),
    data.frame(
      x = cos(seq(from = -pi, to = pi, length.out = 50))*0.25 + 0.5,
      y = sin(seq(from = -pi, to = pi, length.out = 50))*0.25 + 0.25,
      group = "bombo",
      fill = "black"
    ),
    data.frame(
      x = cos(seq(from = -pi, to = pi, length.out = 50))*0.21 + 0.5,
      y = sin(seq(from = -pi, to = pi, length.out = 50))*0.21 + 0.25,
      group = "bomboColor",
      fill = "darkred"
    ),
    data.frame(
      x = c(0.335, 0.315, 0.315, 0.335),
      y = c(0, 0, 0.1, 0.1),
      group = "baseBomboDerecha",
      fill = "black"
    ),
    data.frame(
      x = c(0.665, 0.685, 0.685, 0.665),
      y = c(0, 0, 0.1, 0.1),
      group = "baseBomboIzquierda",
      fill = "black"
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