# Esta función devuelve el dibujo de un fórmula 1 que ocupa el espacio dado como
  # argumento en la función.
# Última modificación: 22 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # x (list): contiene la coordenada máxima y mínima sobre el eje x del
    # cuadrilatero que formará la base de la gráfica.
  # y (list): contiene la coordenada máxima y mínima sobre el eje y del
    # cuadrilatero que formará la base de la gráfica.
# Return: Una variable que contiene una gráfica ggplot.

geom_f1 <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Poligonos #####
  dataGrafica$poligonos <- rbind(
    data.frame(
      x = c(0, 0.1, 0.1, 0),
      y = c(0.45, 0.45, 0.57, 0.57),
      group = "02aleronTrasero",
      fill = "#233971"
    ),
    data.frame(
      x = c(0.08, 0.6, 0.6, 0.7, 0.95, 0.95, 0.7, 0.5, 0.5, 0.08),
      y = c(0.4, 0.4, 0.435, 0.435, 0.4, 0.43, 0.5, 0.5, 0.6, 0.515),
      group = "02chasis",
      fill = "#233971"
    ),
    data.frame(
      x = c(0.925, 1, 1, 0.925),
      y = c(0.4, 0.4, 0.43, 0.43),
      group = "02aleronDelantero",
      fill = "#233971"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))*0.04 + 0.52,
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))*0.04 + 0.51,
      group = "01casco",
      fill = "#D4AF37"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))*0.075 + 0.83,
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))*0.075 + 0.44,
      group = "04ruedaDelante",
      fill = "black"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))*0.0575 + 0.83,
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))*0.0575 + 0.44,
      group = "05llantaDelante",
      fill = "#323232"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))*0.0075 + 0.83,
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))*0.0075 + 0.44,
      group = "06tuercaDelante",
      fill = "#FFC906"
    ),
    data.frame(
      x = c(
        cos(x = seq(from = -pi, to = pi, length.out = 50))*0.06725 + 0.83,
        rev(cos(x = seq(from = -pi, to = pi, length.out = 50))*0.06525 + 0.83)
      ),
      y = c(
        sin(x = seq(from = -pi, to = pi, length.out = 50))*0.06725 + 0.44,
        rev(sin(x = seq(from = -pi, to = pi, length.out = 50))*0.06525 + 0.44)
      ),
      group = "06pirelliDelante",
      fill = "yellow"
    ),
    data.frame(
      x = c(
        cos(x = seq(from = -pi, to = pi, length.out = 50))*0.054 + 0.83,
        rev(cos(x = seq(from = -pi, to = pi, length.out = 50))*0.0525 + 0.83)
      ),
      y = c(
        sin(x = seq(from = -pi, to = pi, length.out = 50))*0.054 + 0.44,
        rev(sin(x = seq(from = -pi, to = pi, length.out = 50))*0.0525 + 0.44)
      ),
      group = "06detalleLlantaDelante",
      fill = "#CC1E4A"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))*0.08 + 0.1,
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))*0.08 + 0.44,
      group = "04ruedaDetras",
      fill = "black"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))*0.06 + 0.1,
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))*0.06 + 0.44,
      group = "05llantaDetras",
      fill = "#323232"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))*0.0075 + 0.1,
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))*0.0075 + 0.44,
      group = "06tuercaDetras",
      fill = "#FFC906"
    ),
    data.frame(
      x = c(
        cos(x = seq(from = -pi, to = pi, length.out = 50))*0.071 + 0.1,
        rev(cos(x = seq(from = -pi, to = pi, length.out = 50))*0.069 + 0.1)
      ),
      y = c(
        sin(x = seq(from = -pi, to = pi, length.out = 50))*0.071 + 0.44,
        rev(sin(x = seq(from = -pi, to = pi, length.out = 50))*0.069 + 0.44)
      ),
      group = "06pirelliDetras",
      fill = "yellow"
    ),
    data.frame(
      x = c(
        cos(x = seq(from = -pi, to = pi, length.out = 50))*0.0565 + 0.1,
        rev(cos(x = seq(from = -pi, to = pi, length.out = 50))*0.055 + 0.1)
      ),
      y = c(
        sin(x = seq(from = -pi, to = pi, length.out = 50))*0.0565 + 0.44,
        rev(sin(x = seq(from = -pi, to = pi, length.out = 50))*0.055 + 0.44)
      ),
      group = "06detalleLlantaDetras",
      fill = "#CC1E4A"
    ),
    data.frame(
      x = c(0.5, 0.7, 0.83, 0.83, 0.7, 0.5),
      y = c(0.47, 0.47, 0.44, 0.45, 0.48, 0.48),
      group = "03detalle01",
      fill = "#CC1E4A"
    ),
    data.frame(
      x = c(0.01, 0.09, 0.09, 0.01),
      y = c(0.525, 0.525, 0.545, 0.545),
      group = "03detalle02",
      fill = "#CC1E4A"
    ),
    data.frame(
      x = c(0.22, 0.35, 0.35, 0.22),
      y = c(0.48, 0.48, 0.52, 0.52),
      group = "03detalle03",
      fill = "#CC1E4A"
    ),
    data.frame(
      x = c(0.21, 0.5, 0.5),
      y = c(0.541, 0.541, 0.6),
      group = "03detalle04",
      fill = "#FFC906"
    ),
    data.frame(
      x = c(0.27, 0.49, 0.49),
      y = c(0.5475, 0.5475, 0.59),
      group = "03detalle05",
      fill = "#CC1E4A"
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