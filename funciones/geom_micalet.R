# Esta función devuelve el dibujo de El Micalet que ocupa el espacio dado como
  # argumento en la función.
# Última modificación: 22 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # x (list): contiene la coordenada máxima y mínima sobre el eje x del
    # cuadrilatero que formará la base de la gráfica.
  # y (list): contiene la coordenada máxima y mínima sobre el eje y del
    # cuadrilatero que formará la base de la gráfica.
# Return: Una variable que contiene una gráfica ggplot.

geom_micalet <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Poligonos #####
  dataGrafica$poligonos <- rbind(
    data.frame(
      x = c(0.475, 0.525, 0.525, 0.475),
      y = c(0.925, 0.925, 1, 1),
      group = "01campanario01",
      fill = "#DFAD81"
    ),
    data.frame(
      x = c(0.49, 0.51, 0.51, 0.49),
      y = c(0.925, 0.925, 0.97, 0.97),
      group = "02detalleCampanario01",
      fill = "#1B0000"
    ),
    data.frame(
      x = c(0.45, 0.55, 0.55, 0.45),
      y = c(0.8, 0.8, 0.925, 0.925),
      group = "01campanario02",
      fill = "#DFAD81"
    ),
    data.frame(
      x = c(0.48, 0.52, 0.52, 0.48),
      y = c(0.8, 0.8, 0.88, 0.88),
      group = "02detalleCampanario02",
      fill = "#1B0000"
    ),
    data.frame(
      x = c(0.29, 0.29, 0.39, 0.39),
      y = c(0.725, 0.025,    0, 0.8),
      group = "01lateral01",
      fill = "#D59359"
    ),
    data.frame(
      x = c(  0.33,   0.33, 0.35, 0.35),
      y = c(0.6825, 0.5525, 0.55,  0.7),
      group = "02detalleLateral01",
      fill = "#1B0000"
    ),
    data.frame(
      x = c(0.39, 0.61, 0.61, 0.39),
      y = c(   0,    0,  0.8,  0.8),
      group = "01frontal",
      fill = "#DAA06D"
    ),
    data.frame(
      x = c(0.465, 0.535, 0.535, 0.465),
      y = c(0.55, 0.55,  0.7,  0.7),
      group = "02detalleFrontal",
      fill = "#1B0000"
    ),
    data.frame(
      x = c(0.61, 0.61,  0.71, 0.71),
      y = c(   0,  0.8, 0.725, 0.025),
      group = "01lateral02",
      fill = "#DFAD81"
    ),
    data.frame(
      x = c(0.65, 0.65, 0.67, 0.67),
      y = c( 0.55,  0.7, 0.6825, 0.5525),
      group = "02detalleLateral02",
      fill = "#1B0000"
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