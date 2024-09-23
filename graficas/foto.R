# Esta función es la que confecciona el área destinada al nombre.
# Última modificación: 12 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$foto <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Imagen #####
  dataGrafica$imagen <- data.frame(
    xmin = 0, xmax = 1, 
    ymin = -0.1, ymax = 0.9
  )
  
  dataGrafica$imagen$xmin <- (dataGrafica$imagen$xmin * (x[2] - x[1])) + x[1]
  dataGrafica$imagen$xmax <- (dataGrafica$imagen$xmax * (x[2] - x[1])) + x[1]
  dataGrafica$imagen$ymin <- (dataGrafica$imagen$ymin * (y[2] - y[1])) + y[1]
  dataGrafica$imagen$ymax <- (dataGrafica$imagen$ymax * (y[2] - y[1])) + y[1]
  ##### Imagen #####
  
  ##### Marco #####
  dataGrafica$marco <- rbind(
    data.frame(
      x = c(
        cos(x = seq(from = -pi, to = pi, length.out = 50))* (305/210)  *0.098 + (0.5 * (x[2] - x[1]) + x[1]),
        rev(cos(x = seq(from = -pi, to = pi, length.out = 50))* (305/210)  *0.088 + (0.5 * (x[2] - x[1]) + x[1]))
      ),
      y = c(
        sin(x = seq(from = -pi, to = pi, length.out = 50))*0.098 + (0.475 * (y[2] - y[1]) + y[1]),
        rev(sin(x = seq(from = -pi, to = pi, length.out = 50))*0.088 + (0.475 * (y[2] - y[1]) + y[1]))
      ),
      group = "marco01",
      fill = colores$detalles
    ),
    data.frame(
      x = c(
        cos(x = seq(from = -pi, to = pi, length.out = 100))* (305/210)  *0.09325 + (0.5 * (x[2] - x[1]) + x[1]),
        rev(cos(x = seq(from = -pi, to = pi, length.out = 100))* (305/210)  *0.09275 + (0.5 * (x[2] - x[1]) + x[1]))
      ),
      y = c(
        sin(x = seq(from = -pi, to = pi, length.out = 100))*0.09325 + (0.475 * (y[2] - y[1]) + y[1]),
        rev(sin(x = seq(from = -pi, to = pi, length.out = 100))*0.09275 + (0.475 * (y[2] - y[1]) + y[1]))
      ),
      group = "marco02",
      fill = "white"
    )
  )
  ##### Marco #####
  #### Adecuación de datos ####
  

  grafica <- grafica +
    annotation_custom(
      grob = rasterGrob(image = readPNG(source = "./foto/modificada.png")),
      xmin = dataGrafica$imagen$xmin,
      xmax = dataGrafica$imagen$xmax,
      ymin = dataGrafica$imagen$ymin,
      ymax = dataGrafica$imagen$ymax
    ) +
    geom_polygon(
      data = dataGrafica$marco,
      mapping = aes(
        x = x,
        y = y,
        group = group
      ),
      fill = dataGrafica$marco$fill
    )
  
  return(grafica)
}
