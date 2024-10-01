# Esta función es la que confecciona el área destinada al acceso al código que 
  # permite elaborar este currículum. Se trata de un enlace a github.com que es
  # accesible tanto a través de la lectura de un código QR como del propio
  # enlace web.
# Última modificación: 1 de octubre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$acceso <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  #### Segmento ####
  dataGrafica$segmento <- data.frame(
    x = 0.125, xend = 0.875,
    y = 0.9, yend = 0.9
  )
  
  dataGrafica$segmento$x <- (dataGrafica$segmento$x * (x[2] - x[1])) + x[1]
  dataGrafica$segmento$y <- (dataGrafica$segmento$y * (y[2] - y[1])) + y[1]
  dataGrafica$segmento$xend <-
    (dataGrafica$segmento$xend * (x[2] - x[1])) + x[1]
  dataGrafica$segmento$yend <-
    (dataGrafica$segmento$yend * (y[2] - y[1])) + y[1]
  #### Segmento ####
  
  ##### Imagen #####
  dataGrafica$imagen <- data.frame(
    xmin = 0, xmax = 1, 
    ymin = 0.3, ymax = 0.85
  )
  
  dataGrafica$imagen$xmin <- (dataGrafica$imagen$xmin * (x[2] - x[1])) + x[1]
  dataGrafica$imagen$xmax <- (dataGrafica$imagen$xmax * (x[2] - x[1])) + x[1]
  dataGrafica$imagen$ymin <- (dataGrafica$imagen$ymin * (y[2] - y[1])) + y[1]
  dataGrafica$imagen$ymax <- (dataGrafica$imagen$ymax * (y[2] - y[1])) + y[1]
  ##### Imagen #####
  
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = 0.5,
    y = 0.075,
    label = paste0(
      "Para ver el código que permite\ngenerar este currículum acceder a:\ngit",
      "hub.com/footix7/curriculum" 
    )
  )
  
  dataGrafica$texto$x <- (dataGrafica$texto$x * (x[2] - x[1])) + x[1]
  dataGrafica$texto$y <- (dataGrafica$texto$y * (y[2] - y[1])) + y[1]
  ##### Texto #####
  #### Adecuación de datos ####
  
  #### Gráfica ####
  grafica <- grafica +
    annotation_custom(
      grob = rasterGrob(image = readPNG(source = "./imagenes/qr.png")),
      xmin = dataGrafica$imagen$xmin,
      xmax = dataGrafica$imagen$xmax,
      ymin = dataGrafica$imagen$ymin,
      ymax = dataGrafica$imagen$ymax
    )  +
    geom_segment(
      data = dataGrafica$segmento,
      mapping = aes(
        x = x, xend = xend,
        y = y, yend = yend
      ),
      col = colores$detalles
    ) + 
    geom_text(
      data = dataGrafica$texto,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      vjust = 0,
      size = 10,
      family = "tipografia",
      col = colores$secundario,
      lineheight = 1/3
    )
  #### Gráfica ####
  
  return(grafica)
} 