# Esta función es la que confecciona el área destinada a las habilidades.
# Última modificación: 30 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$habilidades <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Título #####
  dataGrafica$titulo <- data.frame(
    x = 0.03,
    y = 0.95,
    label = "Habilidades"
  )
  
  dataGrafica$titulo$x <- (dataGrafica$titulo$x * (x[2] - x[1])) + x[1]
  dataGrafica$titulo$y <- (dataGrafica$titulo$y * (y[2] - y[1])) + y[1]
  ##### Título #####
  
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = 0.05,
    y = c(0.825, 0.45, 0.1),
    label = c(
      "Profundos conocimientos de\nR, Python y SQL.",
      "Manejo con soltura HTML\ny Javascript.",
      "Vasto dominio de la ofimática."
    )
  )
  
  dataGrafica$texto$x <- (dataGrafica$texto$x * (x[2] - x[1])) + x[1]
  dataGrafica$texto$y <- (dataGrafica$texto$y * (y[2] - y[1])) + y[1]
  ##### Texto #####
  #### Adecuación de datos ####
  
  #### Gráfica ####
  grafica <- grafica + 
    geom_text(
      data = dataGrafica$titulo,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      size = 12,
      fontface = "bold",
      family = "tipografia",
      hjust = 0,
      col = colores$secundario
    ) + 
    geom_text(
      data = dataGrafica$texto,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      size = 12,
      hjust = 0,
      vjust = 1,
      family = "tipografia",
      col = colores$secundario,
      lineheight = 1/3
    )
  #### Gráfica ####
  
  return(grafica)
} 