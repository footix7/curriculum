# Esta función es la que confecciona el área destinada al lugar de nacimiento y
  # el lugar de residencia. La información básica, también se muestra sombre un
  # mapa que contiene algunos de los lugares más icónicos de la ciudad de
  # Valencia -El Miguelete, La Ciudad de las Artes y las Ciencias, el puerto, 
  # ambos cauces del rio, etc.
# Última modificación: 23 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$lugar <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = c(0.21, 0.41 + 0.005, 0.41, 0.70, 0.23, 0.49),
    y = c(1, 1 - 0.005, 1, 1, 0.9, 0.9),
    label = c(
      "Soy de", "Burjassot", "Burjassot", "pero", "resido en", "Benicalap"
    ),
    fontface = c("plain", "bold", "bold", "plain", "plain", "bold"),
    col = c(
      colores$secundario, colores$secundario, colores$principal,
      colores$secundario, colores$secundario, colores$secundario
    )
  )
  
  dataGrafica$texto$x <- (dataGrafica$texto$x * (x[2] - x[1])) + x[1]
  dataGrafica$texto$y <- (dataGrafica$texto$y * (y[2] - y[1])) + y[1]
  ##### Texto #####
  
  ##### Mapa #####
  dataGrafica$mapa <- rbind(
    data.frame(
      x = c(0.08, 0.9,  0.9, 0.08, 0.08, 0.085, 0.085, 0.895, 0.895, 0.085),
      y = c(   0,   0, 0.75, 0.75,    0, 0.005, 0.745, 0.745, 0.005, 0.005),
      group = "03marco",
      fill = colores$secundario,
      alpha = 1
    ),
    data.frame(
      x = c(0.08, 0.74, 0.725, 0.72, 0.72, 0.725, 0.7325, 0.75, 0.08),
      y = c(0, 0, 0.2, 0.3, 0.35, 0.4, 0.5, 0.75, 0.75),
      group = "01tierra",
      fill = "#FBE7CC",
      alpha = 1
    ),
    data.frame(
      x = c(0.74, 0.725, 0.72, 0.72, 0.725, 0.7325, 0.75, 0.9, 0.9),
      y = c(0, 0.2, 0.3, 0.35, 0.4, 0.5, 0.75, 0.75, 0),
      group = "01mar",
      fill = "#CBE6EF",
      alpha = 1
    ),
    data.frame(
      x = c(0.08, 0.22,  0.4, 0.75,   0.75,   0.41,  0.22, 0.08),
      y = c( 0.4,  0.3, 0.1,  0.05,  0.075,  0.125, 0.325, 0.425),
      group = "02rio01",
      fill = "#CBE6EF",
      alpha = 1
    ),
    data.frame(
      x = c(0.22,  0.5, 0.725, 0.725,   0.5, 0.22),
      y = c( 0.3, 0.41,   0.2, 0.225, 0.435, 0.325),
      group = "02rio02",
      fill = "#BBF1BB",
      alpha = 1
    )
  )
  
  dataGrafica$mapa$x <- (dataGrafica$mapa$x * (x[2] - x[1])) + x[1]
  dataGrafica$mapa$y <- (dataGrafica$mapa$y * (y[2] - y[1])) + y[1]
  ##### Mapa #####
  
  ##### Puntos #####
  dataGrafica$puntos <- data.frame(
    x = c(0.2, 0.3),
    y = c(0.6, 0.5),
    col = c(colores$principal, colores$secundario)
  )
  
  dataGrafica$puntos$x <- (dataGrafica$puntos$x * (x[2] - x[1])) + x[1]
  dataGrafica$puntos$y <- (dataGrafica$puntos$y * (y[2] - y[1])) + y[1]
  ##### Puntos #####
  #### Adecuación de datos ####
  
  #### Gráfica ####
  grafica <- grafica +
    geom_text(
      data = dataGrafica$texto,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      family = "tipografia",
      size = 12,
      fontface = dataGrafica$texto$fontface,
      col = dataGrafica$texto$col,
      vjust = 1,
      hjust = 0
    ) +
    geom_polygon(
      data = dataGrafica$mapa,
      mapping = aes(
        x = x,
        y = y,
        group = group
      ),
      fill = dataGrafica$mapa$fill,
      alpha = dataGrafica$mapa$alpha
    ) +
    geom_point(
      data = dataGrafica$puntos,
      mapping = aes(
        x = x,
        y = y
      ),
      col = dataGrafica$puntos$col,
      size = 3.5
    ) %>%
      ggfx::with_shadow(
        sigma = 10
      )
  
  grafica <- grafica %>%
    geom_barco(
      x = (0.79 + c(0, 0.08)) * (x[2] - x[1]) + x[1],
      y = (0.25 + c(0, 0.1)) * (y[2] - y[1]) + y[1]
    ) %>%
    geom_barco(
      x = (0.76 + c(0, 0.08)) * (x[2] - x[1]) + x[1],
      y = (0.03 + c(0, 0.1)) * (y[2] - y[1]) + y[1]
    ) %>%
    geom_sombrilla(
      x = (0.66 + c(0, 0.08)) * (x[2] - x[1]) + x[1],
      y = (0.625 + c(0, 0.1)) * (y[2] - y[1]) + y[1]
    ) %>%
    geom_sombrilla(
      x = (0.65 + c(0, 0.08)) * (x[2] - x[1]) + x[1],
      y = (0.5625 + c(0, 0.1)) * (y[2] - y[1]) + y[1]
    ) %>%
    geom_arts(
      x = (0.54 + c(0, 0.175)) * (x[2] - x[1]) + x[1],
      y = (0.15 + c(0, 0.175)) * (y[2] - y[1]) + y[1]
    ) %>%
    geom_micalet(
      x = (0.34 + c(0, 0.175)) * (x[2] - x[1]) + x[1],
      y = (0.26 + c(0, 0.25)) * (y[2] - y[1]) + y[1]
    )
  #### Gráfica ####
  
  return(grafica)
} 