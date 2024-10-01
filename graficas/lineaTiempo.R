# Esta función es la que confecciona el área destinada a la linea del tiempo que
  # muestra de una manera más visual tanto el historial académico como el
  # laboral.
# Última modificación: 30 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$lineaTiempo <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Años #####
  dataGrafica$años <- data.frame(
    x = seq(from = 0.075, to = 0.925, length.out = 2024-2018+1),
    y = 0,
    label = 2018:2024
  )
  
  dataGrafica$años$x <- (dataGrafica$años$x * (x[2] - x[1])) + x[1]
  dataGrafica$años$y <- (dataGrafica$años$y * (y[2] - y[1])) + y[1]
  ##### Años #####
  
  ##### Divisorias #####
  dataGrafica$divisorias <- data.frame(
    x = seq(from = 0.075 + (17/120/2), to = 0.925 - (17/120/2), by = 17/120),
    xend = seq(from = 0.075 + (17/120/2), to = 0.925 - (17/120/2), by = 17/120),
    y = 0.05,
    yend = 0.9
  )
  
  dataGrafica$divisorias$x <- (dataGrafica$divisorias$x * (x[2] - x[1])) + x[1]
  dataGrafica$divisorias$y <- (dataGrafica$divisorias$y * (y[2] - y[1])) + y[1]
  dataGrafica$divisorias$xend <-
    (dataGrafica$divisorias$xend * (x[2] - x[1])) + x[1]
  dataGrafica$divisorias$yend <-
    (dataGrafica$divisorias$yend * (y[2] - y[1])) + y[1]
  ##### Divisorias #####
  
  ##### Líneas #####
  dataGrafica$lineas <- data.frame(
    x = c(0.01, 0.01, 0.075, 0.075+(17/120)*3, 0.075+(17/120)*3.53),
    xend = c(
      0.075, 0.075+(17/120)*6.05, 0.075+(17/120)*4, 0.075+(17/120)*3.47,
      0.075+(17/120)*6.05
    ),
    y = c(0.9-0.14, 0.9-(0.14*2), 0.9-(0.14*3), 0.9-(0.14*4), 0.9-(0.14*5)),
    yend = c(0.9-0.14, 0.9-(0.14*2), 0.9-(0.14*3), 0.9-(0.14*4), 0.9-(0.14*5))
  )
  
  dataGrafica$lineas$x <- (dataGrafica$lineas$x * (x[2] - x[1])) + x[1]
  dataGrafica$lineas$y <- (dataGrafica$lineas$y * (y[2] - y[1])) + y[1]
  dataGrafica$lineas$xend <- (dataGrafica$lineas$xend * (x[2] - x[1])) + x[1]
  dataGrafica$lineas$yend <- (dataGrafica$lineas$yend * (y[2] - y[1])) + y[1]
  ##### Líneas #####
  
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = c(0.03, 0.03, 0.075, 0.075+(17/120)*3, 0.075+(17/120)*3.53),
    yend = c(0.9-0.14, 0.9-(0.14*2), 0.9-(0.14*3), 0.9-(0.14*4), 0.9-(0.14*5)),
    label = c(
      "Bachillerato Científico", "Clases particulares",
      "Grado en Ciencia de Datos", "Becario en Mercados",
      "Técnico en Fidelización"
    )
  )
  
  dataGrafica$texto$x <- (dataGrafica$texto$x * (x[2] - x[1])) + x[1]
  dataGrafica$texto$y <- (dataGrafica$texto$y * (y[2] - y[1])) + y[1]
  ##### Texto #####
  #### Adecuación de datos ####
  
  #### Gráfica ####
  grafica <- grafica +
    geom_text(
      data = dataGrafica$años,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      family = "tipografia",
      vjust = 0,
      size = 10,
      col = colores$secundario
    ) +
    geom_segment(
      data = dataGrafica$divisorias,
      mapping = aes(
        x = x, xend = xend,
        y = y, yend = yend
      ),
      linetype = "dashed",
      linewidth = 0.33,
      alpha = 0.4,
      col = colores$detalles
    ) +
    geom_segment(
      data = dataGrafica$lineas,
      mapping = aes(
        x = x, xend = xend,
        y = y - 1/900, yend = yend - 1/900
      ),
      col = colores$detalles,
      linewidth = 0.75
    ) %>%
      ggfx::with_shadow(
        sigma = 10,
        y_offset = 7.5
      ) +
    geom_segment(
      data = dataGrafica$lineas,
      mapping = aes(
        x = x, xend = xend,
        y = y, yend = yend
      ),
      col = colores$principal,
      linewidth = 0.75
    ) +
    geom_text(
      data = dataGrafica$texto,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      family = "tipografia",
      col = colores$secundario,
      size = 10,
      vjust = -0.5,
      hjust = -0.025
    )
  #### Gráfica ####
  
  return(grafica)
} 