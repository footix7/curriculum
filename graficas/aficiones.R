# Esta función es la que confecciona el área destinada a las aficiones. Cada
  # afición se representa con un círculo y su tamaño muestra el grada de
  # afinidad.
# Última modificación: 1 de octubre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$aficiones <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  
  ##### Título #####
  dataGrafica$titulo <- data.frame(
    x = 0.03,
    y = 0.95,
    label = "Aficiones"
  )
  
  dataGrafica$titulo$x <- (dataGrafica$titulo$x * (x[2] - x[1])) + x[1]
  dataGrafica$titulo$y <- (dataGrafica$titulo$y * (y[2] - y[1])) + y[1]
  ##### Título #####
  
  ##### Circulos Sombras #####
  dataGrafica$circulosSombras <- rbind(
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /28 + (0.72 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /28 + (0.85 * (y[2] - y[1]) + y[1]),
      group = "formula 1"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /37 + (0.48 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /37 + (0.675 * (y[2] - y[1]) + y[1]),
      group = "cine"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /17.6 + (0.2 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /17.6 + (0.43 * (y[2] - y[1]) + y[1]),
      group = "futbol"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /35 + (0.72 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /35 + (0.52 * (y[2] - y[1]) + y[1]),
      group = "cocinar"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /35 + (0.9 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /35 + (0.30 * (y[2] - y[1]) + y[1]),
      group = "musica"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /25 + (0.58 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /25 + (0.10 * (y[2] - y[1]) + y[1]),
      group = "escribir"
    )
  )
  ##### Circulos Sombras #####
  
  ##### Circulos #####
  dataGrafica$circulos <- rbind(
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /32 + (0.72 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /32 + (0.85 * (y[2] - y[1]) + y[1]),
      group = "formula 1"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /45 + (0.48 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /45 + (0.675 * (y[2] - y[1]) + y[1]),
      group = "cine"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /20 + (0.2 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /20 + (0.43 * (y[2] - y[1]) + y[1]),
      group = "futbol"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /42 + (0.72 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /42 + (0.52 * (y[2] - y[1]) + y[1]),
      group = "cocinar"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /44 + (0.9 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /44 + (0.30 * (y[2] - y[1]) + y[1]),
      group = "musica"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /29 + (0.58 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /29 + (0.10 * (y[2] - y[1]) + y[1]),
      group = "escribir"
    )
  )
  ##### Circulos #####
  
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = c(0.2, 0.48, 0.58, 0.72, 0.9, 0.72),
    y = c(0.43, 0.675, 0.10, 0.85, 0.30, 0.52),
    label = c("Fútbol", "Cine", "Escribir", "Fórmula 1", "Música", "Cocinar"),
    size = c(12, 9, 10, 9, 9, 9)
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
    geom_polygon(
      data = dataGrafica$circulosSombras,
      mapping = aes(
        x = x,
        y = y,
        group = group
      ),
      fill = colores$detalles
    ) %>%
      ggfx::with_shadow(
        sigma = 20
      ) +
    geom_polygon(
      data = dataGrafica$circulos,
      mapping = aes(
        x = x,
        y = y,
        group = group
      ),
      fill = colores$principal
    ) +
    geom_text(
      data = dataGrafica$texto,
      mapping = aes(
        x = x,
        y = y,
        label = label
      ),
      fontface = "bold",
      family = "tipografia",
      col = colores$secundario,
      size = dataGrafica$texto$size
    )
  #### Gráfica ####
  
  return(grafica)
} 