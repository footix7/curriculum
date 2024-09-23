# Esta función es la que confecciona el área destinada al nombre.
# Última modificación: 12 de septiembre de 2024
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
  ##### Circulos Sombras #####
  dataGrafica$circulosSombras <- rbind(
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /20 + (0.2 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /20 + (0.43 * (y[2] - y[1]) + y[1]),
      group = "futbol"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /42 + (0.44 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /42 + (0.61 * (y[2] - y[1]) + y[1]),
      group = "cine"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /40 + (0.58 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /40 + (0.24 * (y[2] - y[1]) + y[1]),
      group = "escribir"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /29 + (0.72 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /29 + (0.65 * (y[2] - y[1]) + y[1]),
      group = "formula 1"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /34 + (0.90 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /34 + (0.39 * (y[2] - y[1]) + y[1]),
      group = "musica"
    )
  )
  ##### Circulos Sombras #####
  
  ##### Circulos #####
  dataGrafica$circulos <- rbind(
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /22.5 + (0.2 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /22.5 + (0.43 * (y[2] - y[1]) + y[1]),
      group = "futbol"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /51 + (0.44 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /51 + (0.61 * (y[2] - y[1]) + y[1]),
      group = "cine"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /49 + (0.58 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /49 + (0.24 * (y[2] - y[1]) + y[1]),
      group = "escribir"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /34 + (0.72 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /34 + (0.65 * (y[2] - y[1]) + y[1]),
      group = "formula 1"
    ),
    data.frame(
      x = cos(x = seq(from = -pi, to = pi, length.out = 50))
      * (297/210) /40 + (0.90 * (x[2] - x[1]) + x[1]),
      y = sin(x = seq(from = -pi, to = pi, length.out = 50))
      /40 + (0.39 * (y[2] - y[1]) + y[1]),
      group = "musica"
    )
  )
  ##### Circulos #####
  
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = c(0.2, 0.44, 0.58, 0.72, 0.9),
    y = c(0.53, 0.65, 0.27, 0.7, 0.43),
    label = c("Fútbol", "Cine", "Música", "Fórmula 1", "Escribir"),
    size = c(14, 9, 8, 9, 9)
  )
  
  
  dataGrafica$texto$x <- (dataGrafica$texto$x * (x[2] - x[1])) + x[1]
  dataGrafica$texto$y <- (dataGrafica$texto$y * (y[2] - y[1])) + y[1]
  ##### Texto #####
  #### Adecuación de datos ####
  
  #### Gráfica ####
  grafica <- grafica +
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
  
  grafica <- grafica %>%
    geom_musica(
      x = (0.50 + c(0, 0.08)) * (x[2] - x[1]) + x[1],
      y = (0.05 + c(0, 0.20)) * (y[2] - y[1]) + y[1]
    ) %>%
    geom_f1(
      x = (0.69 + c(0, 0.14)) * (x[2] - x[1]) + x[1],
      y = (0.36 + c(0, 0.40)) * (y[2] - y[1]) + y[1]
    )
  #### Gráfica ####
  
  return(grafica)
} 