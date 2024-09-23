# Esta función es la que confecciona el área destinada al nombre.
# Última modificación: 12 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$edad <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = c(0.5, 0.65),
    y = c(0.8, 0.5125),
    label = c("17 de noviembre del 2000", "Nov. 2000")
  )
  
  dataGrafica$texto$x <- (dataGrafica$texto$x * (x[2] - x[1])) + x[1]
  dataGrafica$texto$y <- (dataGrafica$texto$y * (y[2] - y[1])) + y[1]
  ##### Texto #####
  
  ##### Calendario #####
  dataGrafica$calendario <- rbind(
    data.frame(
      x = c(0.45, 0.95, 0.95, 0.45),
      y = c(0.45, 0.45, 0.575, 0.575),
      group = "cabecera"
    ),
    data.frame(
      x = c(0.45, 0.95, 0.95, 0.45),
      y = c(0.15, 0.15, 0.45, 0.45),
      group = "dias"
    )
  )
  
  dataGrafica$calendario$x <- (dataGrafica$calendario$x * (x[2] - x[1])) + x[1]
  dataGrafica$calendario$y <- (dataGrafica$calendario$y * (y[2] - y[1])) + y[1]
  ##### Calendario #####
  
  ##### Puntos #####
  dataGrafica$puntos <- rbind(
    data.frame(# Primera semana
      x = seq(from = 1/14/2+(1/14)*2+0.45, by = 1/14, length.out = 5),
      y = 0.45-0.03
    ),
    data.frame(
      x = rep(seq(from = 1/14/2+0.45, by = 1/14, length.out = 7), times = 3),
      y = rep(seq(from = 0.45-0.03-0.06, by = -0.06, length.out = 3), each = 7)
    ),
    data.frame(# Última semana
      x = seq(from = 1/14/2+0.45, by = 1/14, length.out = 4),
      y = 0.45-0.03-(0.06*4)
    )
  ) %>%
    dplyr::mutate(
      col = c(
        rep(x = colores$secundario, times = 16), colores$principal,
        rep(x = colores$secundario, times = 13)
      )
    )
  
  dataGrafica$puntos$x <- (dataGrafica$puntos$x * (x[2] - x[1])) + x[1]
  dataGrafica$puntos$y <- (dataGrafica$puntos$y * (y[2] - y[1])) + y[1]
  ##### Puntos #####
  #### Adecuación de datos ####
  
  #### Gráfica ####
  grafica <- grafica +
    geom_polygon(
      data = dataGrafica$calendario,
      mapping = aes(
        x = x,
        y = y,
        group = group
      ),
      fill = NA,
      col = colores$secundario
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
      size = 14
    ) +
    geom_point(
      data = dataGrafica$punto,
      mapping = aes(
        x = x,
        y = y
      ),
      size = 1.5,
      col = dataGrafica$punto$col
    )
  #### Gráfica ####
  
  return(grafica)
} 