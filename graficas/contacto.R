# Esta función es la que confecciona el área destinada al contacto en donde
  # aparece número de teléfono y correo electrónico.
# Última modificación: 23 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # grafica (ggplot): Objeto sobre el cual la función va añadir capas.
  # x (list): contiene la coordenada mínima y máxima sobre el x del área
    # destinada dentro de la imagen.
  # y (list): contiene la coordenada mínima y máxima sobre el y del área
    # destinada dentro de la imagen.

graficas$contacto <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  
  #### Adecuación de datos ####
  dataGrafica <- list()
  ##### Título #####
  dataGrafica$titulo <- data.frame(
    x = 0.03,
    y = c(1, 0.5),
    label = c("Número de teléfono", "Correo (@gmail.com)")
  )
  
  dataGrafica$titulo$x <- (dataGrafica$titulo$x * (x[2] - x[1])) + x[1]
  dataGrafica$titulo$y <- (dataGrafica$titulo$y * (y[2] - y[1])) + y[1]
  ##### Título #####
  
  ##### Texto #####
  dataGrafica$texto <- data.frame(
    x = 0.5,
    y = c(0.75, 0.25),
    label = c("600 343 831", "nacho.valero.mavalencia")
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
      family = "tipografia",
      col = colores$secundario
    )
  #### Gráfica ####
  
  return(grafica)
} 