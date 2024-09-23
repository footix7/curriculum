# Una función que devuelve una gráfica base, es decir, vacía y con un
  # cuadrilatero, que por defecto será un cuadrado de dimenesión 1.
# Última modificación: 12 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # x (list): contiene la coordenada máxima y mínima sobre el eje x del
    # cuadrilatero que formará la base de la gráfica.
  # y (list): contiene la coordenada máxima y mínima sobre el eje y del
    # cuadrilatero que formará la base de la gráfica.
# Return: Una variable que contiene una gráfica ggplot.

geom_base <- function(grafica = ggplot(), x = c(0, 1), y = c(0, 1)){
  grafica <- grafica +
    theme_void() +
    geom_polygon(
      mapping = aes(
        x = c(x[1], x[2], x[2], x[1]),
        y = c(y[1], y[1], y[2], y[2])
      ),
      fill = NA
    )
  
  return(grafica)
}
