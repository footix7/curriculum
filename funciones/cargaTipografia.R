# Una función que carga la tipografía que se desea usar en una determinada
  # gráfica de la librería ggplot2. Por defecto, y para una mayor generalización
  # se usará el apelativo «tipografía» para emplear la tipografía seleccionado
  # por el usuario, no obstante, se reserva la posibilidad de que el propioç
  # usuario modifique este apelativo.
# Última modificación: 22 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Argumentos:
  # tipografia (caracter): nombre de la tipografía alojada en el repositorio
    # «fonts.google.com/». Atención, si la tipografía es demasiado nueva, corre
    # el riesgo de que no esté accesible desde R.
  # nombre (caracter): apelativo que se usará dentro de la gráfica para
    # referirse a la tipografía cargada. Por defecto, este apelativo será
    # «tipografia».
# Return:

cargaTipografia <- function(tipografia, nombre = "tipografia"){
  font_add_google(tipografia, nombre)
  showtext_auto()
  return()
}
