# Este script obtiene una imagen de tamaño A4. La imagen contiene un currículum
  # elaborado gracias a la unión de varias gráficas, programadas en R con un uso
  # mayoritario de la librería ggplot.
# Última modificación: 23 de septiembre de 2024
# Autor: Valero Vilar, Ignacio
# Requiere:
# Obtiene: Un archivo .png
# Cabe: Ejecutar este script y solo este script para obtener la imagen.

#### Carga de librerias ####
librerias <- c(
  "ggplot2", "magrittr", "ggfx", "sysfonts", "showtext", "png", "grid"
)
instalados <- .packages(all.available = TRUE)

for (libreria in librerias){
  if (!(libreria %in% instalados)){
    install.packages(libreria)
  }
  library(package = libreria, character.only = TRUE)
}
#### Carga de librerias ####

#### Directorio de trabajo #####
# Fija como directorio de trabajo el mismo en el que está alojado el script en
  # ejecución.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#### Directorio de trabajo #####

#### Carga de funciones ####
ruta <- "./funciones"
ficheros <- list.files(path = ruta, full.names = TRUE)

for (fichero in ficheros){
  source(file = fichero)
}
#### Carga de funciones ####

#### Variables globales ####
graficas <- list()

colores <- list(
  principal = "#ED8B00",
  secundario = "#323232",
  detalles = "#A25E2A"
)

cargaTipografia(tipografia = "Nunito")
#### Variables globales ####

#### Carga de gráficas ####
ruta <- "./graficas"
ficheros <- list.files(path = ruta, full.names = TRUE)

for (fichero in ficheros){
  source(file = fichero)
}
#### Carga de gráficas ####

#### Gráfica ####
geom_base() %>%
  graficas$fondo(
    x = c(0, 0.7),
    y = c(0, 1)
  ) %>%
  graficas$panel(
    x = c(0.7, 1),
    y = c(0, 1)
  ) %>%
  graficas$nombre(
    x = c(0, 0.7),
    y = c(0.9, 1)
  ) %>%
  graficas$foto(
    x = c(0.7, 1),
    y = c(0.75, 1)
  ) %>%
  graficas$edad(
    x = c(0, 0.35),
    y = c(0.7, 0.9)
  ) %>%
  graficas$lugar(
    x = c(0.35, 0.7),
    y = c(0.7, 0.9)
  ) %>%
  graficas$estudios(
    x = c(0, 0.35),
    y = c(0.55, 0.7)
  ) %>%
  graficas$trabajo(
    x = c(0.35, 0.7),
    y = c(0.55, 0.7)
  ) %>%
  graficas$lineaTiempo(
    x = c(0, 0.7),
    y = c(0.375, 0.55)
  ) %>%
  graficas$aficiones(
    x = c(0, 0.7),
    y = c(0.2, 0.375)
  ) %>%
  graficas$creatividad(
    x = c(0, 7/30),
    y = c(0, 0.2)
  ) %>%
  graficas$repDatos(
    x = c(7/30, 14/30),
    y = c(0, 0.2)
  ) %>%
  graficas$hablarPublico(
    x = c(14/30, 0.7),
    y = c(0, 0.2)
  ) %>%
  graficas$mensaje(
    x = c(0.7, 1),
    y = c(0, 0.125)
  )
#### Gráfica ####

#### Guardado de gráfica ####
ggsave(
  filename = "curriculum.png",
  width = 210,
  height = 297,
  units = "mm",
  bg = "white"
)

# ggsave(
#   filename = "../../../media/sf_Maquina/curriculum.png",
#   width = 210,
#   height = 297,
#   units = "mm",
#   bg = "white"
# )
#### Guardado de gráfica ####