# app.R

# Cargar configuraciones globales
source("global.R")

# Cargar interfaz de usuario
ui <- source("ui.R")$value

# Cargar la lógica del servidor
source("server.R")

# Ejecutar la aplicación
shinyApp(ui, server)
