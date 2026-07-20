

# ============================================================
# global.R - Carga de paquetes y configuración global
# ============================================================
# Lista de paquetes requeridos
paquetes <- c(
  "shiny",
  "bslib",
  "bsicons",
  "leaflet",
  "shinyjs",
  "htmltools",
  "fontawesome",
  # NUEVOS PAQUETES
  "DBI",                    # Interfaz de base de datos
  "RSQLite",                # Base de datos SQLite
  "pool",                   # Pool de conexiones
  "blastula",               # Emails con R
  "shinydashboard",         # Dashboard
  "shinyauthr",             # Autenticación
  "shinyalert",             # Alertas bonitas
  "DT",                     # Tablas interactivas
  "plotly",                 # Gráficos interactivos
  "lubridate",              # Manejo de fechas
  "dplyr",                  # Manipulación de datos
  "glue"                    # String interpolation
)

# ... resto del código igual ...

# Función para verificar e instalar paquetes
verificar_paquetes <- function(pkgs) {
  instalados <- rownames(installed.packages())
  faltantes <- pkgs[!(pkgs %in% instalados)]
  
  if (length(faltantes) > 0) {
    cat("\n⚠️  Instalando paquetes faltantes:\n")
    cat(paste("  -", faltantes), sep = "\n")
    install.packages(faltantes, dependencies = TRUE)
    cat("\n✅ Instalación completada.\n")
  } else {
    cat("\n✅ Todos los paquetes ya están instalados.\n")
  }
}

# Verificar e instalar
verificar_paquetes(paquetes)

# Cargar todos los paquetes
invisible(lapply(paquetes, library, character.only = TRUE))

# Mensaje de confirmación
cat("\n✅ Paquetes cargados correctamente.\n")
cat("🚀 Turismo Galaxy App lista para ejecutarse.\n\n")

# Tema principal
theme_galaxy <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#163A5F",
  secondary = "#0B1F36",
  success = "#00A651",
  base_font = font_google("Poppins")
)
