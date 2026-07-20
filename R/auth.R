

# ============================================================
# auth.R - Sistema de autenticación
# ============================================================

library(shinyauthr)
library(bcrypt)

# ============================================================
# FUNCIONES DE AUTENTICACIÓN
# ============================================================

#' Verificar credenciales de usuario
#' 
#' @param usuario Nombre de usuario
#' @param password Contraseña
#' @param pool Pool de conexiones
#' @return Lista con información del usuario o FALSE
verificar_credenciales <- function(usuario, password, pool) {
  
  # Buscar usuario
  query <- "SELECT * FROM usuarios WHERE usuario = ? AND activo = 1"
  user_data <- dbGetQuery(pool, query, params = list(usuario))
  
  if (nrow(user_data) == 0) {
    return(FALSE)
  }
  
  # Verificar contraseña (usando bcrypt)
  if (bcrypt::checkpw(password, user_data$password_hash)) {
    return(list(
      id = user_data$id,
      usuario = user_data$usuario,
      nombre = user_data$nombre,
      email = user_data$email,
      rol = user_data$rol
    ))
  }
  
  return(FALSE)
}

#' Obtener información del usuario logueado
#' 
#' @param session Sesión de Shiny
#' @return Lista con información del usuario o NULL
usuario_actual <- function(session) {
  if (!is.null(session$userData$user)) {
    return(session$userData$user)
  }
  return(NULL)
}

#' Verificar si el usuario es administrador
#' 
#' @param session Sesión de Shiny
#' @return TRUE si es administrador
es_admin <- function(session) {
  user <- usuario_actual(session)
  if (!is.null(user) && user$rol == "admin") {
    return(TRUE)
  }
  return(FALSE)
}

#' Cerrar sesión
#' 
#' @param session Sesión de Shiny
cerrar_sesion <- function(session) {
  session$userData$user <- NULL
  session$userData$logged_in <- FALSE
  session$reload()
}

# ============================================================
# UI DE LOGIN
# ============================================================

#' UI del módulo de login
#' 
#' @return UI elementos de login
login_ui <- function() {
  div(
    id = "login-page",
    style = "
      min-height: 100vh;
      display: flex;
      align-items: center;
      justify-content: center;
      background: linear-gradient(135deg, #163A5F, #0B1F36);
    ",
    div(
      style = "
        background: white;
        padding: 50px;
        border-radius: 25px;
        box-shadow: 0 25px 50px rgba(0,0,0,0.3);
        width: 100%;
        max-width: 420px;
      ",
      div(
        style = "text-align: center; margin-bottom: 30px;",
        img(
          src = "img/logo/Galaxy.png",
          height = "80px",
          alt = "Turismo Galaxy"
        ),
        h3(
          style = "color: #163A5F; font-weight: 700; margin-top: 15px;",
          "Panel de Administración"
        )
      ),
      
      loginUI(
        id = "auth",
        title = "",
        login_title = "Iniciar Sesión",
        login_icon = bs_icon("person-fill"),
        user_placeholder = "Usuario",
        pwd_placeholder = "Contraseña",
        cookie_expiry = 60 * 60 * 8  # 8 horas
      )
    )
  )
}