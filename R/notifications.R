

# ============================================================
# notifications.R - Sistema de notificaciones
# ============================================================

library(shinyalert)

# ============================================================
# NOTIFICACIONES EN PANTALLA
# ============================================================

#' Mostrar notificación de éxito
#' 
#' @param mensaje Mensaje a mostrar
#' @param duration Duración en segundos
mostrar_exito <- function(mensaje, duration = 5) {
  showNotification(
    HTML(paste("✅", mensaje)),
    type = "success",
    duration = duration,
    closeButton = TRUE
  )
}

#' Mostrar notificación de error
#' 
#' @param mensaje Mensaje a mostrar
#' @param duration Duración en segundos
mostrar_error <- function(mensaje, duration = 5) {
  showNotification(
    HTML(paste("❌", mensaje)),
    type = "error",
    duration = duration,
    closeButton = TRUE
  )
}

#' Mostrar notificación de advertencia
#' 
#' @param mensaje Mensaje a mostrar
#' @param duration Duración en segundos
mostrar_advertencia <- function(mensaje, duration = 5) {
  showNotification(
    HTML(paste("⚠️", mensaje)),
    type = "warning",
    duration = duration,
    closeButton = TRUE
  )
}

#' Mostrar alerta modal
#' 
#' @param title Título de la alerta
#' @param text Texto de la alerta
#' @param type Tipo (success, error, warning, info)
#' @param callbackR Función callback
mostrar_alerta <- function(title, text, type = "info", callbackR = NULL) {
  shinyalert::shinyalert(
    title = title,
    text = text,
    type = type,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    callbackR = callbackR
  )
}

#' Mostrar confirmación con opciones
#' 
#' @param title Título
#' @param text Texto
#' @param confirm_text Texto del botón confirmar
#' @param cancel_text Texto del botón cancelar
#' @param callbackR Función callback
mostrar_confirmacion <- function(title, text, confirm_text = "Confirmar", 
                                 cancel_text = "Cancelar", callbackR = NULL) {
  shinyalert::shinyalert(
    title = title,
    text = text,
    type = "question",
    showCancelButton = TRUE,
    confirmButtonText = confirm_text,
    cancelButtonText = cancel_text,
    callbackR = callbackR
  )
}

# ============================================================
# NOTIFICACIONES POR EMAIL (AUTOMATIZADAS)
# ============================================================

#' Enviar notificación de nueva cotización
#' 
#' @param datos Datos de la cotización
#' @param pool Pool de conexiones
enviar_notificacion_nueva_cotizacion <- function(datos, pool) {
  
  # Obtener emails de administradores
  admin_emails <- dbGetQuery(pool, "
    SELECT email FROM usuarios WHERE rol = 'admin'
  ")$email
  
  if (length(admin_emails) == 0) {
    return(FALSE)
  }
  
  # Enviar notificación
  enviar_notificacion_admin(datos, admin_emails)
}

#' Enviar recordatorio de cotizaciones pendientes
#' 
#' @param pool Pool de conexiones
#' @param dias Antigüedad en días
enviar_recordatorio_pendientes <- function(pool, dias = 2) {
  
  # Cotizaciones pendientes con más de X días
  pendientes <- dbGetQuery(pool, "
    SELECT * FROM cotizaciones 
    WHERE estado = 'pendiente' 
    AND fecha_creacion < datetime('now', ?)
  ", params = list(paste0("-", dias, " days")))
  
  if (nrow(pendientes) == 0) {
    return(FALSE)
  }
  
  admin_emails <- dbGetQuery(pool, "
    SELECT email FROM usuarios WHERE rol = 'admin'
  ")$email
  
  if (length(admin_emails) == 0) {
    return(FALSE)
  }
  
  # Crear resumen
  resumen <- pendientes %>%
    mutate(fecha = format(fecha_creacion, "%d/%m/%Y")) %>%
    select(ID = id, Nombre = nombre, Servicio = servicio, Fecha = fecha)
  
  md_body <- glue::glue("
    ## ⏰ Recordatorio de Cotizaciones Pendientes
    
    Existen {nrow(pendientes)} cotizaciones pendientes con más de {dias} días.
    
    ### 📋 Lista:
    
    {knitr::kable(resumen, format = 'pipe')}
    
    ---
    [📊 Ir al Dashboard](http://localhost:3838/dashboard)
  ")
  
  email <- compose_email(
    body = md_body,
    footer = md("
      ---
      Sistema de Recordatorios Automáticos
      Turismo Galaxy
    ")
  )
  
  # Enviar a todos los admins
  for (email_admin in admin_emails) {
    enviar_email(
      email = email,
      para = email_admin,
      asunto = glue("⏰ {nrow(pendientes)} cotizaciones pendientes - Turismo Galaxy")
    )
  }
  
  return(TRUE)
}

# ============================================================
# WEBHOOKS (para integración con servicios externos)
# ============================================================

#' Enviar notificación a Slack
#' 
#' @param mensaje Mensaje a enviar
#' @param webhook_url URL del webhook de Slack
enviar_slack <- function(mensaje, webhook_url = NULL) {
  
  if (is.null(webhook_url)) {
    webhook_url <- Sys.getenv("SLACK_WEBHOOK_URL")
  }
  
  if (webhook_url == "") {
    return(FALSE)
  }
  
  payload <- list(text = mensaje)
  
  tryCatch({
    httr::POST(
      url = webhook_url,
      body = payload,
      encode = "json"
    )
    return(TRUE)
  }, error = function(e) {
    cat("❌ Error al enviar a Slack:", e$message, "\n")
    return(FALSE)
  })
}

#' Enviar notificación a Telegram
#' 
#' @param mensaje Mensaje a enviar
#' @param bot_token Token del bot
#' @param chat_id ID del chat
enviar_telegram <- function(mensaje, bot_token = NULL, chat_id = NULL) {
  
  if (is.null(bot_token)) {
    bot_token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
  }
  
  if (is.null(chat_id)) {
    chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")
  }
  
  if (bot_token == "" || chat_id == "") {
    return(FALSE)
  }
  
  url <- glue("https://api.telegram.org/bot{bot_token}/sendMessage")
  
  tryCatch({
    httr::POST(
      url = url,
      body = list(
        chat_id = chat_id,
        text = mensaje,
        parse_mode = "HTML"
      ),
      encode = "form"
    )
    return(TRUE)
  }, error = function(e) {
    cat("❌ Error al enviar a Telegram:", e$message, "\n")
    return(FALSE)
  })
}