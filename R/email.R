

# ============================================================
# email.R - Envío de emails con blastula
# ============================================================

library(blastula)
library(glue)

# ============================================================
# CONFIGURACIÓN DE EMAIL
# ============================================================

#' Configurar credenciales de SMTP
#' 
#' @return Lista con configuración SMTP
configurar_smtp <- function() {
  
  # En producción, usar variables de entorno
  # Sys.setenv(SMTP_HOST = "smtp.gmail.com")
  # Sys.setenv(SMTP_PORT = "587")
  # Sys.setenv(SMTP_USER = "tuemail@gmail.com")
  # Sys.setenv(SMTP_PASSWORD = "tucontraseña")
  
  smtp <- list(
    host = Sys.getenv("SMTP_HOST", "smtp.gmail.com"),
    port = as.numeric(Sys.getenv("SMTP_PORT", "587")),
    user = Sys.getenv("SMTP_USER", "contacto@turismogalaxy.com"),
    password = Sys.getenv("SMTP_PASSWORD", "")
  )
  
  return(smtp)
}

#' Crear credenciales SMTP para blastula
#' 
#' @return Objeto credenciales
crear_credenciales <- function() {
  smtp <- configurar_smtp()
  
  creds <- creds(
    host = smtp$host,
    port = smtp$port,
    user = smtp$user,
    password = smtp$password
  )
  
  return(creds)
}

# ============================================================
# PLANTILLAS DE EMAIL
# ============================================================

#' Plantilla de email de confirmación para cliente
#' 
#' @param nombre Nombre del cliente
#' @param servicio Servicio solicitado
#' @param id ID de la cotización
#' @return Email como objeto blastula
plantilla_confirmacion_cliente <- function(nombre, servicio, id) {
  
  # Cargar logo como imagen para el email
  # logo_path <- "www/img/logo/Galaxy.png"
  # logo_img <- add_image(logo_path, width = 150)
  
  md_body <- glue::glue("
    ## Hola {nombre} 👋
    
    ¡Gracias por contactar a **Turismo Galaxy**!
    
    Hemos recibido tu solicitud de cotización para el servicio de **{servicio}**.
    
    **Número de folio:** #{id}
    
    ### 📋 Próximos pasos:
    
    1. Nuestro equipo revisará tu solicitud en las próximas **24 horas**
    2. Recibirás una propuesta personalizada por este mismo correo
    3. Podrás resolver cualquier duda por teléfono o WhatsApp
    
    ### 📞 ¿Necesitas atención inmediata?
    
    - **Teléfono:** (464) 000-0000
    - **WhatsApp:** (464) 111-1111
    
    ---
    
    *Este es un mensaje automático. Por favor no respondas a este correo.*
    
    **Turismo Galaxy** - Viaja seguro y con confianza
  ")
  
  email <- compose_email(
    body = md_body,
    footer = md("
      ---
      **Turismo Galaxy** &bull; Salamanca, Guanajuato
      &bull; [Sitio web](https://turismogalaxy.com)
    ")
  )
  
  return(email)
}

#' Plantilla de email para administradores (nueva cotización)
#' 
#' @param datos Lista con datos de la cotización
#' @return Email como objeto blastula
plantilla_notificacion_admin <- function(datos) {
  
  md_body <- glue::glue("
    ## 🔔 Nueva Cotización Recibida
    
    Se ha recibido una nueva cotización en Turismo Galaxy.
    
    ### 📋 Detalles:
    
    | Campo | Valor |
    |-------|-------|
    | **Folio** | #{datos$id} |
    | **Nombre** | {datos$nombre} |
    | **Empresa** | {datos$empresa %||% 'No especificada'} |
    | **Teléfono** | {datos$telefono %||% 'No especificado'} |
    | **Correo** | {datos$correo} |
    | **Servicio** | {datos$servicio} |
    | **Fecha** | {format(Sys.time(), '%d/%m/%Y %H:%M')} |
    
    ### 📝 Mensaje:
    
    > {datos$mensaje}
    
    ---
    
    [📊 Ver en el Dashboard](http://localhost:3838/dashboard)
  ")
  
  email <- compose_email(
    body = md_body,
    footer = md("
      ---
      Sistema de Cotizaciones Turismo Galaxy
    ")
  )
  
  return(email)
}

#' Plantilla de seguimiento para cliente
#' 
#' @param nombre Nombre del cliente
#' @param id ID de la cotización
#' @param estado Nuevo estado
#' @return Email como objeto blastula
plantilla_actualizacion_estado <- function(nombre, id, estado) {
  
  mensajes_estado <- list(
    "en_proceso" = "📌 Tu cotización está siendo revisada por nuestro equipo",
    "completado" = "✅ Tu cotización ha sido completada. ¡Revisa tu propuesta!",
    "cancelado" = "⏸️ Tu cotización ha sido cancelada. ¿Necesitas ayuda?"
  )
  
  md_body <- glue::glue("
    ## Hola {nombre} 👋
    
    {mensajes_estado[[estado]]}
    
    **Folio:** #{id}
    **Estado:** {estado}
    
    ---
    
    **Turismo Galaxy** - Viaja seguro y con confianza
  ")
  
  email <- compose_email(
    body = md_body,
    footer = md("
      ---
      **Turismo Galaxy** &bull; Salamanca, Guanajuato
    ")
  )
  
  return(email)
}

# ============================================================
# FUNCIÓN PARA ENVIAR EMAILS
# ============================================================

#' Enviar email usando blastula
#' 
#' @param email Objeto email de blastula
#' @param para Dirección de correo del destinatario
#' @param asunto Asunto del email
#' @param cc Direcciones en copia (opcional)
#' @return TRUE si se envió correctamente
enviar_email <- function(email, para, asunto, cc = NULL) {
  
  tryCatch({
    # Crear credenciales
    creds <- crear_credenciales()
    
    # Enviar email
    smtp_send(
      email = email,
      to = para,
      from = Sys.getenv("SMTP_USER", "contacto@turismogalaxy.com"),
      subject = asunto,
      credentials = creds,
      cc = cc
    )
    
    cat("✅ Email enviado a:", para, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error al enviar email:", e$message, "\n")
    return(FALSE)
  })
}

#' Enviar confirmación al cliente
#' 
#' @param datos Lista con datos de la cotización
#' @return TRUE si se envió correctamente
enviar_confirmacion_cliente <- function(datos) {
  email <- plantilla_confirmacion_cliente(
    datos$nombre,
    datos$servicio,
    datos$id
  )
  
  enviar_email(
    email = email,
    para = datos$correo,
    asunto = paste("Confirmación de cotización #", datos$id, " - Turismo Galaxy")
  )
}

#' Enviar notificación a administradores
#' 
#' @param datos Lista con datos de la cotización
#' @param admin_emails Vector de emails de administradores
#' @return TRUE si se envió correctamente
enviar_notificacion_admin <- function(datos, admin_emails) {
  email <- plantilla_notificacion_admin(datos)
  
  enviar_email(
    email = email,
    para = admin_emails[1],
    cc = admin_emails[-1],
    asunto = paste("🔔 Nueva cotización #", datos$id, " - Turismo Galaxy")
  )
}