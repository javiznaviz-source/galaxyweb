
# ============================================================
# helpers.R - Funciones auxiliares
# ============================================================

#' Validar formulario de contacto
#' 
#' @param nombre Nombre completo
#' @param correo Correo electrónico
#' @param telefono Teléfono
#' @param mensaje Mensaje del usuario
#' @return Lista con estado y mensaje de error
validar_formulario <- function(nombre, correo, telefono, mensaje) {
  errores <- c()
  
  # Validar nombre
  if (is.null(nombre) || nchar(trimws(nombre)) < 3) {
    errores <- c(errores, "El nombre debe tener al menos 3 caracteres")
  }
  
  # Validar correo
  if (is.null(correo) || !grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", correo)) {
    errores <- c(errores, "Ingresa un correo electrónico válido")
  }
  
  # Validar teléfono (opcional pero con formato)
  if (!is.null(telefono) && nchar(telefono) > 0) {
    if (!grepl("^[0-9]{10,15}$", gsub("[^0-9]", "", telefono))) {
      errores <- c(errores, "El teléfono debe tener entre 10 y 15 dígitos")
    }
  }
  
  # Validar mensaje
  if (is.null(mensaje) || nchar(trimws(mensaje)) < 10) {
    errores <- c(errores, "El mensaje debe tener al menos 10 caracteres")
  }
  
  # Retornar resultado
  if (length(errores) > 0) {
    return(list(
      valido = FALSE,
      errores = errores
    ))
  } else {
    return(list(
      valido = TRUE,
      errores = NULL
    ))
  }
}

#' Generar mensaje de confirmación para cotización
#' 
#' @param nombre Nombre del cliente
#' @param servicio Servicio solicitado
#' @return Mensaje HTML formateado
mensaje_confirmacion <- function(nombre, servicio) {
  HTML(paste0(
    "✅ <strong>¡Cotización enviada correctamente!</strong><br><br>",
    "Hola <strong>", nombre, "</strong>,<br><br>",
    "Hemos recibido tu solicitud para el servicio de <strong>", servicio, "</strong>.<br>",
    "Uno de nuestros asesores se pondrá en contacto contigo en las próximas 24 horas.<br><br>",
    "📞 <strong>¿Necesitas atención inmediata?</strong><br>",
    "Contáctanos al WhatsApp: <strong>(464) 111-1111</strong>"
  ))
}

#' Obtener el ID de una pestaña por su nombre
#' 
#' @param nombre Nombre de la pestaña
#' @return ID de la pestaña
obtener_id_pestana <- function(nombre) {
  # Mapeo de nombres a IDs
  mapeo <- list(
    "inicio" = "Inicio",
    "nosotros" = " Nosotros",
    "servicios" = " Servicios",
    "flotilla" = " Flotilla",
    "contacto" = " Filiales y Contacto",
    "galeria" = " Galería",
    "testimonios" = " Testimonios",
    "clientes" = " Nuestros Clientes",
    "porque" = " ¿Por qué elegirnos?",
    "contratar" = " ¿Cómo contratar?",
    "faq" = " Preguntas Frecuentes",
    "compromiso" = " Nuestro Compromiso",
    "cotiza" = " Cotiza Ahora"
  )
  
  return(mapeo[[nombre]])
}