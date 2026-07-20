

# ============================================================
# database.R - Gestión de base de datos
# ============================================================

library(DBI)
library(RSQLite)
library(pool)
library(lubridate)
library(dplyr)

# ============================================================
# CONEXIÓN A LA BASE DE DATOS
# ============================================================

#' Crear pool de conexiones a SQLite
#' 
#' @return Objeto pool de conexiones
crear_pool_db <- function() {
  pool <- dbPool(
    drv = RSQLite::SQLite(),
    dbname = "data/turismo_galaxy.db",
    create = TRUE
  )
  return(pool)
}

#' Inicializar la base de datos (crear tablas si no existen)
#' 
#' @param pool Objeto pool de conexiones
inicializar_db <- function(pool) {
  
  # Crear tabla de cotizaciones
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS cotizaciones (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      nombre TEXT NOT NULL,
      empresa TEXT,
      telefono TEXT,
      correo TEXT NOT NULL,
      servicio TEXT NOT NULL,
      mensaje TEXT NOT NULL,
      estado TEXT DEFAULT 'pendiente',
      fecha_creacion DATETIME DEFAULT CURRENT_TIMESTAMP,
      fecha_actualizacion DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Crear tabla de usuarios (para autenticación)
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS usuarios (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      usuario TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      nombre TEXT NOT NULL,
      email TEXT NOT NULL,
      rol TEXT DEFAULT 'admin',
      activo INTEGER DEFAULT 1,
      fecha_creacion DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Crear tabla de logs
  dbExecute(pool, "
    CREATE TABLE IF NOT EXISTS logs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      usuario_id INTEGER,
      accion TEXT NOT NULL,
      detalle TEXT,
      ip TEXT,
      fecha DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (usuario_id) REFERENCES usuarios(id)
    )
  ")
  
  # Verificar si existe el usuario admin, si no crearlo
  admin_exists <- dbGetQuery(pool, "
    SELECT COUNT(*) as total FROM usuarios WHERE usuario = 'admin'
  ")
  
  if (admin_exists$total == 0) {
    # Contraseña por defecto: admin123 (en producción usar hash real)
    library(bcrypt)
    hash <- bcrypt::hashpw("admin123")
    
    dbExecute(pool, "
      INSERT INTO usuarios (usuario, password_hash, nombre, email, rol)
      VALUES ('admin', ?, 'Administrador', 'admin@turismogalaxy.com', 'admin')
    ", params = list(hash))
    
    cat("✅ Usuario admin creado con contraseña 'admin123'\n")
  }
  
  cat("✅ Base de datos inicializada correctamente\n")
}

# ============================================================
# FUNCIONES CRUD PARA COTIZACIONES
# ============================================================

#' Guardar una nueva cotización
#' 
#' @param pool Pool de conexiones
#' @param datos Lista con los datos del formulario
#' @return ID de la cotización creada
guardar_cotizacion <- function(pool, datos) {
  
  query <- "
    INSERT INTO cotizaciones 
    (nombre, empresa, telefono, correo, servicio, mensaje, estado)
    VALUES (?, ?, ?, ?, ?, ?, 'pendiente')
  "
  
  params <- list(
    datos$nombre,
    datos$empresa %||% "",
    datos$telefono %||% "",
    datos$correo,
    datos$servicio,
    datos$mensaje
  )
  
  dbExecute(pool, query, params = params)
  
  # Obtener el ID de la última inserción
  id <- dbGetQuery(pool, "SELECT last_insert_rowid() as id")$id
  
  return(id)
}

#' Obtener todas las cotizaciones
#' 
#' @param pool Pool de conexiones
#' @param estado Filtrar por estado (opcional)
#' @param limite Número máximo de registros (opcional)
#' @return Dataframe con las cotizaciones
obtener_cotizaciones <- function(pool, estado = NULL, limite = NULL) {
  
  query <- "SELECT * FROM cotizaciones"
  params <- list()
  
  if (!is.null(estado)) {
    query <- paste(query, "WHERE estado = ?")
    params <- list(estado)
  }
  
  query <- paste(query, "ORDER BY fecha_creacion DESC")
  
  if (!is.null(limite)) {
    query <- paste(query, "LIMIT ?")
    params <- c(params, limite)
  }
  
  df <- dbGetQuery(pool, query, params = params)
  
  # Formatear fechas
  if (nrow(df) > 0) {
    df$fecha_creacion <- ymd_hms(df$fecha_creacion)
    df$fecha_actualizacion <- ymd_hms(df$fecha_actualizacion)
  }
  
  return(df)
}

#' Obtener una cotización por ID
#' 
#' @param pool Pool de conexiones
#' @param id ID de la cotización
#' @return Dataframe con la cotización
obtener_cotizacion_por_id <- function(pool, id) {
  query <- "SELECT * FROM cotizaciones WHERE id = ?"
  df <- dbGetQuery(pool, query, params = list(id))
  
  if (nrow(df) > 0) {
    df$fecha_creacion <- ymd_hms(df$fecha_creacion)
    df$fecha_actualizacion <- ymd_hms(df$fecha_actualizacion)
  }
  
  return(df)
}

#' Actualizar el estado de una cotización
#' 
#' @param pool Pool de conexiones
#' @param id ID de la cotización
#' @param estado Nuevo estado (pendiente, en_proceso, completado, cancelado)
#' @return TRUE si se actualizó correctamente
actualizar_estado_cotizacion <- function(pool, id, estado) {
  
  estados_validos <- c("pendiente", "en_proceso", "completado", "cancelado")
  
  if (!estado %in% estados_validos) {
    stop("Estado no válido")
  }
  
  query <- "
    UPDATE cotizaciones 
    SET estado = ?, fecha_actualizacion = CURRENT_TIMESTAMP
    WHERE id = ?
  "
  
  dbExecute(pool, query, params = list(estado, id))
  
  return(TRUE)
}

#' Obtener estadísticas de cotizaciones
#' 
#' @param pool Pool de conexiones
#' @return Lista con estadísticas
obtener_estadisticas <- function(pool) {
  
  # Total de cotizaciones
  total <- dbGetQuery(pool, "SELECT COUNT(*) as total FROM cotizaciones")$total
  
  # Por estado
  por_estado <- dbGetQuery(pool, "
    SELECT estado, COUNT(*) as total 
    FROM cotizaciones 
    GROUP BY estado
  ")
  
  # Por mes (últimos 12 meses)
  por_mes <- dbGetQuery(pool, "
    SELECT 
      strftime('%Y-%m', fecha_creacion) as mes,
      COUNT(*) as total
    FROM cotizaciones
    WHERE fecha_creacion >= datetime('now', '-12 months')
    GROUP BY mes
    ORDER BY mes DESC
  ")
  
  # Cotizaciones del día
  hoy <- dbGetQuery(pool, "
    SELECT COUNT(*) as total 
    FROM cotizaciones 
    WHERE DATE(fecha_creacion) = DATE('now')
  ")$total
  
  # Servicios más solicitados
  top_servicios <- dbGetQuery(pool, "
    SELECT servicio, COUNT(*) as total
    FROM cotizaciones
    GROUP BY servicio
    ORDER BY total DESC
    LIMIT 5
  ")
  
  return(list(
    total = total,
    por_estado = por_estado,
    por_mes = por_mes,
    hoy = hoy,
    top_servicios = top_servicios
  ))
}

# ============================================================
# FUNCIONES DE LOG
# ============================================================

#' Registrar una acción en el log
#' 
#' @param pool Pool de conexiones
#' @param usuario_id ID del usuario
#' @param accion Acción realizada
#' @param detalle Detalle de la acción
#' @param ip Dirección IP
registrar_log <- function(pool, usuario_id, accion, detalle = NULL, ip = NULL) {
  
  query <- "
    INSERT INTO logs (usuario_id, accion, detalle, ip)
    VALUES (?, ?, ?, ?)
  "
  
  dbExecute(pool, query, params = list(usuario_id, accion, detalle, ip))
}

#' Obtener logs recientes
#' 
#' @param pool Pool de conexiones
#' @param limite Número máximo de logs
#' @return Dataframe con los logs
obtener_logs <- function(pool, limite = 100) {
  
  query <- "
    SELECT 
      l.*,
      u.usuario,
      u.nombre as usuario_nombre
    FROM logs l
    LEFT JOIN usuarios u ON l.usuario_id = u.id
    ORDER BY l.fecha DESC
    LIMIT ?
  "
  
  df <- dbGetQuery(pool, query, params = list(limite))
  
  if (nrow(df) > 0) {
    df$fecha <- ymd_hms(df$fecha)
  }
  
  return(df)
}

# ============================================================
# OPERADOR %||% (NULL coalescing)
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}