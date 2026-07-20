

# ============================================================
# dashboard.R - Dashboard de estadísticas
# ============================================================

library(shinydashboard)
library(DT)
library(plotly)
library(lubridate)

# ============================================================
# UI DEL DASHBOARD
# ============================================================

dashboard_ui <- function() {
  
  dashboardPage(
    skin = "blue",
    
    # ===== HEADER =====
    dashboardHeader(
      title = tags$div(
        tags$img(
          src = "img/logo/Galaxy.png",
          height = "30px",
          style = "margin-right: 10px;"
        ),
        "Turismo Galaxy"
      ),
      dropdownMenu(
        type = "notifications",
        badgeStatus = "warning",
        icon = bs_icon("bell"),
        notificationItem(
          text = "Nuevas cotizaciones pendientes",
          icon = bs_icon("envelope"),
          status = "success"
        )
      )
    ),
    
    # ===== SIDEBAR =====
    dashboardSidebar(
      sidebarMenu(
        id = "dashboard_menu",
        menuItem(
          "Resumen",
          tabName = "resumen",
          icon = icon("chart-pie")
        ),
        menuItem(
          "Cotizaciones",
          tabName = "cotizaciones",
          icon = icon("list")
        ),
        menuItem(
          "Detalle",
          tabName = "detalle",
          icon = icon("search")
        ),
        menuItem(
          "Estadísticas",
          tabName = "estadisticas",
          icon = icon("chart-line")
        ),
        menuItem(
          "Logs",
          tabName = "logs",
          icon = icon("history")
        ),
        tags$hr(),
        menuItem(
          "Cerrar Sesión",
          tabName = "logout",
          icon = icon("sign-out"),
          selected = FALSE
        )
      )
    ),
    
    # ===== BODY =====
    dashboardBody(
      tags$head(
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "css/dashboard.css"
        )
      ),
      
      tabItems(
        # --- Tab: Resumen ---
        tabItem(
          tabName = "resumen",
          fluidRow(
            valueBoxOutput("total_cotizaciones"),
            valueBoxOutput("pendientes"),
            valueBoxOutput("hoy")
          ),
          fluidRow(
            box(
              title = "Cotizaciones por Mes",
              width = 12,
              plotlyOutput("grafico_mensual", height = "300px")
            )
          ),
          fluidRow(
            box(
              title = "Top Servicios",
              width = 6,
              plotlyOutput("top_servicios", height = "300px")
            ),
            box(
              title = "Estado de Cotizaciones",
              width = 6,
              plotlyOutput("estado_pie", height = "300px")
            )
          )
        ),
        
        # --- Tab: Cotizaciones ---
        tabItem(
          tabName = "cotizaciones",
          fluidRow(
            box(
              title = "Todas las Cotizaciones",
              width = 12,
              status = "primary",
              DTOutput("tabla_cotizaciones"),
              br(),
              div(
                style = "display: flex; gap: 10px;",
                actionButton("btn_actualizar", "🔄 Actualizar"),
                actionButton("btn_exportar", "📥 Exportar CSV")
              )
            )
          )
        ),
        
        # --- Tab: Detalle ---
        tabItem(
          tabName = "detalle",
          fluidRow(
            box(
              title = "Detalle de Cotización",
              width = 12,
              selectInput(
                "select_cotizacion",
                "Seleccionar cotización:",
                choices = NULL
              ),
              uiOutput("detalle_cotizacion"),
              br(),
              div(
                style = "display: flex; gap: 10px;",
                actionButton(
                  "btn_en_proceso",
                  "📌 Marcar en Proceso",
                  class = "btn-warning"
                ),
                actionButton(
                  "btn_completar",
                  "✅ Marcar Completado",
                  class = "btn-success"
                ),
                actionButton(
                  "btn_cancelar",
                  "❌ Cancelar",
                  class = "btn-danger"
                )
              )
            )
          )
        ),
        
        # --- Tab: Estadísticas ---
        tabItem(
          tabName = "estadisticas",
          fluidRow(
            box(
              title = "Estadísticas Avanzadas",
              width = 12,
              selectInput(
                "select_estadistica",
                "Tipo de estadística:",
                choices = c(
                  "Por Servicio" = "servicio",
                  "Por Mes" = "mes",
                  "Por Día" = "dia"
                )
              ),
              plotlyOutput("estadistica_avanzada", height = "500px")
            )
          )
        ),
        
        # --- Tab: Logs ---
        tabItem(
          tabName = "logs",
          fluidRow(
            box(
              title = "Registro de Actividades",
              width = 12,
              DTOutput("tabla_logs")
            )
          )
        ),
        
        # --- Tab: Logout ---
        tabItem(
          tabName = "logout",
          h3("¿Seguro que deseas cerrar sesión?"),
          br(),
          actionButton(
            "btn_logout",
            "Cerrar Sesión",
            class = "btn-danger btn-lg",
            icon = icon("sign-out")
          )
        )
      )
    )
  )
}

# ============================================================
# SERVER DEL DASHBOARD
# ============================================================

dashboard_server <- function(input, output, session, pool) {
  
  # ==========================================================
  # DATOS REACTIVOS
  # ==========================================================
  
  cotizaciones <- reactive({
    obtener_cotizaciones(pool)
  })
  
  estadisticas <- reactive({
    obtener_estadisticas(pool)
  })
  
  # ==========================================================
  # VALUE BOXES
  # ==========================================================
  
  output$total_cotizaciones <- renderValueBox({
    valueBox(
      value = estadisticas()$total,
      subtitle = "Total de Cotizaciones",
      icon = icon("file-invoice"),
      color = "blue"
    )
  })
  
  output$pendientes <- renderValueBox({
    pendientes <- estadisticas()$por_estado %>%
      filter(estado == "pendiente") %>%
      pull(total) %>%
      { if (length(.) == 0) 0 else . }
    
    valueBox(
      value = pendientes,
      subtitle = "Cotizaciones Pendientes",
      icon = icon("clock"),
      color = "yellow"
    )
  })
  
  output$hoy <- renderValueBox({
    valueBox(
      value = estadisticas()$hoy,
      subtitle = "Cotizaciones de Hoy",
      icon = icon("calendar-day"),
      color = "green"
    )
  })
  
  # ==========================================================
  # GRÁFICOS
  # ==========================================================
  
  output$grafico_mensual <- renderPlotly({
    datos <- estadisticas()$por_mes
    
    plot_ly(
      datos,
      x = ~mes,
      y = ~total,
      type = "bar",
      marker = list(color = "#163A5F"),
      text = ~total,
      textposition = "outside"
    ) %>%
      layout(
        xaxis = list(title = "Mes"),
        yaxis = list(title = "Cotizaciones"),
        showlegend = FALSE
      )
  })
  
  output$top_servicios <- renderPlotly({
    datos <- estadisticas()$top_servicios
    
    plot_ly(
      datos,
      y = ~servicio,
      x = ~total,
      type = "bar",
      orientation = "h",
      marker = list(color = "#00A651")
    ) %>%
      layout(
        xaxis = list(title = "Total"),
        yaxis = list(title = ""),
        showlegend = FALSE
      )
  })
  
  output$estado_pie <- renderPlotly({
    datos <- estadisticas()$por_estado
    
    colores <- c(
      "pendiente" = "#FFC107",
      "en_proceso" = "#17A2B8",
      "completado" = "#28A745",
      "cancelado" = "#DC3545"
    )
    
    plot_ly(
      datos,
      labels = ~estado,
      values = ~total,
      type = "pie",
      marker = list(colors = colores[datos$estado]),
      textinfo = "label+percent"
    ) %>%
      layout(showlegend = FALSE)
  })
  
  # ==========================================================
  # TABLA DE COTIZACIONES
  # ==========================================================
  
  output$tabla_cotizaciones <- renderDT({
    df <- cotizaciones() %>%
      select(
        ID = id,
        Nombre = nombre,
        Empresa = empresa,
        Teléfono = telefono,
        Correo = correo,
        Servicio = servicio,
        Estado = estado,
        Fecha = fecha_creacion
      )
    
    datatable(
      df,
      options = list(
        pageLength = 20,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      ),
      filter = "top",
      style = "bootstrap"
    ) %>%
      formatStyle(
        "Estado",
        backgroundColor = styleEqual(
          c("pendiente", "en_proceso", "completado", "cancelado"),
          c("#FFF3CD", "#D1ECF1", "#D4EDDA", "#F8D7DA")
        )
      )
  })
  
  # ==========================================================
  # DETALLE DE COTIZACIÓN
  # ==========================================================
  
  observe({
    df <- cotizaciones()
    updateSelectInput(
      session,
      "select_cotizacion",
      choices = setNames(df$id, paste(df$id, "-", df$nombre))
    )
  })
  
  output$detalle_cotizacion <- renderUI({
    req(input$select_cotizacion)
    
    cotizacion <- obtener_cotizacion_por_id(pool, input$select_cotizacion)
    
    if (nrow(cotizacion) == 0) {
      return(p("No se encontró la cotización"))
    }
    
    cot <- cotizacion[1, ]
    
    div(
      style = "padding: 20px; background: #f8f9fa; border-radius: 10px;",
      h4(glue("Cotización #{cot$id} - {cot$nombre}")),
      tags$hr(),
      tags$table(
        class = "table",
        tags$tr(tags$th("Nombre:"), tags$td(cot$nombre)),
        tags$tr(tags$th("Empresa:"), tags$td(cot$empresa %||% "No especificada")),
        tags$tr(tags$th("Teléfono:"), tags$td(cot$telefono %||% "No especificado")),
        tags$tr(tags$th("Correo:"), tags$td(cot$correo)),
        tags$tr(tags$th("Servicio:"), tags$td(cot$servicio)),
        tags$tr(tags$th("Estado:"), tags$td(cot$estado)),
        tags$tr(tags$th("Fecha:"), tags$td(format(cot$fecha_creacion, "%d/%m/%Y %H:%M"))),
        tags$tr(
          tags$th("Mensaje:"),
          tags$td(
            style = "white-space: pre-wrap;",
            cot$mensaje
          )
        )
      )
    )
  })
  
  # ==========================================================
  # ACCIONES SOBRE COTIZACIONES
  # ==========================================================
  
  observeEvent(input$btn_actualizar, {
    showNotification("✅ Cotizaciones actualizadas", type = "success")
  })
  
  observeEvent(input$btn_exportar, {
    df <- cotizaciones()
    write.csv(df, "exports/cotizaciones_export.csv", row.names = FALSE)
    showNotification("✅ Archivo exportado correctamente", type = "success")
  })
  
  observeEvent(input$btn_en_proceso, {
    req(input$select_cotizacion)
    actualizar_estado_cotizacion(pool, input$select_cotizacion, "en_proceso")
    showNotification("✅ Estado actualizado a 'En Proceso'", type = "success")
    registrar_log(pool, session$userData$user$id, "actualizar_estado", 
                  paste("Cotización", input$select_cotizacion, "-> en_proceso"))
  })
  
  observeEvent(input$btn_completar, {
    req(input$select_cotizacion)
    actualizar_estado_cotizacion(pool, input$select_cotizacion, "completado")
    showNotification("✅ Estado actualizado a 'Completado'", type = "success")
    registrar_log(pool, session$userData$user$id, "actualizar_estado",
                  paste("Cotización", input$select_cotizacion, "-> completado"))
  })
  
  observeEvent(input$btn_cancelar, {
    req(input$select_cotizacion)
    shinyalert::shinyalert(
      title = "¿Cancelar cotización?",
      text = "Esta acción no se puede deshacer",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Sí, cancelar",
      cancelButtonText = "No",
      callbackR = function(confirm) {
        if (confirm) {
          actualizar_estado_cotizacion(pool, input$select_cotizacion, "cancelado")
          showNotification("❌ Cotización cancelada", type = "warning")
          registrar_log(pool, session$userData$user$id, "actualizar_estado",
                        paste("Cotización", input$select_cotizacion, "-> cancelado"))
        }
      }
    )
  })
  
  # ==========================================================
  # ESTADÍSTICAS AVANZADAS
  # ==========================================================
  
  output$estadistica_avanzada <- renderPlotly({
    req(input$select_estadistica)
    df <- cotizaciones()
    
    if (input$select_estadistica == "servicio") {
      df %>%
        group_by(servicio) %>%
        summarise(total = n()) %>%
        plot_ly(
          x = ~servicio,
          y = ~total,
          type = "bar",
          marker = list(color = "#163A5F")
        ) %>%
        layout(
          xaxis = list(title = "Servicio"),
          yaxis = list(title = "Total")
        )
      
    } else if (input$select_estadistica == "mes") {
      df %>%
        mutate(mes = floor_date(fecha_creacion, "month")) %>%
        group_by(mes) %>%
        summarise(total = n()) %>%
        plot_ly(
          x = ~mes,
          y = ~total,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#00A651", width = 3),
          marker = list(color = "#163A5F", size = 8)
        ) %>%
        layout(
          xaxis = list(title = "Mes"),
          yaxis = list(title = "Total")
        )
      
    } else if (input$select_estadistica == "dia") {
      df %>%
        mutate(dia = floor_date(fecha_creacion, "day")) %>%
        group_by(dia) %>%
        summarise(total = n()) %>%
        plot_ly(
          x = ~dia,
          y = ~total,
          type = "bar",
          marker = list(color = "#17A2B8")
        ) %>%
        layout(
          xaxis = list(title = "Día"),
          yaxis = list(title = "Total")
        )
    }
  })
  
  # ==========================================================
  # LOGS
  # ==========================================================
  
  output$tabla_logs <- renderDT({
    df <- obtener_logs(pool)
    
    datatable(
      df %>%
        select(
          Fecha = fecha,
          Usuario = usuario,
          Acción = accion,
          Detalle = detalle
        ),
      options = list(
        pageLength = 50,
        autoWidth = TRUE,
        order = list(0, "desc")
      )
    )
  })
  
  # ==========================================================
  # LOGOUT
  # ==========================================================
  
  observeEvent(input$btn_logout, {
    cerrar_sesion(session)
  })
}

# ==========================================================
# CSS ADICIONAL PARA DASHBOARD
# ==========================================================

# Esto iría en www/css/dashboard.css