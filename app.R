# ============================================================
# app.R - Turismo Galaxy con LOGO CENTRADO (VERSIÓN CORREGIDA)
# ============================================================

install.packages("shiny"),

source("global.R")

# Cargar paquetes
library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(htmltools)
library(fontawesome)
library(DT)
library(plotly)
library(lubridate)
library(dplyr)
library(DBI)
library(RSQLite)

# ============================================================
# UI - Con logo centrado
# ============================================================

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
    # CSS para centrar el navbar y logo
    tags$style(HTML("
  /* Estilos para el navbar centrado */
  .navbar {
    padding-top: 8px !important;
    padding-bottom: 8px !important;
    background: #163A5F !important;
    border: none !important;
  }
  
  .navbar .container-fluid {
    display: flex !important;
    flex-direction: column !important;
    align-items: center !important;
    justify-content: center !important;
    padding-top: 5px !important;
  }
  
  .navbar-header {
    width: 100% !important;
    display: flex !important;
    justify-content: center !important;
    margin-bottom: 5px !important;
  }
  
  .navbar-brand {
    display: flex !important;
    flex-direction: column !important;
    align-items: center !important;
    justify-content: center !important;
    padding: 0 !important;
    margin: 0 !important;
    float: none !important;
    height: auto !important;
  }
  
  .navbar-brand img {
    display: block !important;
    margin: 0 auto !important;
    height: 50px !important;
  }
  
  .navbar-brand .logo-text {
    text-align: center !important;
    color: white !important;
    margin-top: 2px !important;
  }
  
  .navbar-brand .logo-text strong {
    font-size: 18px !important;
    display: block !important;
    letter-spacing: 1px !important;
    color: white !important;
  }
  
  .navbar-brand .logo-text .subtitle {
    font-size: 11px !important;
    opacity: 0.85 !important;
    display: block !important;
    margin-top: -2px !important;
    color: white !important;
  }
  
  /* ==== MENÚ CENTRADO - TEXTO BLANCO ==== */
  .navbar-nav {
    display: flex !important;
    justify-content: center !important;
    width: 100% !important;
    gap: 5px !important;
    flex-wrap: wrap !important;
    margin-top: 5px !important;
  }
  
  /* TEXTO BLANCO para todos los items del menú */
  .navbar-nav .nav-link,
  .navbar-nav > li > a,
  .navbar-default .navbar-nav > li > a {
    font-size: 14px !important;
    padding: 6px 15px !important;
    color: white !important;
    background: transparent !important;
  }
  
  /* Hover: color verde */
  .navbar-nav .nav-link:hover,
  .navbar-nav > li > a:hover,
  .navbar-default .navbar-nav > li > a:hover {
    color: #00A651 !important;
    background: transparent !important;
  }
  
  /* Activo: fondo sutil */
  .navbar-nav > .active > a,
  .navbar-nav > .active > a:hover,
  .navbar-nav > .active > a:focus {
    color: white !important;
    background-color: rgba(255,255,255,0.1) !important;
  }
  
  /* Botón hamburguesa en móvil */
  .navbar-toggle {
    position: absolute !important;
    right: 15px !important;
    top: 50% !important;
    transform: translateY(-50%) !important;
    border-color: white !important;
  }
  
  .navbar-toggle .icon-bar {
    background-color: white !important;
  }
  
  /* En móvil, menú vertical */
  @media (max-width: 991px) {
    .navbar-nav {
      flex-direction: column !important;
      align-items: center !important;
      margin-top: 10px !important;
    }
    .navbar-nav .nav-link,
    .navbar-nav > li > a {
      text-align: center !important;
      width: 100% !important;
      padding: 10px 15px !important;
      color: white !important;
    }
    .navbar-collapse {
      width: 100% !important;
      background: #163A5F !important;
    }
  }
")),
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("Turismo Galaxy - Transporte seguro y confiable")
  ),
  
  # ============================================================
  # NAVBAR CON LOGO CENTRADO
  # ============================================================
  navbarPage(
    # El título ahora contiene el logo y nombre centrados
    title = div(
      class = "navbar-brand",
      img(
        src = "img/logo/Galaxy.png",
        alt = "Logo Turismo Galaxy"
      ),
      div(
        class = "logo-text",
        tags$strong("TURISMO GALAXY"),
        span(class = "subtitle", "Viaja seguro y con confianza")
      )
    ),
    
    # ==================== PESTAÑA INICIO ====================
    tabPanel(
      "Inicio",
      div(
        class = "hero",
        div(
          class = "hero-content",
          div(class = "hero-subtitle", "TRANSPORTE TURÍSTICO • EJECUTIVO • EMPRESARIAL"),
          h1(class = "hero-title", HTML("Viaja Seguro<br>y con Confianza")),
          p(class = "hero-text", 
            HTML("Más de <strong>25 años</strong> brindando transporte turístico,
                 empresarial, escolar y ejecutivo con una moderna flotilla.")),
          div(
            class = "hero-buttons",
            actionButton("cotizar", "Solicitar cotización", class = "btn-galaxy")
          )
        ),
        div(class = "scroll-down", HTML("&#8595;"))
      )
    ),
    
    # ==================== PESTAÑA NOSOTROS (VERSIÓN COMPLETA) ====================
    tabPanel(
      "Nosotros",
      div(
        class = "section",
        
        # --- QUIÉNES SOMOS ---
        h2(class = "titulo", "¿Quiénes somos?"),
        layout_columns(
          col_widths = c(5, 7),
          card(
            img(
              src = "img/nosotros/nosotros.jpg",
              style = "width:100%; border-radius:15px; box-shadow: 0 8px 25px rgba(0,0,0,0.1);",
              alt = "Equipo Turismo Galaxy"
            )
          ),
          div(
            h3(style = "color: #163A5F; font-weight: 700;", "Turismo Galaxy"),
            p(
              style = "font-size: 16px; line-height: 1.8; color: #555;",
              "Somos una empresa 100% mexicana con más de 25 años de experiencia 
          en el sector del transporte terrestre. Nos especializamos en brindar 
          servicios turísticos, ejecutivos, escolares y empresariales con los 
          más altos estándares de calidad y seguridad."
            ),
            tags$ul(
              style = "list-style: none; padding: 0;",
              tags$li(style = "padding: 5px 0;", "🚀 Más de 25 años de experiencia"),
              tags$li(style = "padding: 5px 0;", "👨‍✈️ Operadores altamente capacitados"),
              tags$li(style = "padding: 5px 0;", "🚌 Flotilla moderna y en óptimas condiciones"),
              tags$li(style = "padding: 5px 0;", "📍 Cobertura nacional"),
              tags$li(style = "padding: 5px 0;", "🏆 Certificados en seguridad vial")
            ),
            br(),
            actionButton("servicios_nosotros", "Conoce nuestros servicios", class = "btn-galaxy")
          )
        ),
        
        br(), br(),
        tags$hr(style = "border-top: 2px solid #00A651; width: 80%; margin: 40px auto;"),
        br(),
        
        # --- MISIÓN, VISIÓN, FILOSOFÍA ---
        h2(class = "titulo", "Nuestra Identidad"),
        p(
          style = "text-align: center; font-size: 18px; max-width: 800px; margin: -30px auto 50px auto; color: #666;",
          "Conoce lo que nos impulsa a ser mejores cada día"
        ),
        
        layout_columns(
          col_widths = c(4, 4, 4),
          
          # MISIÓN
          card(
            class = "card-galaxy",
            style = "text-align: center; padding: 30px 25px; height: 100%;",
            div(style = "font-size: 50px; margin-bottom: 15px;", "🎯"),
            h4(style = "color: #163A5F; font-weight: 700;", "Misión"),
            p(
              style = "text-align: justify; line-height: 1.8; color: #555; font-size: 15px;",
              "Ofrecer servicios de transporte turístico seguros, confiables y de alta calidad, 
              proporcionando a nuestros clientes una experiencia de viaje cómoda y puntual, 
              mediante un servicio personalizado, unidades modernas y un equipo comprometido con 
              la excelencia y la satisfacción de cada usuario."
            )
          ),
          
          # VISIÓN
          card(
            class = "card-galaxy",
            style = "text-align: center; padding: 30px 25px; height: 100%;",
            div(style = "font-size: 50px; margin-bottom: 15px;", "🔭"),
            h4(style = "color: #163A5F; font-weight: 700;", "Visión"),
            p(
              style = "text-align: justify; line-height: 1.8; color: #555; font-size: 15px;",
              "Consolidarnos como la empresa de transporte turístico más reconocida y confiable de México, 
              distinguiéndonos por la calidad de nuestros servicios, la innovación, la seguridad y la excelencia 
              operativa. Aspiramos a ser la primera opción de nuestros clientes, generando experiencias de viaje 
              memorables y contribuyendo al desarrollo del turismo mediante un servicio responsable, competitivo y sustentable."
            )
          ),
          
          # FILOSOFÍA
          card(
            class = "card-galaxy",
            style = "text-align: center; padding: 30px 25px; height: 100%;",
            div(style = "font-size: 50px; margin-bottom: 15px;", "💎"),
            h4(style = "color: #163A5F; font-weight: 700;", "Filosofía"),
            p(
              style = "text-align: justify; line-height: 1.8; color: #555; font-size: 15px;",
              "Creemos que un servicio de transporte de excelencia se construye sobre la confianza, 
              la seguridad, la puntualidad y el respeto hacia nuestros clientes. Nuestra filosofía es 
              brindar una atención personalizada, con profesionalismo y compromiso, generando experiencias 
              de viaje cómodas, confiables y relaciones duraderas basadas en la calidad del servicio."
            )
          )
        ),
        
        br(), br(),
        
           # ===== VALORES =====
    h2(class = "titulo", "Nuestros Valores"),
    p(
      style = "text-align: center; font-size: 18px; max-width: 800px; margin: -20px auto 50px auto; color: #666;",
      "Ocho principios que guían cada uno de nuestros viajes"
    ),
    
    # Primera fila (4 valores)
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      
      # Valor 1: Seguridad
      card(
        class = "card-galaxy",
        style = "text-align: center; padding: 25px 15px; height: 100%;",
        div(style = "font-size: 38px; margin-bottom: 8px;", "🛡️"),
        h5(style = "color: #163A5F; font-weight: 700; margin-top: 5px; margin-bottom: 8px;", "Seguridad"),
        p(style = "font-size: 13px; color: #666; line-height: 1.6; margin: 0;", 
          "Garantizamos la integridad de nuestros pasajeros con unidades en óptimas condiciones.")
      ),
      
      # Valor 2: Calidad
      card(
        class = "card-galaxy",
        style = "text-align: center; padding: 25px 15px; height: 100%;",
        div(style = "font-size: 38px; margin-bottom: 8px;", "⭐"),
        h5(style = "color: #163A5F; font-weight: 700; margin-top: 5px; margin-bottom: 8px;", "Calidad"),
        p(style = "font-size: 13px; color: #666; line-height: 1.6; margin: 0;",
          "Ofrecemos servicios con los más altos estándares de excelencia y atención al detalle.")
      ),
      
      # Valor 3: Puntualidad
      card(
        class = "card-galaxy",
        style = "text-align: center; padding: 25px 15px; height: 100%;",
        div(style = "font-size: 38px; margin-bottom: 8px;", "⏰"),
        h5(style = "color: #163A5F; font-weight: 700; margin-top: 5px; margin-bottom: 8px;", "Puntualidad"),
        p(style = "font-size: 13px; color: #666; line-height: 1.6; margin: 0;",
          "Cumplimos con los horarios establecidos, respetando el tiempo de nuestros clientes.")
      ),
      
      # Valor 4: Respeto
      card(
        class = "card-galaxy",
        style = "text-align: center; padding: 25px 15px; height: 100%;",
        div(style = "font-size: 38px; margin-bottom: 8px;", "🙏"),
        h5(style = "color: #163A5F; font-weight: 700; margin-top: 5px; margin-bottom: 8px;", "Respeto"),
        p(style = "font-size: 13px; color: #666; line-height: 1.6; margin: 0;",
          "Tratamos a cada persona con dignidad y cortesía, valorando su confianza en nosotros.")
      )
    ),
    
    # Segunda fila (4 valores)
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      
      # Valor 5: Trabajo en equipo
      card(
        class = "card-galaxy",
        style = "text-align: center; padding: 25px 15px; height: 100%;",
        div(style = "font-size: 38px; margin-bottom: 8px;", "🤝"),
        h5(style = "color: #163A5F; font-weight: 700; margin-top: 5px; margin-bottom: 8px;", "Trabajo en Equipo"),
        p(style = "font-size: 13px; color: #666; line-height: 1.6; margin: 0;",
          "Colaboramos de manera coordinada para lograr los mejores resultados para nuestros clientes.")
      ),
      
      # Valor 6: Orientación al cliente
      card(
        class = "card-galaxy",
        style = "text-align: center; padding: 25px 15px; height: 100%;",
        div(style = "font-size: 38px; margin-bottom: 8px;", "💙"),
        h5(style = "color: #163A5F; font-weight: 700; margin-top: 5px; margin-bottom: 8px;", "Orientación al Cliente"),
        p(style = "font-size: 13px; color: #666; line-height: 1.6; margin: 0;",
          "Ponemos al cliente en el centro de todo lo que hacemos, superando sus expectativas.")
      ),
      
      # Valor 7: Innovación
      card(
        class = "card-galaxy",
        style = "text-align: center; padding: 25px 15px; height: 100%;",
        div(style = "font-size: 38px; margin-bottom: 8px;", "💡"),
        h5(style = "color: #163A5F; font-weight: 700; margin-top: 5px; margin-bottom: 8px;", "Innovación"),
        p(style = "font-size: 13px; color: #666; line-height: 1.6; margin: 0;",
          "Buscamos soluciones creativas y tecnológicas para mejorar constantemente nuestro servicio.")
      ),
      
      # Valor 8: Sustentabilidad
      card(
        class = "card-galaxy",
        style = "text-align: center; padding: 25px 15px; height: 100%;",
        div(style = "font-size: 38px; margin-bottom: 8px;", "🌱"),
        h5(style = "color: #163A5F; font-weight: 700; margin-top: 5px; margin-bottom: 8px;", "Sustentabilidad"),
        p(style = "font-size: 13px; color: #666; line-height: 1.6; margin: 0;",
          "Cuidamos el medio ambiente en cada viaje, adoptando prácticas responsables y sostenibles.")
      )
    )
      )),
    
    # ==================== PESTAÑA FILIALES (CON EFECTO HOVER) ====================
    tabPanel(
      "Filiales",
      div(
        class = "section",
        h2(class = "titulo", "Nuestras Filiales"),
        p(
          style = "text-align: center; font-size: 18px; max-width: 700px; margin: -20px auto 50px auto; color: #666;",
          "Empresas aliadas que comparten nuestra pasión por el transporte de calidad"
        ),
        
        layout_columns(
          col_widths = c(6, 6),
          
          # Filial 1: Lean Travel
          card(
            class = "card-galaxy",
            style = "
          text-align: center; 
          padding: 35px 25px; 
          height: 100%; 
          display: flex; 
          flex-direction: column; 
          align-items: center; 
          justify-content: center; 
          border-radius: 20px;
          transition: all 0.3s ease;
        ",
            div(
              style = "
            display: flex;
            align-items: center;
            justify-content: center;
            margin-bottom: 20px;
            width: 100%;
          ",
              div(
                style = "
              width: 160px; 
              height: 160px; 
              border-radius: 50%; 
              background: white;
              display: flex; 
              align-items: center; 
              justify-content: center; 
              box-shadow: 0 8px 30px rgba(0,0,0,0.12);
              padding: 20px;
              transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
              flex-shrink: 0;
              cursor: pointer;
            ",
                # Efecto hover con JavaScript inline
                onmouseenter = "this.style.transform='scale(1.08)'; this.style.boxShadow='0 12px 40px rgba(0,0,0,0.2)';",
                onmouseleave = "this.style.transform='scale(1)'; this.style.boxShadow='0 8px 30px rgba(0,0,0,0.12)';",
                img(
                  src = "img/filiales/lean-travel.png",
                  style = "
                max-width: 120px; 
                max-height: 120px; 
                object-fit: contain;
                display: block;
                pointer-events: none;
              ",
                  alt = "Lean Travel"
                )
              )
            ),
            h4(
              style = "
            color: #163A5F; 
            font-weight: 700; 
            margin-bottom: 10px; 
            font-size: 24px;
            text-align: center;
          ", 
              "Lean Travel"
            ),
            p(
              style = "
            font-size: 15px; 
            color: #666; 
            line-height: 1.8; 
            max-width: 400px; 
            margin: 0 auto;
            text-align: center;
          ",
              "Especialistas en la organización de viajes, excursiones y experiencias turísticas, 
              ofreciendo un servicio personalizado, seguro y de calidad."
            )
          ),
          
          # Filial 2: SUTT
          card(
            class = "card-galaxy",
            style = "
          text-align: center; 
          padding: 35px 25px; 
          height: 100%; 
          display: flex; 
          flex-direction: column; 
          align-items: center; 
          justify-content: center; 
          border-radius: 20px;
          transition: all 0.3s ease;
        ",
            div(
              style = "
            display: flex;
            align-items: center;
            justify-content: center;
            margin-bottom: 20px;
            width: 100%;
          ",
              div(
                style = "
              width: 160px; 
              height: 160px; 
              border-radius: 50%; 
              background: white;
              display: flex; 
              align-items: center; 
              justify-content: center; 
              box-shadow: 0 8px 30px rgba(0,0,0,0.12);
              padding: 20px;
              transition: all 0.4s cubic-bezier(0.175, 0.885, 0.32, 1.275);
              flex-shrink: 0;
              cursor: pointer;
            ",
                onmouseenter = "this.style.transform='scale(1.08)'; this.style.boxShadow='0 12px 40px rgba(0,0,0,0.2)';",
                onmouseleave = "this.style.transform='scale(1)'; this.style.boxShadow='0 8px 30px rgba(0,0,0,0.12)';",
                img(
                  src = "img/filiales/sutt.png",
                  style = "
                max-width: 120px; 
                max-height: 120px; 
                object-fit: contain;
                display: block;
                pointer-events: none;
              ",
                  alt = "SUTT"
                )
              )
            ),
            h4(
              style = "
            color: #163A5F; 
            font-weight: 700; 
            margin-bottom: 10px; 
            font-size: 24px;
            text-align: center;
          ", 
              "SUTT"
            ),
            p(
              style = "
            font-size: 15px; 
            color: #666; 
            line-height: 1.8; 
            max-width: 400px; 
            margin: 0 auto;
            text-align: center;
          ",
              "Especialistas en transporte de personal, brindando un servicio seguro, 
              puntual y confiable para empresas e instituciones."
            )
          )
        )
      )
    ),
    
    # ==================== PESTAÑA SUCURSALES ====================
    tabPanel(
      "Sucursales",
      div(
        class = "section",
        h2(class = "titulo", "Nuestras Sucursales"),
        layout_columns(
          col_widths = c(4,4,4),
          card(
            class = "card-galaxy",
            h4("Tarimoro"),
            p("Calle Melchor Ocampo 1B, planta alta "),
            p("📍 Tarimoro, Guanajuato"),
            p("📞 (466) 109 2559"),
            p("🕘 Lun-Vie 09:00-18:00 y sabados 9:00-13:00")
          ),
          card(
            class = "card-galaxy",
            h4("Celaya"),
            p("Av. Irrigación 137, Local 2"),
            p("📍 Celaya, Guanajuato"),
            p("📞 (461) 198 5878"),
            p("🕘 Lun-Vie 09:00-18:00 y sabados 9:00-13:00")
          ),
          card(
            class = "card-galaxy",
            h4("Querétaro"),
            p("Calle Hidalgo 189C"),
            p("📍 Querétaro, Qro."),
            p("📞 (461) 210 9868"),
            p("🕘 Lun-Vie 09:00-18:00 y sabados 9:00-13:00")
          )
        )
      )
    ),
    
    # ==================== PESTAÑA SERVICIOS ====================
    tabPanel(
      "Servicios",
      div(
        class = "section",
        h2(class = "titulo", "Nuestros Servicios"),
        layout_columns(
          col_widths = c(3,3,3,3),
          card(class = "card-galaxy", style = "text-align:center;",
               bs_icon("bus-front-fill", size = "3em", style = "color:#163A5F;"),
               br(), br(), h4("Turismo"), p("Viajes turísticos nacionales con unidades cómodas.")),
          card(class = "card-galaxy", style = "text-align:center;",
               bs_icon("building-fill", size = "3em", style = "color:#163A5F;"),
               br(), br(), h4("Empresarial"), p("Transporte para personal con puntualidad y seguridad.")),
          card(class = "card-galaxy", style = "text-align:center;",
               bs_icon("mortarboard-fill", size = "3em", style = "color:#163A5F;"),
               br(), br(), h4("Escolar"), p("Servicio para instituciones educativas con operadores certificados.")),
          card(class = "card-galaxy", style = "text-align:center;",
               bs_icon("airplane-fill", size = "3em", style = "color:#163A5F;"),
               br(), br(), h4("Traslados"), p("Servicio ejecutivo y traslados al aeropuerto."))
        )
      )
    ),
    
    # ==================== PESTAÑA UNIDADES ====================
    tabPanel(
      "Nuestras Unidades",
      div(
        class = "section",
        h2(class = "titulo", "Nuestras Unidades"),
        layout_columns(
          col_widths = c(3,3,3,3),
          card(class = "card-galaxy", style = "text-align:center;",
               bs_icon("bus-front-fill", size = "3em", style = "color:#163A5F;"),
               br(), br(), h4("Autobuses Sencillos"), p("Unidades cómodas para viajes turísticos.")),
          card(class = "card-galaxy", style = "text-align:center;",
               bs_icon("bus-front-fill", size = "3em", style = "color:#163A5F;"),
               br(), br(), h4("Autobuses Doble Piso"), p("Mayor capacidad y comodidad.")),
          card(class = "card-galaxy", style = "text-align:center;",
               bs_icon("truck-front-fill", size = "3em", style = "color:#163A5F;"),
               br(), br(), h4("Sprinter y Hiace"), p("Unidades ejecutivas para grupos pequeños.")),
          card(class = "card-galaxy", style = "text-align:center;",
               bs_icon("bus-front-fill", size = "3em", style = "color:#163A5F;"),
               br(), br(), h4("Volksbus"), p("Versátiles para diferentes tipos de viaje."))
        )
      )
    ),
    
    # ==================== PESTAÑA COTIZAR ====================
    tabPanel(
      "Cotizar",
      div(
        class = "section",
        h2(class = "titulo", "Solicita una Cotización"),
        layout_columns(
          col_widths = c(6,6),
          card(
            textInput("nombre", "Nombre completo *"),
            textInput("empresa", "Empresa (Opcional)"),
            textInput("telefono", "Teléfono"),
            textInput("correo", "Correo electrónico *"),
            selectInput("servicio_sel", "Servicio requerido *",
                        choices = c("Selecciona" = "", "Turismo", "Empresarial", "Escolar", "Traslado Ejecutivo", "Otro")),
            textAreaInput("mensaje", "Mensaje *", rows = 5),
            actionButton("enviar", "Enviar solicitud", class = "btn-galaxy")
          ),
          card(
            h3(style = "color:#163A5F;", "Información de Contacto"),
            hr(),
            h4("📍 Dirección"), p("Salamanca, Guanajuato"),
            h4("📞 Teléfonos"), p("(464) 000-0000"),
            h4("📧 Correo"), p("ventas@turismogalaxy.com"),
            hr(),
            h4("🕘 Horario"),
            p("Lunes a Viernes"),
            p("08:00 - 18:00")
          )
        )
      )
    ),
    
    # ==================== PESTAÑA ¿POR QUÉ ELEGIRNOS? ====================
    tabPanel(
      "¿Por qué elegirnos?",
      div(
        class = "section",
        h2(class = "titulo", "¿Por qué elegirnos?"),
        layout_columns(
          col_widths = c(4,4,4),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "🛡️"),
               h4("Seguridad"),
               p("Unidades con mantenimiento preventivo y seguro de viajero.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "⏰"),
               h4("Puntualidad"),
               p("Cumplimos con los horarios establecidos.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "👨‍✈️"),
               h4("Operadores Certificados"),
               p("Personal con amplia experiencia y capacitación."))
        ),
        layout_columns(
          col_widths = c(4,4,4),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "📍"),
               h4("Cobertura Nacional"),
               p("Servicios locales, regionales y nacionales.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "💬"),
               h4("Atención Personalizada"),
               p("Asesoría desde la cotización hasta el final del servicio.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "🚌"),
               h4("Flotilla Moderna"),
               p("Unidades equipadas para máximo confort."))
        )
      )
    ),
    
    # ==================== PESTAÑA ¿CÓMO CONTRATAR? ====================
    tabPanel(
      "¿Cómo contratar?",
      div(
        class = "section",
        h2(class = "titulo", "¿Cómo contratar?"),
        layout_columns(
          col_widths = c(3,3,3,3),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px; font-weight:bold; color:#00A651;", "1"),
               h4("Solicita tu cotización"),
               p("Contáctanos por teléfono, WhatsApp o el formulario.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px; font-weight:bold; color:#00A651;", "2"),
               h4("Recibe una propuesta"),
               p("Analizamos tus necesidades y elaboramos una cotización.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px; font-weight:bold; color:#00A651;", "3"),
               h4("Confirma tu reservación"),
               p("Aceptas la propuesta y programamos la unidad.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px; font-weight:bold; color:#00A651;", "4"),
               h4("Disfruta tu viaje"),
               p("Te brindamos un servicio seguro, puntual y confortable."))
        )
      )
    ),
    
    # ==================== PESTAÑA NUESTRO COMPROMISO ====================
    tabPanel(
      "Nuestro Compromiso",
      div(
        class = "section",
        h2(class = "titulo", "Nuestro Compromiso"),
        layout_columns(
          col_widths = c(4,4,4),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "🛡"),
               h4("Seguridad"),
               p("Todas nuestras unidades cuentan con mantenimiento preventivo.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "👨‍✈"),
               h4("Personal Capacitado"),
               p("Operadores con capacitación continua.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "🔧"),
               h4("Mantenimiento"),
               p("Revisiones periódicas para garantizar el óptimo funcionamiento."))
        ),
        layout_columns(
          col_widths = c(4,4,4),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "🚌"),
               h4("Flotilla Moderna"),
               p("Unidades cómodas y en excelentes condiciones.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "📍"),
               h4("Cobertura"),
               p("Servicios locales, regionales y nacionales.")),
          card(class = "card-galaxy", style = "text-align:center;",
               div(style = "font-size:48px;", "🤝"),
               h4("Atención Personalizada"),
               p("Acompañamos al cliente desde la cotización hasta el final."))
        )
      )
    ),
    
    # ==================== PESTAÑA PREGUNTAS FRECUENTES ====================
    tabPanel(
      "Preguntas Frecuentes",
      div(
        class = "section",
        h2(class = "titulo", "Preguntas Frecuentes"),
        accordion(
          id = "faq",
          accordion_panel(
            title = "¿Cómo solicito una cotización?",
            p("Puedes solicitar una cotización mediante el formulario de contacto, por teléfono o WhatsApp.")
          ),
          accordion_panel(
            title = "¿Realizan viajes fuera del estado?",
            p("Sí. Ofrecemos servicios locales, regionales y nacionales.")
          ),
          accordion_panel(
            title = "¿Las unidades cuentan con seguro?",
            p("Sí. Todas nuestras unidades cuentan con seguro de viajero.")
          ),
          accordion_panel(
            title = "¿Qué capacidad tienen las unidades?",
            p("Disponemos de diferentes capacidades para grupos pequeños y grandes.")
          ),
          accordion_panel(
            title = "¿Qué formas de pago aceptan?",
            p("Aceptamos transferencia bancaria, depósito y otros métodos.")
          ),
          accordion_panel(
            title = "¿Con cuánto tiempo debo reservar?",
            p("Recomendamos reservar con anticipación para asegurar disponibilidad.")
          )
        )
      )
    )
  ),
  
  # ============================================================
  # FOOTER
  # ============================================================
  div(
    style = "background:#0B1F36; color:white; padding:50px 30px 25px; margin-top:50px;",
    div(
      style = "max-width:1200px; margin:auto;",
      div(
        style = "display:grid; grid-template-columns:2fr 1fr 1fr 1fr; gap:30px;",
        div(
          img(src = "img/logo/Galaxy.png", style = "width:150px; margin-bottom:15px;", alt = "Turismo Galaxy"),
          p(style = "opacity:0.85; line-height:1.8; font-size:14px;",
            "Turismo Galaxy ofrece soluciones de transporte turístico, empresarial, ejecutivo y escolar.")
        ),
        div(
          h4(style = "color:white; margin-bottom:15px;", "Enlaces"),
          tags$ul(style = "list-style:none; padding:0; margin:0;",
                  tags$li(style = "margin-bottom:8px;", "Inicio"),
                  tags$li(style = "margin-bottom:8px;", "Nosotros"),
                  tags$li(style = "margin-bottom:8px;", "Servicios"),
                  tags$li(style = "margin-bottom:8px;", "Unidades")
          )
        ),
        div(
          h4(style = "color:white; margin-bottom:15px;", "Servicios"),
          tags$ul(style = "list-style:none; padding:0; margin:0;",
                  tags$li(style = "margin-bottom:8px;", "Turismo"),
                  tags$li(style = "margin-bottom:8px;", "Empresarial"),
                  tags$li(style = "margin-bottom:8px;", "Escolar"),
                  tags$li(style = "margin-bottom:8px;", "Traslados Ejecutivos")
          )
        ),
        div(
          h4(style = "color:white; margin-bottom:15px;", "Contacto"),
          p(style = "opacity:0.85; font-size:14px;", "📍 Salamanca, Guanajuato"),
          p(style = "opacity:0.85; font-size:14px;", "📞 (464) 000-0000"),
          p(style = "opacity:0.85; font-size:14px;", "✉ contacto@turismogalaxy.com")
        )
      )
    ),
    hr(style = "margin:30px 0 20px; border-color:rgba(255,255,255,0.15);"),
    div(
      style = "text-align:center; opacity:0.7; font-size:14px;",
      HTML("
        <strong>&copy; 2026 Turismo Galaxy</strong><br>
        Todos los derechos reservados.
        <br><br>
        <span style='opacity:.8;'>Powered by</span>
        <br>
        <span style='color:#00A651; font-weight:700; font-size:16px;'>
          Javix Mares &bull; Tonix Contreras
        </span>
        <br>
        <span style='opacity:.7; font-size:13px; letter-spacing:1px;'>
          Web Development Team
        </span>
      ")
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # Navegación a Cotizar
  observeEvent(input$cotizar, {
    updateTabsetPanel(session, "navbar", selected = "Cotizar")
  })
  
  # Navegación a Servicios desde Nosotros
  observeEvent(input$servicios_nosotros, {
    updateTabsetPanel(session, "navbar", selected = "Servicios")
  })
  
  # Envío de formulario
  observeEvent(input$enviar, {
    if (input$nombre == "" || input$correo == "" || input$servicio_sel == "" || input$mensaje == "") {
      showNotification("❌ Todos los campos son obligatorios", type = "error")
      return()
    }
    
    # Validar correo
    if (!grepl("@", input$correo)) {
      showNotification("❌ Ingresa un correo electrónico válido", type = "error")
      return()
    }
    
    showNotification(
      HTML(paste0("✅ ¡Cotización enviada! Hola <strong>", input$nombre, "</strong>, te contactaremos pronto.")),
      type = "success",
      duration = 10
    )
    
    # Limpiar formulario
    updateTextInput(session, "nombre", value = "")
    updateTextInput(session, "empresa", value = "")
    updateTextInput(session, "telefono", value = "")
    updateTextInput(session, "correo", value = "")
    updateSelectInput(session, "servicio_sel", selected = "")
    updateTextAreaInput(session, "mensaje", value = "")
  })
}

# ============================================================
# EJECUTAR
# ============================================================

shinyApp(ui = ui, server = server)