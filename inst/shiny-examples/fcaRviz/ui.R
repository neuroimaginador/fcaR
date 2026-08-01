# ui.R

source("uiUploadData.R")
source("uiBasicOperations.R")
source("uiImplications.R")
source("uiConcepts.R")

ui <- page_sidebar(
  title = NULL,
  theme = my_theme,
  shinyjs::useShinyjs(),
  rintrojs::introjsUI(),

  # Custom CSS Head and Shinyjs
  tags$head(
    tags$style(HTML("
      /* Fix bslib sidebar flex height & pin footer */
      .bslib-sidebar-layout .sidebar-content {
        display: flex !important;
        flex-direction: column !important;
        height: 100% !important;
      }
      .sidebar-footer-pinned {
        margin-top: auto !important;
      }
      .btn-settings-icon {
        border-radius: 6px;
        padding: 4px 8px;
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        color: #495057;
        transition: all 0.2s ease;
      }
      .btn-settings-icon:hover {
        background-color: #e9ecef;
        color: #0d6efd;
      }
      /* --- Premium Intro.js Guided Tour Styling --- */
      .introjs-tooltip {
        max-width: 390px !important;
        width: 90vw !important;
        background: #ffffff !important;
        border: 1px solid rgba(44, 62, 80, 0.12) !important;
        box-shadow: 0 16px 36px rgba(0, 0, 0, 0.16), 0 4px 12px rgba(0, 0, 0, 0.06) !important;
        border-radius: 16px !important;
        padding: 16px 20px 18px 20px !important;
        font-family: inherit !important;
        position: relative !important;
      }
      .introjs-tooltiptext {
        font-size: 0.9rem !important;
        line-height: 1.5 !important;
        color: #334155 !important;
        padding: 0 !important;
        margin-top: 4px !important;
      }
      .introjs-tooltip-header {
        margin-bottom: 8px;
        padding-right: 28px;
      }
      .introjs-badge {
        display: inline-block;
        font-size: 0.68rem;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        padding: 2px 8px;
        border-radius: 12px;
        background: rgba(13, 110, 253, 0.1);
        color: #0d6efd;
        margin-bottom: 4px;
      }
      .introjs-title {
        font-size: 1.08rem;
        font-weight: 700;
        color: #0f172a;
        margin: 0;
        display: flex;
        align-items: center;
        gap: 6px;
      }
      .introjs-skipbutton {
        position: absolute !important;
        top: 14px !important;
        right: 14px !important;
        width: 28px !important;
        height: 28px !important;
        display: inline-flex !important;
        align-items: center !important;
        justify-content: center !important;
        color: #64748b !important;
        font-size: 0.95rem !important;
        font-weight: 600 !important;
        border-radius: 50% !important;
        background: #f8fafc !important;
        border: 1px solid #e2e8f0 !important;
        text-decoration: none !important;
        transition: all 0.2s ease !important;
        line-height: 1 !important;
      }
      .introjs-skipbutton:hover {
        background: #fee2e2 !important;
        color: #ef4444 !important;
        border-color: #fca5a5 !important;
      }
      .introjs-tooltipbuttons {
        margin-top: 14px !important;
        padding-top: 12px !important;
        border-top: 1px solid #f1f5f9 !important;
        display: flex !important;
        align-items: center !important;
        justify-content: space-between !important;
        width: 100% !important;
        box-sizing: border-box !important;
      }
      .introjs-button {
        font-size: 0.82rem !important;
        font-weight: 600 !important;
        border-radius: 8px !important;
        padding: 6px 14px !important;
        text-shadow: none !important;
        box-shadow: none !important;
        transition: all 0.2s ease !important;
        display: inline-flex !important;
        align-items: center !important;
        justify-content: center !important;
      }
      .introjs-nextbutton {
        background: #0d6efd !important;
        color: #ffffff !important;
        border: 1px solid #0d6efd !important;
        float: right !important;
        margin-left: auto !important;
      }
      .introjs-nextbutton:hover {
        background: #0b5ed7 !important;
        color: #ffffff !important;
      }
      .introjs-prevbutton {
        background: #ffffff !important;
        color: #475569 !important;
        border: 1px solid #cbd5e1 !important;
        float: left !important;
      }
      .introjs-prevbutton:hover {
        background: #f8fafc !important;
        color: #0f172a !important;
      }
      .introjs-disabled, .introjs-disabled:hover {
        opacity: 0.4 !important;
        cursor: not-allowed !important;
        background: #f1f5f9 !important;
        border-color: #e2e8f0 !important;
        color: #94a3b8 !important;
      }
      .introjs-helperLayer {
        background: rgba(255, 255, 255, 0.2) !important;
        border: 2px solid #0d6efd !important;
        box-shadow: 0 0 20px rgba(13, 110, 253, 0.35) !important;
        border-radius: 8px !important;
      }
      .introjs-bullets {
        padding-top: 6px !important;
      }
      .introjs-bullets ul li a {
        width: 6px !important;
        height: 6px !important;
        background: #cbd5e1 !important;
      }
      .introjs-bullets ul li a.active {
        width: 18px !important;
        border-radius: 10px !important;
        background: #0d6efd !important;
      }
    "))
  ),

  sidebar = sidebar(
    title = tagList(
      div(id = "app_title_brand", class = "d-flex align-items-center justify-content-between py-1 w-100 me-2",
          div(class = "d-flex align-items-center gap-2",
              icon("cubes", class = "text-primary fs-3"),
              span(class = "fw-bold fs-4 text-dark", "fcaRviz")
          ),
          div(class = "d-flex gap-1 ms-auto",
              actionButton("btnStartTour", label = icon("compass"),
                           class = "btn-settings-icon", title = "Interactive Guided Tour"),
              actionButton("btnOpenSettings", label = icon("gear"),
                           class = "btn-settings-icon", title = "App Settings & Theme")
          )
      )
    ),
    width = 280,

    div(
      class = "mb-2",
      h6("FCA Workflow", class = "text-muted small text-uppercase fw-bold mb-2 px-1"),
      radioGroupButtons(
        inputId = "main_nav",
        label = NULL,
        choiceNames = list(
          tagList(icon("table"), " Formal Contexts"),
          tagList(icon("sliders"), " Basic Operations"),
          tagList(icon("project-diagram"), " Concepts Lattice"),
          tagList(icon("list-check"), " Implications"),
          tagList(icon("flask"), " Labs")
        ),
        choiceValues = c("upload_data", "basic_operations", "ui_concepts", "ui_implications", "ui_labs"),
        selected = "upload_data",
        direction = "vertical",
        status = "light",
        size = "lg",
        justified = TRUE,
        width = "100%"
      )
    ),

    hr(class = "my-2"),

    div(
      class = "mb-2",
      h6("Governance & History", class = "text-muted small text-uppercase fw-bold mb-2 px-1"),
      radioGroupButtons(
        inputId = "audit_nav",
        label = NULL,
        choiceNames = list(
          tagList(icon("clipboard-list"), " Project Info & Audit")
        ),
        choiceValues = c("ui_project_audit"),
        selected = character(0),
        direction = "vertical",
        status = "light",
        size = "lg",
        justified = TRUE,
        width = "100%"
      )
    ),

    # BOTTOM FOOTER (PINNED AT ABSOLUTE BOTTOM)
    div(
      class = "sidebar-footer-pinned pt-3 border-top bg-white rounded p-3 shadow-sm",
      div(class = "px-1 text-muted small",
          div(class = "d-flex align-items-center justify-content-between mb-2",
              span(class = "fw-bold text-dark", "fcaRviz v1.1"),
              a(href = "https://github.com/Malaga-FCA-group/fcaR", target = "_blank", class = "btn btn-xs btn-outline-secondary d-flex align-items-center gap-1 text-decoration-none py-1 px-2", title = "fcaR R Package Repository (Malaga-FCA-group)",
                icon("github", class = "fs-6"), span("GitHub"))
          ),
          div(class = "lh-sm text-muted", style = "font-size: 0.8rem;",
              div(class = "mb-1 d-flex align-items-center justify-content-between",
                  span("Domingo López-Rodríguez"),
                  a(href = "mailto:dominlopez@uma.es", class = "text-primary text-decoration-none ms-1", title = "Email Domingo López-Rodríguez", icon("envelope"))
              ),
              div(class = "mb-1 d-flex align-items-center justify-content-between",
                  span("Ángel Mora-Bonilla"),
                  a(href = "mailto:amora@uma.es", class = "text-primary text-decoration-none ms-1", title = "Email Ángel Mora-Bonilla", icon("envelope"))
              ),
              div(class = "text-muted opacity-75 mt-2 small", "© 2026 Universidad de Málaga")
          )
      )
    )
  ),

  navset_hidden(
    id = "hidden_tabs",
    nav_panel_hidden("upload_data", uiUploadData),
    nav_panel_hidden("basic_operations", uiBasicOperations),
    nav_panel_hidden("ui_concepts", uiConcepts),
    nav_panel_hidden("ui_implications", uiImplications),
    nav_panel_hidden("ui_labs", uiLabs),
    nav_panel_hidden("ui_project_audit", uiProjectAudit)
  )
)
