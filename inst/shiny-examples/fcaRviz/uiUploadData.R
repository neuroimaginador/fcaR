library(shinyjs)
library(shinycssloaders)
# uiUploadData.R - Version 4.0: hybrid static/dynamic layout

uiUploadData <- tagList(
  # Header Bar
  div(class = "d-flex justify-content-between align-items-center mb-3",
      div(class = "d-flex align-items-center gap-2",
          h2("Formal Contexts"),
          actionLink("btnFcaInfo", "", icon = icon("info-circle"),
                     style = "cursor: pointer; font-size: 1.5em; color: #0d6efd;",
                     title = "What is FCA?")
      ),
      div(class = "d-flex gap-2",
          div(class = "dropdown",
              tags$button(id = "btn_context_origin", class = "btn btn-primary dropdown-toggle", type = "button",
                          `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                          icon("table"), " Context origin"),
              tags$ul(class = "dropdown-menu dropdown-menu-end",
                      tags$li(actionLink("optImportFile", "Import file / Load project",
                                         class = "dropdown-item", icon = icon("file-arrow-up"))),
                      tags$li(actionLink("optConnectRepo", "Connect to Repository",
                                         class = "dropdown-item", icon = icon("github"))),
                      tags$li(actionLink("optGenerateSynthetic", "Generate synthetic context",
                                         class = "dropdown-item", icon = icon("dice"))),
                      tags$li(actionLink("optCreateEmpty", "Create empty context",
                                         class = "dropdown-item", icon = icon("plus-circle")))
              )
          )
      )
  ),

  # --- PLACEHOLDER: shown when no context is loaded ---
  uiOutput("noContextPlaceholderUI"),

  # --- CONTEXT TABS: always in DOM, hidden via shinyjs when no context ---
  div(id = "contextTabsWrapper", style = "display:none;",
    navset_card_tab(
      id = "data_management_tabs",
      height = "750px",

      nav_panel(
        title = "Context Viewer & Editor",
        icon = icon("table"),

        # Control bar
        div(class = "d-flex justify-content-between align-items-center bg-light p-3 border-bottom rounded-top mb-3",
            div(class = "d-flex gap-4 align-items-center",
                materialSwitch(inputId = "editMode", label = "Enable Edit Mode",
                               status = "warning", right = FALSE),
                materialSwitch(inputId = "ask_before_calc", label = "Ask before calculating",
                               value = TRUE, status = "info", right = FALSE)
            ),
            div(class = "d-flex gap-2",
                actionButton("btnUndoTransformation", "Undo",
                             icon = icon("rotate-left"), class = "btn-outline-danger btn-sm"),
                actionButton("btnRandomizeLoaded", "Randomize",
                             icon = icon("dice"), class = "btn-outline-secondary btn-sm"),
                div(class = "dropdown",
                    tags$button(class = "btn btn-info btn-sm dropdown-toggle text-white fw-semibold", type = "button",
                                `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                                icon("floppy-disk"), " Export & Save"),
                    tags$ul(class = "dropdown-menu dropdown-menu-end shadow-sm",
                            tags$li(downloadLink("btnSaveProjectBundleTop", tagList(icon("box-archive", class = "me-2 text-info"), "Save Full Project (.fcarviz)"), class = "dropdown-item fw-semibold")),
                            tags$li(tags$hr(class = "dropdown-divider")),
                            tags$li(downloadLink("exportCxt", tagList(icon("file-lines", class = "me-2 text-warning"), "Export Burmeister (.cxt)"), class = "dropdown-item")),
                            tags$li(downloadLink("exportCex", tagList(icon("file-export", class = "me-2 text-primary"), "Export ConExp (.cex)"), class = "dropdown-item")),
                            tags$li(downloadLink("exportCsvMatrix", tagList(icon("file-csv", class = "me-2 text-success"), "Export Incidence Matrix (.csv)"), class = "dropdown-item")),
                            tags$li(downloadLink("saveProject", tagList(icon("file-code", class = "me-2 text-secondary"), "Export Context RDS (.rds)"), class = "dropdown-item"))
                    )
                )
            )
        ),

        # View Mode
        div(id = "viewModePanel", class = "px-3 pb-3",
            withSpinner(DT::DTOutput("contents"), type = 4, color = "#2c3e50")
        ),

        # Edit Mode
        div(id = "editModePanel", class = "px-3 pb-3",
            tags$div(class = "alert alert-warning small py-2 mb-2", icon("pen"),
                     " Edit grid below. Use 'X' or '1' for incidence. Right-click to add/remove rows/cols."),
            rHandsontableOutput("hot_context", height = "450px"),
            hr(),
            div(class = "text-end",
                actionButton("btnUpdateContext", "Apply Changes & Update FC",
                             class = "btn-warning", icon = icon("sync"))
            )
        )
      ),

      nav_panel(
        title = "Multivalued Scaling",
        icon = icon("balance-scale"),
        card_body(
          h5("Formal Conceptual Scaling Wizard", class = "text-primary fw-bold mb-1"),
          p(class = "text-muted small mb-3",
            "If you loaded a multi-valued dataset, configure each column's scaling strategy to transform it into a binary context."),
          div(style = "max-height: 440px; overflow-y: auto; border: 1px solid rgba(0,0,0,0.1); border-radius: 8px; padding: 15px; background: rgba(255,255,255,0.45); margin-bottom: 15px;",
              uiOutput("scalingWizardUI")
          ),
          actionButton("btnPerformScaling", "Scale Context & Import to fcaRViz",
                       class = "btn-success w-100 py-2 fw-bold", icon = icon("cogs"))
        )
      )
    )
  )
)
