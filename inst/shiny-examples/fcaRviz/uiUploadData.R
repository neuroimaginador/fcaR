library(shinyjs)
library(shinycssloaders)
# uiUploadData.R - Versión 2.1 (Alignment fixes, removed arrows switch)

uiUploadData <- tagList(
  # Header Bar
  div(class = "d-flex justify-content-between align-items-center mb-3",
      div(class = "d-flex align-items-center gap-2",
          h2("Formal Contexts"),
          actionLink("btnFcaInfo", "", icon = icon("info-circle"), style = "cursor: pointer; font-size: 1.5em; color: #0d6efd;", title = "What is FCA?")
      ),
      div(class = "d-flex gap-2",
          # Dropdown de Origen del Contexto (Context origin)
          div(class = "dropdown",
              tags$button(class = "btn btn-primary dropdown-toggle", type = "button", `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
                          icon("table"), " Context origin"),
              tags$ul(class = "dropdown-menu dropdown-menu-end",
                      tags$li(actionLink("optImportFile", "Import file / Load project", class = "dropdown-item", icon = icon("file-arrow-up"))),
                      tags$li(actionLink("optConnectRepo", "Connect to Repository", class = "dropdown-item", icon = icon("github"))),
                      tags$li(actionLink("optGenerateSynthetic", "Generate synthetic context", class = "dropdown-item", icon = icon("dice"))),
                      tags$li(actionLink("optCreateEmpty", "Create empty context", class = "dropdown-item", icon = icon("plus-circle")))
              )
          )
      )
  ),
  
  # Main Layout Area (Full width)
  # Placeholder (when no context is loaded)
  conditionalPanel(
    condition = "!output.isContextLoaded",
    card(
      class = "border-0 shadow-sm py-5 text-center text-muted",
      div(class="py-5",
          icon("table", class="fa-5x mb-4 text-muted"),
          h3("No Formal Context Loaded", class="fw-bold text-dark"),
          p(class="text-muted fs-5 px-4", "Choose an option from the 'Context origin' menu at the top right to import, connect, or generate a context.")
      )
    )
  ),
  
  # Loaded Context View
  conditionalPanel(
    condition = "output.isContextLoaded",
    navset_card_tab(
      id = "data_management_tabs",
      height = "750px",
      
      nav_panel(
        title = "Context Viewer & Editor",
        icon = icon("table"),
        
        # Upper control bar inside the card
        div(class = "d-flex justify-content-between align-items-center bg-light p-3 border-bottom rounded-top mb-3",
            div(class = "d-flex gap-4 align-items-center",
                # Edit mode switch
                materialSwitch(inputId = "editMode", label = "Enable Edit Mode", status = "warning", right = FALSE),
                # Ask before calculating switch
                materialSwitch(inputId = "ask_before_calc", label = "Ask before calculating", value = TRUE, status = "info", right = FALSE)
            ),
            div(
              # Randomize button
              actionButton("btnRandomizeLoaded", "Randomize Context", icon = icon("dice"), class = "btn-outline-secondary btn-sm")
            )
        ),
        
        # Table Content Area
        card_body(
          class = "p-0",
          
          # View Mode
          conditionalPanel(
            condition = "input.editMode == false",
            div(class = "px-3 pb-3",
                withSpinner(DT::DTOutput("contents"), type=4, color = "#2c3e50")
            )
          ),
          
          # Edit Mode
          conditionalPanel(
            condition = "input.editMode == true",
            div(class="px-3 pb-3",
                tags$div(class="alert alert-warning small py-2 mb-2", icon("pen"),
                         " Edit grid below. Use 'X' or '1' for incidence. Right-click to add/remove rows/cols."),
                rHandsontableOutput("hot_context", height = "450px"),
                hr(),
                div(class="text-end",
                    actionButton("btnUpdateContext", "Apply Changes & Update FC", class = "btn-warning", icon = icon("sync"))
                )
            )
          )
        ),
        
        # Export Buttons Footer
        card_footer(
          class = "bg-white border-top-0 d-flex justify-content-end gap-2 py-3",
          downloadButton("saveProject", "Save Project (.rds)", class = "btn-success btn-sm"),
          downloadButton("exportCex", "Export to ConExp (.cex)", class = "btn-outline-info btn-sm")
        )
      ),
      
      nav_panel(
        title = "Multivalued Scaling",
        icon = icon("balance-scale"),
        
        card_body(
          h5("Formal Conceptual Scaling Wizard", class="text-primary fw-bold mb-1"),
          p(class="text-muted small mb-3",
            "If you loaded a multi-valued dataset (categorical categories or continuous numbers), configure each column's scaling strategy below to transform it into a formal binary context."),
          
          div(style="max-height: 440px; overflow-y: auto; border: 1px solid rgba(0,0,0,0.1); border-radius: 8px; padding: 15px; background: rgba(255,255,255,0.45); margin-bottom: 15px;",
              uiOutput("scalingWizardUI")
          ),
          
          actionButton("btnPerformScaling", "Scale Context & Import to fcaRViz", 
                       class = "btn-success w-100 py-2 fw-bold", icon = icon("cogs"))
        )
      ),
      
      nav_panel(
        title = "Context Metadata",
        icon = icon("circle-info"),
        card_body(
          uiOutput("datasetDocumentationUI")
        )
      )
    )
  )
)
