# uiImplications.R - (Versión 4.2: Wrapped in output.isContextLoaded check)

uiImplications <- tagList(
  conditionalPanel(
    condition = "!output.isContextLoaded",
    card(
      class = "border-0 shadow-sm py-5 text-center text-muted",
      div(class="py-5",
          icon("list-check", class="fa-5x mb-4 text-muted"),
          h3("No Formal Context Loaded", class="fw-bold text-dark"),
          p(class="text-muted fs-5 px-4", "Please go to the 'Formal Contexts' tab and import, connect, or generate a context first.")
      )
    )
  ),

  conditionalPanel(
    condition = "output.isContextLoaded",
    div(class = "d-flex justify-content-between align-items-center mb-3",
        div(class = "d-flex align-items-center gap-2",
            h2("Implications & Rules"),
            actionLink("btnImplicationsInfo", "", icon = icon("info-circle"), style = "cursor: pointer; font-size: 1.5em; color: #0d6efd;", title = "Show definitions & references")
        ),
        div(class = "d-flex gap-2",
            # Dropdown de Origen de Implicaciones (Implication origin)
            div(class = "dropdown",
                tags$button(class = "btn btn-primary dropdown-toggle btn-sm", 
                            type = "button", 
                            `data-bs-toggle` = "dropdown", 
                            `aria-expanded` = "false",
                            icon("compass"), " Implication origin"
                ),
                tags$ul(class = "dropdown-menu",
                        tags$li(actionLink("optComputeBasis", "Compute canonical basis (from context)", class = "dropdown-item", icon = icon("cube"))),
                        tags$li(actionLink("optMineImps", "Implication mining", class = "dropdown-item", icon = icon("hammer"))),
                        tags$li(actionLink("optImportImplicationsFile", "Import from file", class = "dropdown-item", icon = icon("file-import"))),
                        tags$li(actionLink("optAttrExploration", "Attribute exploration", class = "dropdown-item", icon = icon("route")))
                )
            ),
            div(class = "btn-group",
                actionButton("createLatexImplications", "LaTeX", icon = icon("code"), class = "btn-outline-primary btn-sm"),
                downloadButton("downloadRdsImp", "Save RDS", class = "btn-outline-secondary btn-sm")
            )
        )
    ),

    # --- VISTA SI NO HAY IMPLICACIONES CALCULADAS ---
    conditionalPanel(
      condition = "!output.has_implications",
      card(
        class = "text-center py-5 border-dashed shadow-sm",
        div(
          class = "card-body",
          icon("compass", class = "text-muted fs-1 mb-3"),
          h4("No implications loaded", class = "text-muted"),
          p("Please select an option from the ", strong("Implication origin"), " menu at the top to compute, mine, or import implications.", class = "text-muted")
        )
      )
    ),

    # --- VISTA SI HAY IMPLICACIONES CALCULADAS ---
    conditionalPanel(
      condition = "output.has_implications",
      
      # --- ACCORDEÓN 1: FILTRADO Y CALIDAD DE LAS REGLAS ---
      accordion(
        open = "Rule filtering and quality metrics",
        accordion_panel(
          "Rule filtering and quality metrics",
          icon = icon("filter"),
          
          # Filtros en horizontal usando tarjetas para un orden perfecto
          layout_columns(
            col_widths = c(3, 3, 3, 3),

            # Tarjeta 1: LHS
            card(
              card_header("Left Hand Side (IF)", class = "bg-primary text-white py-1"),
              class = "shadow-sm border-0",
              selectInput("selectLHS", "Contains:", choices = NULL, multiple = TRUE),
              selectInput("selectNotLHS", "Does NOT contain:", choices = NULL, multiple = TRUE)
            ),

            # Tarjeta 2: RHS
            card(
              card_header("Right Hand Side (THEN)", class = "bg-primary text-white py-1"),
              class = "shadow-sm border-0",
              selectInput("selectRHS", "Contains:", choices = NULL, multiple = TRUE),
              selectInput("selectNotRHS", "Does NOT contain:", choices = NULL, multiple = TRUE)
            ),

            # Tarjeta 3: Métricas
            card(
              card_header("Metrics", class = "bg-primary text-white py-1"),
              class = "shadow-sm border-0",
              sliderInput("support", "Min Support:", 0, 1, 0, step = 0.01),
              numericInput("minLift", "Min Lift:", value = 0.0, min = 0, max = 100, step = 0.1)
            ),

            # Tarjeta 4: Acciones
            card(
              card_header("Actions", class = "bg-dark text-white py-1"),
              class = "shadow-sm border-0",
              div(class = "d-grid gap-2 my-auto",
                  actionButton("btnApplyFilters", "Apply Filters", class = "btn-primary w-100", icon = icon("filter")),
                  actionButton("btnClearFilters", "Reset Filters", class = "btn-outline-secondary w-100", icon = icon("rotate"))
              )
            )
          ),
          hr(),

          # Panel Principal: Tabla Interactiva
          card(
            full_screen = TRUE,
            card_header("Interactive rules table", class = "bg-light"),
            height = "550px",
            DT::DTOutput("arulesTable", height = "100%")
          )
        )
      ),
      br(),

      # --- ACCORDEÓN 2: VISUALIZACIÓN DE REGLAS ---
      accordion(
        open = FALSE,
        accordion_panel(
          "Rule visualization",
          icon = icon("project-diagram"),
          layout_sidebar(
            sidebar = sidebar(
              title = "Visualization Controls",
              width = 300,
              class = "bg-light",

              selectInput("arulesPlotType", "Visualization Type:",
                          choices = c("Enriched Network Graph" = "graph",
                                      "Metrics Scatterplot" = "scatterplot",
                                      "Grouped Matrix" = "grouped")),
              
              # Mappings for Network Graph
              conditionalPanel(
                condition = "input.arulesPlotType == 'graph'",
                hr(),
                h6("Visual Mappings", class="text-muted mb-2"),
                selectInput("netColorMetric", "Node Color Metric:",
                            choices = c("Lift" = "lift", "Support" = "support"),
                            selected = "lift"),
                selectInput("netSizeMetric", "Node Size Metric:",
                            choices = c("Support" = "support", "Lift" = "lift"),
                            selected = "support"),
                uiOutput("netGraphLegendUI")
              ),
              
              # Mappings for Scatterplot
              conditionalPanel(
                condition = "input.arulesPlotType == 'scatterplot'",
                hr(),
                h6("Visual Mappings", class="text-muted mb-2"),
                selectInput("scatterXMetric", "X Axis Metric:",
                            choices = c("Support" = "support", "Lift" = "lift", "LHS Size" = "lhs_size", "RHS Size" = "rhs_size"),
                            selected = "support"),
                selectInput("scatterYMetric", "Y Axis Metric:",
                            choices = c("Lift" = "lift", "Support" = "support", "LHS Size" = "lhs_size", "RHS Size" = "rhs_size"),
                            selected = "lift"),
                selectInput("scatterColorMetric", "Marker Color:",
                            choices = c("LHS Size" = "lhs_size", "RHS Size" = "rhs_size", "Support" = "support", "Lift" = "lift"),
                            selected = "lhs_size"),
                selectInput("scatterSizeMetric", "Marker Size:",
                            choices = c("LHS Size" = "lhs_size", "RHS Size" = "rhs_size", "Support" = "support", "Lift" = "lift"),
                            selected = "lhs_size")
              )
            ),
            
            # Contenedor del Gráfico
            card(
              full_screen = TRUE,
              class = "border-0 shadow-sm",
              uiOutput("arulesPlotContainer", height = "600px")
            )
          )
        )
      ),
      br(),

      # --- ACCORDEÓN 3: OPERACIONES LÓGICAS ---
      accordion(
        open = FALSE,
        accordion_panel(
          "Logical operations",
          icon = icon("brain"),
          
          # Estructura de Pestañas Principales para Operaciones Lógicas
          navset_card_tab(
            id = "logical_ops_tabs",
            
            # Pestaña 1: Base Computation
            nav_panel("Base computation",
                      icon = icon("gears"),
                      p(class="text-muted mb-3", "Reduce the current implication set to canonical form, or compute direct-optimal system."),
                      layout_columns(
                        col_widths = c(6, 6),
                        card(
                          card_header("Canonical Basis"),
                          card_body(
                            p("Reduce the loaded implication set to its Duquenne-Guigues canonical basis. If no implications are loaded, computes from the formal context.")
                          ),
                          card_footer(
                            class = "border-top-0 bg-transparent pt-0",
                            actionButton("btnComputeBasis", "Reduce to canonical basis", class = "btn-primary w-100", icon = icon("cube"))
                          )
                        ),
                        card(
                          card_header("Direct-Optimal System"),
                          card_body(
                            p("Calculate the direct-optimal system. Note: this might take longer for larger contexts."),
                            div(class = "alert alert-warning py-2 mb-0 small", icon("triangle-exclamation"), " Direct-optimal calculation can be computationally expensive.")
                          ),
                          card_footer(
                            class = "border-top-0 bg-transparent pt-0",
                            actionButton("btnComputeDirectOptimal", "Compute direct-optimal system", class = "btn-warning w-100 text-dark", icon = icon("bolt"))
                          )
                        )
                      )
            ),

            # Pestaña 2: Simplification logic
            nav_panel("Simplification logic",
                      icon = icon("wand-magic-sparkles"),
                      layout_columns(
                        col_widths = c(4, 8),
                        card(
                          card_header("Simplification Controls"),
                          class = "shadow-sm border-0",
                          uiOutput("dynamicRulesUI"),
                          numericInput("batchSize", "Batch Size:", value = 10, min = 1, step = 1),
                          materialSwitch("reorder", "Reorder attributes", value = FALSE, status = "primary"),
                          actionButton("btnApplyLogic", "Apply Equivalence Rules", class = "btn-success w-100", icon = icon("play")),
                          hr(),
                          uiOutput("logicStats")
                        ),
                        card(
                          card_header("Current Implication Base"),
                          class = "shadow-sm border-0",
                          tableOutput("fcImplications")
                        )
                      )
            ),
            
            # Pestaña 3: Closure computation
            nav_panel("Closure computation",
                      icon = icon("compress"),
                      layout_columns(
                        col_widths = c(4, 8),
                        card(
                          card_header("Closure Parameters"),
                          p(class="text-muted small", "Compute closure of an attribute set under implications."),
                          selectInput("selectClosure", "Attributes Set (S):", choices = NULL, multiple = TRUE),
                          materialSwitch("ignoreFiltersClosure", "Ignore filters (use all rules)", value = TRUE, status = "info"),
                          div(class = "d-grid gap-2",
                              actionButton("btnComputeClosure", "Compute Closure", class = "btn-primary", icon = icon("compress"))
                          )
                        ),
                        card(
                          card_header("Results Preview"),
                          navset_underline(
                            id = "closure_results_tabs",
                            nav_panel("Closure (Set)", verbatimTextOutput("closureResultText")),
                            nav_panel("Reduced Implications", tableOutput("closureReducedImplications")),
                            nav_panel("Implications Base Preview", tableOutput("implicationsForClosurePreview"))
                          )
                        )
                      )
            ),
            
            # Pestaña 4: Hypothesis testing
            nav_panel("Hypothesis testing",
                      icon = icon("flask"),
                      layout_columns(
                        col_widths = c(5, 7),
                        card(
                          card_header("Define New Implication Rule"),
                          height = "500px",
                          p(class="text-muted", "Define a new rule (Hypothesis) and check if it holds in the current system."),
                          selectInput("hypLHS", "IF (Premise):", choices = NULL, multiple = TRUE, width = "100%"),
                          div(class="text-center my-2", icon("arrow-down"), style="font-size: 1.5rem; color: #aaa;"),
                          selectInput("hypRHS", "THEN (Conclusion):", choices = NULL, multiple = TRUE, width = "100%"),
                          hr(),
                          actionButton("btnValidate", "Check Implication", class = "btn-info w-100", icon = icon("check-double"))
                        ),
                        card(
                          card_header("Validation Report"),
                          height = "500px",
                          uiOutput("validationReportUI")
                        )
                      )
            )
          )
        )
      )
    )
  )
)
