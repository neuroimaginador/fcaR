# uiConcepts.R - Versión 3.1 (Bot Híbrido)

library(bslib)
library(shinyjs)
library(visNetwork)
library(shinyWidgets)

uiConcepts <- tagList(
  useShinyjs(),
  tags$head(
    tags$script(HTML("
      (function() {
        function getLatticeNetwork() {
          var el = document.getElementById('interactivePlot');
          if (!el) return null;
          if (el.network) return el.network;
          if (window.HTMLWidgets && HTMLWidgets.find) {
            var w = HTMLWidgets.find('#interactivePlot');
            if (w && w.network) return w.network;
          }
          return null;
        }

        Shiny.addCustomMessageHandler('latticeVizControl', function(msg) {
          var net = getLatticeNetwork();
          if (!net) return;
          var scale = net.getScale();
          if (msg.action === 'zoomIn') {
            net.moveTo({ scale: scale * 1.25, animation: { duration: 200, easingFunction: 'easeInOutQuad' } });
          } else if (msg.action === 'zoomOut') {
            net.moveTo({ scale: scale * 0.8, animation: { duration: 200, easingFunction: 'easeInOutQuad' } });
          } else if (msg.action === 'fit') {
            net.fit({ animation: { duration: 300, easingFunction: 'easeInOutQuad' } });
          } else if (msg.action === 'moveUp') {
            var pos = net.getViewPosition();
            net.moveTo({ position: { x: pos.x, y: pos.y + 80 }, animation: { duration: 180 } });
          } else if (msg.action === 'moveDown') {
            var pos2 = net.getViewPosition();
            net.moveTo({ position: { x: pos2.x, y: pos2.y - 80 }, animation: { duration: 180 } });
          } else if (msg.action === 'moveLeft') {
            var pos3 = net.getViewPosition();
            net.moveTo({ position: { x: pos3.x + 80, y: pos3.y }, animation: { duration: 180 } });
          } else if (msg.action === 'moveRight') {
            var pos4 = net.getViewPosition();
            net.moveTo({ position: { x: pos4.x - 80, y: pos4.y }, animation: { duration: 180 } });
          }
        });

        function setLatticeFullscreen(on) {
          document.body.classList.toggle('lattice-fullscreen-mode', on);
          var btn = document.getElementById('btnLatticeTrueFullscreen');
          if (btn) {
            btn.innerHTML = on
              ? '<i class=\\'fa fa-compress\\'></i> Exit fullscreen'
              : '<i class=\\'fa fa-expand\\'></i> Fullscreen';
          }
          setTimeout(function() {
            var net = getLatticeNetwork();
            if (net) net.fit({ animation: { duration: 250 } });
          }, 250);
        }

        document.addEventListener('keydown', function(e) {
          if (e.key === 'Escape' && document.body.classList.contains('lattice-fullscreen-mode')) {
            setLatticeFullscreen(false);
          }
        });

        Shiny.addCustomMessageHandler('latticeToggleFullscreen', function(msg) {
          setLatticeFullscreen(!document.body.classList.contains('lattice-fullscreen-mode'));
        });

        window.fcaRvizSendLatticeEdgeEvent = function(inputId, edgeId, edge) {
          if (!edge) return;
          Shiny.setInputValue(inputId, {
            key: String(edgeId),
            from: String(edge.from),
            to: String(edge.to),
            t: Date.now()
          }, {priority: 'event'});
        };

        window.fcaRvizHandleLatticeEdgeClick = function(network, edgeId, edge) {
          window.fcaRvizSendLatticeEdgeEvent('lattice_click_edge', edgeId, edge);
          if (network && network.unselectAll) network.unselectAll();
        };

        window.fcaRvizHandleLatticeEdgeHover = function(edgeId, edge) {
          window.fcaRvizSendLatticeEdgeEvent('lattice_hover_edge', edgeId, edge);
        };
      })();
    ")),
    tags$style(HTML('
      /* --- Lattice visualization workspace --- */
      .lattice-viz-workspace {
        height: 100%;
        min-height: 0;
      }
      #lattice_card {
        height: 100%;
        display: flex;
        flex-direction: column;
        min-height: 0;
      }
      #lattice_card .card-body.lattice-viz-stage {
        flex: 1 1 auto;
        min-height: 420px;
        padding: 0 !important;
        overflow: hidden;
        position: relative;
        display: flex;
        flex-direction: column;
      }
      /* Hide default vis.js tooltips globally (appended to body, not inside card) */
      body .vis-tooltip,
      body div.vis-tooltip,
      .vis-tooltip,
      div.vis-tooltip {
        display: none !important;
        visibility: hidden !important;
        opacity: 0 !important;
        pointer-events: none !important;
      }

      #latticeContainer,
      #latticeContainer .shiny-spinner-placeholder,
      #interactivePlot {
        flex: 1 1 auto;
        min-height: 0 !important;
        height: 100% !important;
        width: 100% !important;
        position: relative;
        z-index: 1;
      }
      #latticeContainer {
        display: flex;
        flex-direction: column;
      }
      .shiny-spinner-placeholder {
        display: flex;
        flex-direction: column;
      }
      .lattice-viz-stage .vis-network,
      .lattice-viz-stage .vis-network canvas {
        z-index: 1 !important;
      }

      #lattice_card .card-footer {
        flex: 0 0 auto;
        padding: 4px 10px !important;
        min-height: 0 !important;
      }
      #lattice_card .card-footer .form-group {
        margin-bottom: 0 !important;
      }
      #lattice_card .card-footer .shiny-input-radiogroup {
        margin-top: 0 !important;
      }

      /* Custom info panel — top-right inside the viz stage */
      .lattice-viz-stage #latticeNodeInfoPanel,
      .lattice-viz-stage #latticeNodeInfoPanel.shiny-html-output {
        position: absolute !important;
        top: 10px !important;
        right: 10px !important;
        left: auto !important;
        bottom: auto !important;
        z-index: 2000 !important;
        max-width: min(400px, 44vw);
        pointer-events: none;
        margin: 0 !important;
        padding: 0 !important;
      }
      #latticeNodeInfoPanel .lattice-info-panel-inner {
        pointer-events: auto;
        background: rgba(255, 255, 255, 0.97);
        border: 1px solid rgba(44, 62, 80, 0.25);
        border-radius: 10px;
        box-shadow: 0 8px 28px rgba(0, 0, 0, 0.14);
        overflow: hidden;
        backdrop-filter: blur(8px);
      }
      .lattice-info-panel-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 8px;
        padding: 8px 10px;
        background: linear-gradient(135deg, #2c3e50, #34495e);
        color: #fff;
      }
      .lattice-info-panel-header .lattice-info-close {
        border: none;
        color: #2c3e50;
        padding: 2px 7px;
        line-height: 1;
      }
      .lattice-info-panel-body {
        padding: 10px 12px;
        font-size: 12.5px;
        line-height: 1.45;
        max-height: 280px;
        overflow-y: auto;
        color: #222;
      }
      .lattice-info-panel-body hr {
        margin: 6px 0;
        opacity: 0.25;
      }

      /* Navigation toolbar — always visible at bottom of viz stage */
      .lattice-nav-toolbar {
        position: absolute;
        left: 50%;
        bottom: 8px;
        transform: translateX(-50%);
        z-index: 1100;
        display: flex;
        align-items: center;
        gap: 4px;
        padding: 4px 8px;
        border-radius: 999px;
        background: rgba(44, 62, 80, 0.88);
        box-shadow: 0 4px 14px rgba(0, 0, 0, 0.22);
      }
      .lattice-nav-toolbar .btn {
        width: 30px;
        height: 30px;
        padding: 0;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        border-radius: 50%;
        border: 1px solid rgba(255,255,255,0.25);
        background: rgba(255,255,255,0.12);
        color: #fff;
      }
      .lattice-nav-toolbar .btn:hover {
        background: rgba(255,255,255,0.28);
        color: #fff;
      }
      .lattice-nav-toolbar .toolbar-label {
        color: rgba(255,255,255,0.75);
        font-size: 10px;
        font-weight: 600;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        padding: 0 4px;
      }

      /* True fullscreen — entire viewport for focused exploration */
      body.lattice-fullscreen-mode {
        overflow: hidden !important;
      }
      body.lattice-fullscreen-mode .bslib-sidebar-layout > .sidebar,
      body.lattice-fullscreen-mode .navbar,
      body.lattice-fullscreen-mode .bslib-page-title {
        display: none !important;
      }
      body.lattice-fullscreen-mode .bslib-sidebar-layout > .main {
        margin: 0 !important;
        padding: 0 !important;
        width: 100vw !important;
        max-width: 100vw !important;
      }
      body.lattice-fullscreen-mode #hidden_tabs,
      body.lattice-fullscreen-mode .tab-content {
        height: 100vh !important;
      }
      body.lattice-fullscreen-mode .lattice-viz-workspace {
        position: fixed !important;
        inset: 0 !important;
        z-index: 20000 !important;
        background: #fff;
        padding: 0 !important;
        margin: 0 !important;
      }
      body.lattice-fullscreen-mode #lattice_card {
        height: 100vh !important;
        border-radius: 0 !important;
        border: none !important;
      }
      body.lattice-fullscreen-mode #lattice_card .card-body.lattice-viz-stage {
        min-height: 0 !important;
      }
      body.lattice-fullscreen-mode .layout-columns {
        display: block !important;
      }
      body.lattice-fullscreen-mode .layout-columns > .col:nth-child(2) {
        display: none !important;
      }
      body.lattice-fullscreen-mode .layout-columns > .col:first-child {
        width: 100% !important;
        max-width: 100% !important;
      }
    '))
  ),

  conditionalPanel(
    condition = "!output.isContextLoaded",
    card(
      class = "border-0 shadow-sm py-5 text-center text-muted",
      div(
        class = "py-5",
        icon("project-diagram", class = "fa-5x mb-4 text-muted"),
        h3("No Formal Context Loaded", class = "fw-bold text-dark"),
        p(
          class = "text-muted fs-5 px-4",
          "Please go to the 'Formal Contexts' tab and import, connect, or generate a context first."
        )
      )
    )
  ),

  conditionalPanel(
    condition = "output.isContextLoaded",
    div(class = "d-flex justify-content-between align-items-center mb-3",
      h2("Concept Lattice Exploration"),
      div(class = "btn-group",
          div(class = "btn-group",
              tags$button(
                class = "btn btn-outline-primary btn-sm dropdown-toggle",
                type = "button",
                `data-bs-toggle` = "dropdown",
                `aria-expanded` = "false",
                icon("download"), " Export"
              ),
              tags$ul(
                class = "dropdown-menu dropdown-menu-end",
                tags$li(downloadLink("downloadSVG", "Download SVG (Vector)", class = "dropdown-item")),
                tags$li(downloadLink("downloadPDF", "Download PDF (Vector)", class = "dropdown-item")),
                tags$li(tags$hr(class = "dropdown-divider")),
                tags$li(downloadLink("downloadDOT", "Export Graphviz (DOT)", class = "dropdown-item")),
                tags$li(downloadLink("downloadGraphML", "Export Gephi (GraphML)", class = "dropdown-item"))
              )
          ),
          downloadButton("downloadRdsConp", "Save RDS", class = "btn-outline-secondary btn-sm"),
          actionButton("createLatexConcepts", "LaTeX Code", icon = icon("code"), class = "btn-outline-primary btn-sm")
      )
  ),

  navset_card_pill(
    id = "concepts_tabs",

    # --- PESTAÑA 1: EXPLORADOR ---
    nav_panel("Interactive Explorer",
              layout_columns(
                col_widths = c(8, 4),
                row_heights = "minmax(520px, calc(100vh - 200px))",
                div(
                  class = "lattice-viz-workspace",
                  card(
                    id = "lattice_card",
                    card_header(
                      div(class="d-flex justify-content-between align-items-center w-100 flex-wrap gap-2",
                          div(class="d-flex align-items-center gap-2",
                              span("Lattice Visualization"),
                              actionLink("btnLatticeAlgoInfo", "", icon = icon("info-circle"), style = "cursor: pointer; color: #0d6efd;", title = "Algorithm & theory info")
                          ),
                          div(class="d-flex gap-2 align-items-center flex-wrap",
                               radioButtons("exploration_mode", NULL,
                                            choices = c("Macro View" = "macro", "Micro Explorer" = "micro"),
                                            selected = "macro", inline = TRUE),
                               selectInput("vizLayout", NULL,
                                           choices = c("Hierarchical" = "layout_with_sugiyama",
                                                       "Force Directed" = "layout_with_kk"),
                                           selected = "layout_with_sugiyama",
                                           width = "150px"),
                               actionButton(
                                 "btnLatticeTrueFullscreen",
                                 label = tagList(icon("expand"), "Fullscreen"),
                                 class = "btn btn-outline-dark btn-sm"
                               )
                          )
                      ),
                      class = "bg-light py-2"
                    ),
                    conditionalPanel(
                      condition = "input.exploration_mode == 'micro'",
                      div(class="px-2 py-1 bg-light border-bottom d-flex align-items-center gap-3",
                          numericInput("center_concept_id", "Current Focal Concept (ID)", value = 1, min = 1, step = 1, width = "180px"),
                          helpText(icon("mouse-pointer"), " Click any node to navigate the lattice.", class="text-muted mb-0 small")
                      )
                    ),
                    card_body(
                      class = "lattice-viz-stage",
                      withSpinner(uiOutput("latticeContainer"), type = 4, color = "#2c3e50"),
                      uiOutput("latticeNodeInfoPanel"),
                      div(
                        class = "lattice-nav-toolbar",
                        span(class = "toolbar-label", "Navigate"),
                        actionButton("latticeMoveUp", label = NULL, icon = icon("arrow-up"), class = "btn btn-sm", title = "Move up"),
                        actionButton("latticeMoveDown", label = NULL, icon = icon("arrow-down"), class = "btn btn-sm", title = "Move down"),
                        actionButton("latticeMoveLeft", label = NULL, icon = icon("arrow-left"), class = "btn btn-sm", title = "Move left"),
                        actionButton("latticeMoveRight", label = NULL, icon = icon("arrow-right"), class = "btn btn-sm", title = "Move right"),
                        span(class = "toolbar-label", "Zoom"),
                        actionButton("latticeZoomIn", label = NULL, icon = icon("plus"), class = "btn btn-sm", title = "Zoom in"),
                        actionButton("latticeZoomOut", label = NULL, icon = icon("minus"), class = "btn btn-sm", title = "Zoom out"),
                        actionButton("latticeFit", label = NULL, icon = icon("compress-arrows-alt"), class = "btn btn-sm", title = "Fit view")
                      )
                    ),
                    card_footer(
                      class = "bg-white border-top",
                      div(class="d-flex align-items-center justify-content-between flex-wrap gap-2 py-0",
                          div(class="d-flex align-items-center gap-2 flex-wrap",
                              span(class="small", icon("palette"), strong("Color:")),
                              radioButtons("color_mode", NULL,
                                           choices = c("Irreducibles" = "irreducibles",
                                                       "Stability" = "stability",
                                                       "Separation" = "separation",
                                                       "Fuzzy Density" = "density"),
                                           selected = "irreducibles", inline = TRUE)
                          ),
                          uiOutput("latticeLegendUI")
                      )
                    )
                  )
                ),
                card(
                  card_header("Layout and info"),
                  accordion(
                    id = "lattice_sidebar_accordion",
                    open = "Lattice Properties",
                    accordion_panel("Lattice Properties", icon = icon("circle-info"), uiOutput("latticePropertiesUI")),
                    accordion_panel(
                      "Concept Filters",
                      icon = icon("filter"),
                      selectizeInput("filterObjs", "Filter by Objects:", choices = NULL, multiple = TRUE, width = "100%", options = list(placeholder = "Select objects...")),
                      selectizeInput("filterAtts", "Filter by Attributes:", choices = NULL, multiple = TRUE, width = "100%", options = list(placeholder = "Select attributes...")),
                      div(
                        class = "d-flex gap-2 mt-2",
                        actionButton("btnApplyConceptFilters", "Apply", class = "btn-success w-100", icon = icon("filter")),
                        actionButton("btnClearConceptFilters", "Clear", class = "btn-outline-danger w-100", icon = icon("eraser"))
                      ),
                      checkboxInput("bypassFilters", "Bypass/Disable filters (show full lattice)", value = FALSE),
                      div(
                        class = "text-muted small mt-1 text-center fw-bold text-success",
                        textOutput("filterStatusBadge")
                      )
                    ),
                    accordion_panel("Selected Concept", icon = icon("bullseye"), uiOutput("conceptSelected")),
                    accordion_panel("Association Rule", icon = icon("link"), uiOutput("associationRuleSelected")),
                    accordion_panel(
                      "Lattice Style Settings",
                      icon = icon("sliders"),
                      checkboxInput("scale_node_support", "Scale node size by support (extent cardinality)", value = FALSE),
                      checkboxInput("enable_threshold", "Enable Concept Limit Warning", value = TRUE),
                      conditionalPanel(
                        condition = "input.enable_threshold == true",
                        numericInput("safety_threshold", "Limit Threshold (Concepts):", value = 800, min = 10, step = 50)
                      ),
                      radioButtons("labeling_mode", "Labeling Mode:",
                                   choices = c("Classic Reduced (FCA)" = "classic_reduced",
                                               "Attributes Only (Reduced)" = "attributes_only",
                                               "Objects Only (Reduced)" = "objects_only",
                                               "Full Intent & Extent" = "full",
                                               "Concept ID Only" = "id_only"),
                                   selected = "classic_reduced")
                    ),
                    accordion_panel("Upper Neighbours (General)", icon = icon("arrow-up"), uiOutput("upperNeighbours")),
                    accordion_panel("Lower Neighbours (Specific)", icon = icon("arrow-down"), uiOutput("lowerNeighbours")),
                    accordion_panel("Semantic Journey (Timeline)", icon = icon("clock-rotate-left"), uiOutput("semanticJourneyTimeline"))
                  )
                )
              )
    ),

    # --- PESTAÑA 2: FILTRADO DEL RETÍCULO ---
    nav_panel("Lattice Filtering",
              icon = icon("filter"),
              layout_columns(
                col_widths = c(4, 8),
                card(
                  card_header(
                    div(class="d-flex justify-content-between align-items-center w-100",
                        span("Filter Configuration"),
                        actionLink("btnFilterHelp", "", icon = icon("info-circle"), style = "cursor: pointer; color: #0d6efd;", title = "How does filtering work?")
                    )
                  ),
                  card_body(
                    selectizeInput("filterObjsTab", "Filter by Objects (Must contain ALL):", choices = NULL, multiple = TRUE, width = "100%", options = list(placeholder = "Select objects...")),
                    selectizeInput("filterAttsTab", "Filter by Attributes (Must contain ALL):", choices = NULL, multiple = TRUE, width = "100%", options = list(placeholder = "Select attributes...")),
                    hr(),
                    div(
                      class = "d-grid gap-2",
                      actionButton("btnApplyTabFilters", "Apply Filter", class = "btn-primary", icon = icon("filter")),
                      actionButton("btnClearTabFilters", "Clear Filter", class = "btn-outline-danger", icon = icon("eraser"))
                    ),
                    br(),
                    div(
                      class = "alert alert-info py-2 text-center mb-0 small fw-bold",
                      textOutput("tabFilterStatusBadge")
                    )
                  )
                ),
                card(
                  card_header("Matching Concepts Table"),
                  card_body(
                    div(style = "overflow-y: auto; max-height: 500px;",
                        withSpinner(tableOutput("tabFilteredTable"), type=4, color = "#2c3e50")
                    )
                  )
                )
              )
    ),
    
    # --- PESTAÑA 3: REGLAS DE ASOCIACIÓN DEL RETÍCULO ---
    nav_panel("Association Rules",
              icon = icon("calculator"),
              layout_columns(
                col_widths = c(4, 8),
                card(
                  card_header("Rules Configuration"),
                  card_body(
                    sliderInput("assoc_support", "Min Support:", min = 0.0, max = 1.0, value = 0.1, step = 0.05),
                    sliderInput("assoc_confidence", "Min Confidence:", min = 0.0, max = 1.0, value = 0.5, step = 0.05),
                    radioButtons("assoc_method", "Extraction Method:",
                                 choices = c("Covering Relations (Luxenburger Basis - Fast)" = "covering",
                                             "All Pairs (Exhaustive - Slower)" = "all"),
                                 selected = "covering"),
                    checkboxInput("assoc_use_selected_concept", "Only rules associated with selected concept in Explorer", value = FALSE),
                    hr(),
                    div(
                      class = "d-grid gap-2",
                      actionButton("btnComputeAssocRules", "Extract Rules", class = "btn-primary", icon = icon("play")),
                      downloadButton("downloadAssocRulesRds", "Save Rules (.rds)", class = "btn-outline-success")
                    ),
                    br(),
                    div(
                      class = "alert alert-info py-2 text-center mb-0 small fw-bold",
                      textOutput("assocRulesStatusBadge")
                    )
                  )
                ),
                card(
                  card_header("Lattice Association Rules Table"),
                  card_body(
                    div(style = "overflow-y: auto; max-height: 500px;",
                        withSpinner(DT::DTOutput("assocRulesTable"), type = 4, color = "#2c3e50")
                    )
                  )
                )
              )
    )
    )
  )
)
