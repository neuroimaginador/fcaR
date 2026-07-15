library(DT)
library(shinycssloaders)

uiBasicOperations <- tagList(
  conditionalPanel(
    condition = "!output.isContextLoaded",
    card(
      class = "border-0 shadow-sm py-5 text-center text-muted",
      div(class="py-5",
          icon("sliders", class="fa-5x mb-4 text-muted"),
          h3("No Formal Context Loaded", class="fw-bold text-dark"),
          p(class="text-muted fs-5 px-4", "Please go to the 'Formal Contexts' tab and import, connect, or generate a context first.")
      )
    )
  ),
  
  conditionalPanel(
    condition = "output.isContextLoaded",
    layout_columns(
      col_widths = c(6, 6),
  
      # Columna Izquierda: Contexto
      card(
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            span("Formal Context Matrix"),
            materialSwitch(
              inputId = "showArrowRelations_basic",
              label = "Add arrows",
              status = "info",
              right = TRUE
            )
          )
        ),
        height = "700px",
        withSpinner(DTOutput("tableData_DT"), type = 4, color = "#2c3e50"),
        card_footer(
          div(
            class = "d-flex flex-wrap gap-2",
            actionButton("clarify", "Clarify", class = "btn-outline-primary"),
            actionButton("reduce", "Reduce", class = "btn-outline-primary"),
            actionButton(
              "reduceArrows",
              "Reduce arrows",
              class = "btn-outline-primary",
              icon = icon("compress-arrows-alt"),
              title = "Clarify and reduce using arrow relations (fcaR 1.7+, binary contexts)"
            )
          )
        )
      ),
  
      # Columna Derecha: Operaciones
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center w-100",
              span("Set Operations"),
              actionLink("btnBasicOpsInfo", "", icon = icon("info-circle"), style = "cursor: pointer; color: #0d6efd;", title = "Show definitions & references")
          )
        ),
        height = "700px",
        accordion(
          open = "Intent",
          accordion_panel("Intent",
                          selectInput("intentOptions", "Choose objects:", choices = NULL, multiple = TRUE, width = "100%"),
                          uiOutput("intentResult")
          ),
          accordion_panel("Extent",
                          selectInput("extentOptions", "Choose attributes:", choices = NULL, multiple = TRUE, width = "100%"),
                          uiOutput("extentResult")
          ),
          accordion_panel("Closure",
                          selectInput("closureOptions", "Compute closure for:", choices = NULL, multiple = TRUE, width = "100%"),
                          uiOutput("closureResult")
          )
        )
      )
    )
  )
)
