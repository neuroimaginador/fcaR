# ui.R

source("uiUploadData.R")
source("uiBasicOperations.R")
source("uiImplications.R")
source("uiConcepts.R")

ui <- page_sidebar(
  title = "fcaRviz",
  theme = my_theme,

  # Custom CSS Head and Shinyjs
  tags$head(
    shinyjs::useShinyjs(),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$style(HTML("
      /* Estilos adicionales personalizados para FCA */
      .card-header {
        font-weight: bold;
      }
      .handsontable table {
        width: 100% !important;
      }
      .nav-pills .nav-link.active, .nav-pills .show>.nav-link {
        background-color: #2c3e50;
      }
    "))
  ),

  sidebar = sidebar(
    title = "Main Menu",
    width = 280,

    radioGroupButtons(
      inputId = "main_nav",
      label = NULL,
      choiceNames = list(
        tagList(icon("table"), " Formal Contexts"),
        tagList(icon("sliders"), " Basic Operations"),
        tagList(icon("project-diagram"), " Concepts Lattice"),
        tagList(icon("list-check"), " Implications")
      ),
      choiceValues = c("upload_data", "basic_operations", "ui_concepts", "ui_implications"),
      direction = "vertical",
      status = "light",
      size = "lg",
      justified = TRUE,
      width = "100%"
    ),

    div(class="mt-3"),

    div(class="px-2",
        h6("Appearance", class = "text-muted mb-2"),
        selectInput("theme_selector", NULL,
                    choices = c("Zephyr", "Flatly", "Yeti", "Minty", "Cosmo", "Pulse", "Sandstone", "United", "Darkly", "Slate", "Cyborg", "Superhero"),
                    selected = "Zephyr", width = "100%")
    ),

    hr(),
    div(class="px-2 text-muted", style="font-size: 0.8rem;",
        p(class="mb-2", strong("Main authors:"), br(), "Domingo Lopez Rodriguez", br(), "Angel Mora Bonilla"),
        p(class="mb-0", strong("e-mails:"), br(), "dominlopez@uma.es", br(), "amora@uma.es")
    ),

    div(class = "mt-auto",
        hr(),
        p(class="text-center text-muted small", "v1.1 - fcaRviz")
    )
  ),

  navset_hidden(
    id = "hidden_tabs",
    nav_panel_hidden("upload_data", uiUploadData),
    nav_panel_hidden("basic_operations", uiBasicOperations),
    nav_panel_hidden("ui_concepts", uiConcepts),
    nav_panel_hidden("ui_implications", uiImplications)
  )
)
