library(shiny)
library(bslib)
library(shinyjs)
library(shinycssloaders)

uiLabs <- tagList(
  div(class = "d-flex justify-content-between align-items-center mb-3",
      div(class = "d-flex align-items-center gap-2",
          h2("fcaRviz Labs 🧪"),
          span(class = "badge bg-warning text-dark fs-6", "Experimental Features")
      ),
      div(class = "text-muted small", "Advanced & Experimental FCA Algorithms")
  ),
  
  div(
    class = "card border-0 shadow-sm p-4",
    div(class = "d-flex align-items-center gap-2 mb-3 pb-2 border-bottom",
        icon("square-poll-vertical", class = "fs-4 text-primary"),
        h4(class = "m-0 fw-bold", "Boolean Matrix Factorization (BMF)")
    ),
    
    p(class = "text-muted mb-4",
      "Boolean Matrix Factorization decomposes a binary incidence matrix ", 
      tags$code("I (m x n)"), " into two smaller binary matrices ", 
      tags$code("A (m x k)"), " and ", tags$code("B (k x n)"), 
      " such that ", tags$code("I ≈ A ∘ B"), " under Boolean matrix multiplication. ",
      "Formal concepts serve as high-quality interpretable factors."),
    
    div(id = "labs_no_ctx",
        card(
          class = "border-dashed text-center py-5 my-3 text-muted",
          icon("table", class = "fa-4x mb-3 text-muted"),
          h5("No Formal Context Loaded"),
          p("Please load a formal context in the 'Formal Contexts' tab to perform BMF factorizations.")
        )
    ),
    
    div(id = "labs_content", style = "display:none;",
        # --- TOP CONTROL BAR: ALGORITHM SELECTOR & EXECUTION ---
        div(
          class = "bg-light border rounded p-3 mb-4 shadow-sm",
          div(class = "row align-items-center g-3",
              div(class = "col-md-6",
                  selectInput("bmf_algorithm", "Factorization Algorithm:",
                              choices = c(
                                "GreConD (Greedy Concept on Demand)" = "GreConD",
                                "GreEss (Greedy Essential Factors)" = "GreEss",
                                "RSF (Rice-Siff Factorization)" = "RSF",
                                "RSF-ES (Rice-Siff Factorization with Early Stopping)" = "RSF-ES",
                                "ASSO (Association Rules Mining BMF)" = "ASSO",
                                "Hyper (Hypergraph BMF)" = "Hyper"
                              ), selected = "GreConD", width = "100%")
              ),
              div(class = "col-md-3",
                  div(class = "p-2 bg-white border rounded small text-info d-flex align-items-center gap-2 mt-4",
                      icon("circle-info", class = "text-info fs-5"),
                      div("Native fcaR BMF algorithm.")
                  )
              ),
              div(class = "col-md-3 text-end",
                  div(class = "mt-4",
                      actionButton("btnRunBMF", "Execute Factorization", class = "btn-primary w-100 py-2 fw-bold", icon = icon("play"))
                  )
              )
          )
        ),
        
        # --- RESULTS & FACTOR MATRICES VISUALIZATION ---
        uiOutput("bmfResultsUI")
    )
  )
)
