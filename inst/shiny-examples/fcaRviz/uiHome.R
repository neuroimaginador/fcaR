uiHome <- tagList(
  fluidRow(
    column(6, id="home-primero",
           div(class="text-center",
               h1("fcaRviz"), # <--- CAMBIO: Nuevo nombre
               img(src='logo.png', class="img-fluid", style="max-height: 300px;")
           )
    ),
    column(6, id="home-segundo",
           h4("Tools for Formal Concept Analysis"),
           p("Understand the core concepts of FCA and its implementation in R."),
           p("The fcaR package provides data structures which allow the user to work seamlessly with formal contexts and sets of implications."),
           br(),
           div(class="d-grid gap-2",
               actionButton("btn_start", "Get started", class="btn-primary btn-lg"),
               actionButton("btn-github", "Github docs", class="btn-outline-dark", onclick="window.open('https://github.com/Malaga-FCA-group/fcaR', '_blank')")
           )
    )
  )
)
