library(shiny)
library(bslib)
library(shinyjs)
library(shinycssloaders)

uiProjectAudit <- tagList(
  div(class = "d-flex justify-content-between align-items-center mb-3",
      div(class = "d-flex align-items-center gap-2",
          h2("Project Info & Audit Log 📋"),
          span(class = "badge bg-info text-white fs-6", "Reproducibility & Governance")
      ),
      div(class = "text-muted small", "Full Project Governance & History")
  ),
  
  div(id = "project_audit_no_ctx",
      card(
        class = "border-dashed text-center py-5 my-3 text-muted shadow-sm bg-light-subtle",
        icon("folder-open", class = "fa-4x mb-3 text-info"),
        h4(class = "fw-bold text-dark mb-2", "No Formal Context Loaded"),
        p(class = "text-muted fs-6 mb-4 px-4", "Load a dataset from 'Formal Contexts' or restore an existing .fcarviz / .rds project archive directly below:"),
        div(class = "d-flex justify-content-center align-items-center gap-2",
            fileInput("btnRestoreProjectFileNoCtx", NULL, buttonLabel = tagList(icon("box-archive"), "Restore Project (.fcarviz / .rds)"), placeholder = "Select project file...", accept = c(".rds", ".fcarviz"), width = "320px")
        )
      )
  ),
  
  div(id = "project_audit_content", style = "display:none;",
      div(
        class = "card border-0 shadow-sm p-3 mb-4 bg-white rounded border-start border-4 border-info",
        div(class = "d-flex justify-content-between align-items-center flex-wrap gap-3",
            div(class = "d-flex align-items-center gap-3",
                div(class = "bg-info-subtle p-2 rounded-circle text-info", icon("box-archive", class = "fs-4")),
                div(
                  h6(class = "fw-bold text-dark mb-0", "Project Session Persistence"),
                  p(class = "text-muted small mb-0", "Save or restore the entire state (.fcarviz): context, lattice, implications, BMF & history log.")
                )
            ),
            div(class = "d-flex gap-2 align-items-center",
                downloadButton("btnSaveProjectBundle", "Save (.fcarviz)", class = "btn btn-info btn-sm text-white fw-semibold shadow-sm", icon = icon("floppy-disk")),
                tags$label(class = "btn btn-outline-secondary btn-sm fw-semibold mb-0 shadow-sm position-relative overflow-hidden cursor-pointer",
                    icon("folder-open", class = "me-1"), "Restore Project",
                    tags$input(id = "btnRestoreProjectFile", type = "file", accept = ".rds,.fcarviz", style = "position: absolute; font-size: 100px; right: 0; top: 0; opacity: 0; cursor: pointer;")
                )
            )
        )
      ),
      card(
        class = "border-0 shadow-sm p-4",
        uiOutput("datasetDocumentationUI")
      )
  )
)
