# ==============================================================================
# server.R - VERSIÓN 3.2 (FINAL: CONVERSATIONAL SUB-LATTICE & HIERARCHY)
# ==============================================================================

source("uiHome.R")
source("uiUploadData.R")
source("uiBasicOperations.R")
source("uiImplications.R")
source("uiConcepts.R")

server <- function(input, output, session) {

  # ===========================================================================
  # 1. CONFIGURACIÓN GENERAL Y NAVEGACIÓN
  # ===========================================================================
  observeEvent(input$main_nav, { nav_select("hidden_tabs", selected = input$main_nav) })

  observeEvent(input$theme_selector, {
    req(input$theme_selector)
    theme_name <- tolower(input$theme_selector)
    new_theme <- bs_theme_update(my_theme, bootswatch = theme_name)
    session$setCurrentTheme(new_theme)
    
    if (theme_name %in% c("darkly", "slate", "cyborg", "superhero")) {
      shinyjs::addClass(selector = "body", class = "dark-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "dark-mode")
    }
  })

  output$dataHeaderTitle <- renderText({ if(input$editMode) "Data Editor" else "Data Viewer" })

  # --- VARIABLES REACTIVAS ---
  vals <- reactiveValues(
    # Core Data
    fc = NULL,
    trigger = 0,
    stats_orig = NULL,
    filtered_imps = NULL,

    # Association rules (Concepts Lattice tab)
    assoc_rules_df = NULL,

    force_interactive_render = FALSE,
    concept_filter_ids = NULL,
    tab_concept_filter_ids = NULL,

    # Attribute Exploration State (ConExp)
    exploration_active = FALSE,
    exploration_complete = FALSE,
    confirmed_rules = character(0),
    refuted_rules = character(0),
    current_question = NULL,

    # Multivalued & Scaling State
    multivalued_df = NULL,
    current_doc = NULL,

    # Object Exploration State (Transposed ConExp)
    obj_exploration_active = FALSE,
    obj_exploration_complete = FALSE,
    obj_confirmed_rules = character(0),
    obj_refuted_rules = character(0),
    obj_current_question = NULL,
    fc_transposed = NULL,
    
    # Selected concept focal
    active_concept_id = 1,

    # Context table: arrow relations overlay (fcaR 1.7+)
    show_arrows = FALSE,

    # Lattice explorer overlay panel
    lattice_info_visible = FALSE,
    lattice_info_pinned = FALSE,
    lattice_info_title = NULL,
    lattice_info_body = NULL,
    lattice_info_kind = "concept",
    lattice_info_nonce = 0,
    lattice_edges_cache = NULL,
    active_edge_from = NULL,
    active_edge_to = NULL,
    active_edge_nonce = 0,
    
    # Inline refutation tracking for Ganter Attribute Exploration
    is_refuting = FALSE
  )

  # Background calculation state
  bg_calc <- reactiveValues(
    process = NULL,
    start_time = NULL,
    task_type = NULL # "concepts" or "implications"
  )

  # Polling observer for background processes
  observe({
    req(bg_calc$process)
    invalidateLater(1000) # Poll every 1 second
    
    # Calculate elapsed time
    elapsed <- round(as.numeric(difftime(Sys.time(), bg_calc$start_time, units = "secs")), 1)
    
    # Update notification with elapsed time
    showNotification(
      id = "bg_calc_notification",
      paste0("Computing ", bg_calc$task_type, "... Elapsed time: ", elapsed, " seconds. Please wait, R session remains responsive."),
      duration = NULL,
      closeButton = FALSE,
      type = "warning"
    )
    
    if (!bg_calc$process$is_alive()) {
      p <- bg_calc$process
      type <- bg_calc$task_type
      bg_calc$process <- NULL
      removeNotification("bg_calc_notification")
      
      status <- p$get_exit_status()
      if (!is.null(status) && status != 0) {
        shinyalert("Error", "The background calculation failed or ran out of memory. Try reducing context size.", type = "error")
        return()
      }
      
      tryCatch({
        res <- p$get_result()
        vals$fc <- res
        vals$trigger <- vals$trigger + 1
        
        if (type == "concepts") {
          shinyalert("Done!", "Concepts computed successfully.", type = "success")
        } else {
          # Update implications statistics & cached objects
          vals$stats_orig <- list(
            count = vals$fc$implications$cardinality(),
            size_lhs = sum(vals$fc$implications$get_LHS_matrix()),
            size_rhs = sum(vals$fc$implications$get_RHS_matrix())
          )
          vals$filtered_imps <- vals$fc$implications
          implications_df(get_implications_dataframe(vals$fc$implications))
          shinyalert("Done!", "Implications computed successfully.", type = "success")
        }
      }, error = function(e) {
        shinyalert("Error", paste("Failed to retrieve calculation result:", e$message), type = "error")
      })
    }
  })

  implications_df <- reactiveVal(NULL)

  sync_implications_ui <- function() {
    vals$stats_orig <- list(
      count = vals$fc$implications$cardinality(),
      size_lhs = sum(vals$fc$implications$get_LHS_matrix()),
      size_rhs = sum(vals$fc$implications$get_RHS_matrix())
    )
    vals$filtered_imps <- vals$fc$implications
    implications_df(get_implications_dataframe(vals$fc$implications))
    vals$trigger <- vals$trigger + 1
  }

  run_canonical_basis <- function(from_context = FALSE) {
    if (from_context) {
      safe_find_implications(vals$fc)
    } else {
      has_imps <- tryCatch(vals$fc$implications$cardinality() > 0, error = function(e) FALSE)
      if (has_imps) {
        vals$fc$implications <- vals$fc$implications$to_basis()
      } else {
        safe_find_implications(vals$fc)
      }
    }
    sync_implications_ui()
  }

  reset_context_derived_state <- function() {
    vals$stats_orig <- NULL
    vals$filtered_imps <- NULL
    implications_df(NULL)
  }

  # Reset force_interactive_render flag on any dataset/context change
  observeEvent(vals$trigger, {
    vals$force_interactive_render <- FALSE
  })

  # --- CARGA INICIAL (DESACTIVADA) ---
  # observe({
  #   if(is.null(vals$fc)){
  #     try({
  #       data("planets", package = "fcaR", envir = environment())
  #       if(exists("planets")){
  #         vals$fc <- FormalContext$new(planets)
  #         vals$trigger <- vals$trigger + 1
  #         shinyalert("Welcome to fcaRviz", "Default dataset loaded.", type = "info", closeOnEsc = TRUE)
  #       }
  #     }, silent = TRUE)
  #   }
  # })

  # --- ACTUALIZADOR DE INPUTS (DROPDOWNS) ---
  observe({
    vals$trigger; req(vals$fc)
    objs <- if(!is.null(vals$fc$objects)) vals$fc$objects else character(0)
    atts <- if(!is.null(vals$fc$attributes)) vals$fc$attributes else character(0)

    # Basic & Concepts Inputs
    updateSelectInput(session, "intentOptions", choices = objs)
    updateSelectInput(session, "extentOptions", choices = atts)
    updateSelectInput(session, "closureOptions", choices = atts)

    # Implications Inputs
    updateSelectInput(session, "selectLHS", choices = atts)
    updateSelectInput(session, "selectRHS", choices = atts)
    updateSelectInput(session, "selectNotLHS", choices = atts)
    updateSelectInput(session, "selectNotRHS", choices = atts)
    updateSelectInput(session, "selectClosure", choices = atts)
    updateSelectInput(session, "hypLHS", choices = atts)
    updateSelectInput(session, "hypRHS", choices = atts)

    # Concept Filters Inputs
    updateSelectizeInput(session, "filterObjs", choices = objs, server = TRUE)
    updateSelectizeInput(session, "filterAtts", choices = atts, server = TRUE)
    updateSelectizeInput(session, "filterObjsTab", choices = objs, server = TRUE)
    updateSelectizeInput(session, "filterAttsTab", choices = atts, server = TRUE)
  })

  # --- REACTIVE STATE FOR LOADED CONTEXT VIEW ---
  output$isContextLoaded <- reactive({
    !is.null(vals$fc)
  })
  outputOptions(output, "isContextLoaded", suspendWhenHidden = FALSE)

  # ===========================================================================
  # 2. GESTIÓN DE DATOS (UPLOAD, SAVE, EDITOR)
  # ===========================================================================
  
  # --- CONTEXT ORIGIN DROPDOWN LINKS ---
  observeEvent(input$optImportFile, {
    showModal(modalDialog(
      title = span(icon("file-arrow-up", class="text-primary"), " Import Formal Context or Load Project"),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      fileInput("file", "Select file (.csv, .rds, .cxt, .cex):", accept = c(".csv", ".rds", ".cxt", ".cex"),
                buttonLabel = "Load...", placeholder = "No file selected", width="100%"),
      helpText("Upload a full state (.rds), binary/multivalued table (.csv), CXT context (.cxt), or ConExp (.cex).")
    ))
  })

  observeEvent(input$optConnectRepo, {
    showModal(modalDialog(
      title = span(icon("github", class="text-primary"), " Connect with Repository"),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      selectInput("selectDataset", "Select Example Dataset from Repo:", choices = c("Loading available datasets..." = ""), width = "100%"),
      helpText("Datasets are being fetched from the remote repository. Please wait...")
    ))
    
    # Automatically initiate connection
    tryCatch({
      opts <- selectOptions()
      if (length(opts[[1]]) > 0 && opts[[1]][1] != "Error. Connection failed.") {
        # Format choices list with names and placeholder to prevent automatic load
        choices_list <- setNames(opts[[1]], opts[[2]])
        choices_list <- c("Select a dataset to load..." = "", choices_list)
        updateSelectInput(session, "selectDataset", choices = choices_list)
      } else {
        updateSelectInput(session, "selectDataset", choices = c("Error: Connection failed. Check internet connection." = ""))
      }
    }, error = function(e) {
      updateSelectInput(session, "selectDataset", choices = c("Error: Connection failed" = ""))
    })
  })

  observeEvent(input$optGenerateSynthetic, {
    showModal(modalDialog(
      title = div(class="d-flex justify-content-between align-items-center w-100",
                  span(icon("dice", class="text-primary"), " Synthetic Data Generators"),
                  actionLink("btnSyntheticInfo", "", icon = icon("info-circle"), style = "cursor: pointer; color: #0d6efd; font-size: 1.2em;", title = "Show synthetic generators info & references")
      ),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      
      selectInput("rand_type", "Generator / Perturbator:", 
                  choices = c("Random Context (Uniform/Dirichlet)" = "random_context",
                              "Random Distributive Context" = "random_distributive")),
      
      conditionalPanel(
        condition = "input.rand_type == 'random_context'",
        numericInput("rand_objs", "Number of Objects:", value = 10, min = 1, step = 1),
        numericInput("rand_atts", "Number of Attributes:", value = 8, min = 1, step = 1),
        sliderInput("rand_density", "Density:", min = 0.05, max = 0.95, value = 0.2, step = 0.05),
        selectInput("rand_distribution", "Distribution Method:", choices = c("uniform", "dirichlet")),
        conditionalPanel(
          condition = "input.rand_distribution == 'dirichlet'",
          numericInput("rand_alpha", "Alpha parameter:", value = 1.0, min = 0.1, step = 0.1)
        )
      ),
      
      conditionalPanel(
        condition = "input.rand_type == 'random_distributive'",
        numericInput("dist_elements", "Number of Elements:", value = 10, min = 2, step = 1),
        sliderInput("dist_density", "Density:", min = 0.05, max = 0.95, value = 0.2, step = 0.05)
      ),
      
      hr(),
      actionButton("btnGenerateSynthetic", "Generate & Load Context", class = "btn-info w-100 text-white fw-bold", icon = icon("dice"))
    ))
  })

  observeEvent(input$optCreateEmpty, {
    m <- matrix(0, nrow=3, ncol=3); rownames(m) <- c("Obj1","Obj2","Obj3"); colnames(m) <- c("Att1","Att2","Att3")
    vals$fc <- FormalContext$new(m); vals$trigger <- vals$trigger + 1; implications_df(NULL)
    vals$stats_orig <- NULL
    
    vals$current_doc <- list(
      title = "New Empty Context Template",
      description = "A blank 3x3 template initialized for manual grid editing.",
      source = "Manual Creation (fcaRviz UI Template)",
      creation_method = "Empty Template",
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      history_log = list(paste0("Created empty 3x3 context at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    )
    
    updateMaterialSwitch(session, "editMode", value = TRUE)
    shinyalert("New Context", "Created empty context and enabled Edit Mode.", type = "success")
  })

  observeEvent(input$btnRandomizeLoaded, {
    req(vals$fc)
    showModal(modalDialog(
      title = span(icon("dice", class="text-primary"), " Randomize Current Context"),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      selectInput("pert_method", "Randomization Method:", choices = c("swap", "rewire")),
      numericInput("pert_iterations", "Iterations (NULL for auto):", value = NA, min = 1, step = 1),
      hr(),
      actionButton("btnPerturbCurrent", "Apply Randomization", class = "btn-warning w-100 text-dark fw-bold", icon = icon("sync"))
    ))
  })

  observeEvent(input$file, {
    vals$current_doc <- NULL
    req(input$file); ext <- tolower(tools::file_ext(input$file$name))
    withProgress(message = 'Loading...', value = 0.5, {
      tryCatch({
        if (ext == "rds") {
          loaded_obj <- readRDS(input$file$datapath)
          vals$multivalued_df <- NULL
          if(inherits(loaded_obj, "FormalContext")) {
            vals$fc <- loaded_obj
            if(!is.null(vals$fc$implications) && vals$fc$implications$cardinality() > 0)
              implications_df(get_implications_dataframe(vals$fc$implications))
            removeModal()
            shinyalert("Project Loaded", "Context restored.", type = "success")
          } else { 
            vals$fc <- FormalContext$new(loaded_obj)
            removeModal()
            shinyalert("Data Loaded", "Matrix loaded.", type = "success") 
          }
        } else if (ext == "cxt") {
          clean_path <- file.path(tempdir(), paste0("upload_", as.numeric(Sys.time()), ".cxt"))
          lines <- readLines(input$file$datapath, warn = FALSE, encoding = "UTF-8")
          writeLines(lines, clean_path, useBytes = TRUE)
          vals$fc <- FormalContext$new(clean_path)
          vals$multivalued_df <- NULL
          removeModal()
          shinyalert("Success", "CXT loaded.", type = "success")
        } else if (ext == "cex") {
          mat <- parse_cex(input$file$datapath)
          vals$fc <- FormalContext$new(mat)
          vals$multivalued_df <- NULL
          removeModal()
          shinyalert("Success", "ConExp .cex file loaded successfully.", type = "success")
        } else {
          # Load and detect CSV column structures
          raw_df <- NULL
          try({
            raw_df <- read.csv(input$file$datapath, row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)
          }, silent = TRUE)
          if (is.null(raw_df) || ncol(raw_df) == 0) {
            try({
              raw_df <- read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
            }, silent = TRUE)
          }
          
          if (!is.null(raw_df) && ncol(raw_df) > 0) {
            is_bin <- all(sapply(raw_df, is_binary_vector))
            if (is_bin) {
              vals$fc <- FormalContext$new(input$file$datapath)
              vals$multivalued_df <- NULL
              removeModal()
              shinyalert("Success", "Binary CSV context loaded successfully.", type = "success")
            } else {
              vals$multivalued_df <- raw_df
              
              # Pre-render a safe nominal default context to prevent application crashes
              default_binary <- perform_conceptual_scaling(raw_df)
              vals$fc <- FormalContext$new(as.matrix(default_binary))
              
              removeModal()
              shinyalert("Multivalued Data Loaded", 
                         "This CSV contains non-binary numerical or categorical columns. Configure your scaling options in the 'Multivalued Scaling' tab to binarize it.", 
                         type = "info")
              
              nav_select("data_management_tabs", selected = "Multivalued Scaling")
            }
          } else {
            vals$fc <- FormalContext$new(input$file$datapath)
            vals$multivalued_df <- NULL
            removeModal()
            shinyalert("Success", "CSV context loaded.", type = "success")
          }
        }

        # Set current_doc metadata for imported files
        file_desc <- if (!is.null(vals$multivalued_df)) {
          paste0("A multivalued CSV dataset (", ncol(vals$multivalued_df), " columns) loaded. Requires binarization/scaling.")
        } else {
          paste0("A formal binary context with ", 
                 if(!is.null(vals$fc$objects)) length(vals$fc$objects) else 0, " objects and ",
                 if(!is.null(vals$fc$attributes)) length(vals$fc$attributes) else 0, " attributes.")
        }
        
        vals$current_doc <- list(
          title = paste0("Imported File: ", input$file$name),
          description = file_desc,
          source = paste0("Local file upload (", toupper(ext), ")"),
          creation_method = paste0(toupper(ext), " Import"),
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          history_log = list(paste0("Imported local file '", input$file$name, "' at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
        )

        vals$trigger <- vals$trigger + 1; vals$stats_orig <- NULL; vals$filtered_imps <- NULL
        if(ext != "rds") implications_df(NULL)
      }, error = function(e){ shinyalert("Error", e$message, type = "error") })
    })
  })

  # --- HANDLER PARA GENERACIÓN SINTÉTICA ---
  observeEvent(input$btnGenerateSynthetic, {
    vals$current_doc <- NULL
    req(input$rand_type)
    
    withProgress(message = "Generating synthetic context...", value = 0.5, {
      tryCatch({
        if (input$rand_type == "random_context") {
          req(input$rand_objs, input$rand_atts)
          
          fc_new <- fcaR::RandomContext(
            n_objects = input$rand_objs,
            n_attributes = input$rand_atts,
            density = input$rand_density,
            distribution = input$rand_distribution,
            alpha = if (input$rand_distribution == "dirichlet") input$rand_alpha else 1
          )
          
          vals$fc <- fc_new
          vals$trigger <- vals$trigger + 1
          
          vals$current_doc <- list(
            title = "Synthetic Random Context",
            description = paste0("Randomly generated binary relation context. Parameters: objects=", input$rand_objs, 
                                 ", attributes=", input$rand_atts, ", density=", input$rand_density, 
                                 ", distribution=", input$rand_distribution),
            source = "Generated synthetically inside fcaRviz",
            creation_method = "Synthetic Generation",
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            history_log = list(paste0("Generated random context (", input$rand_objs, "x", input$rand_atts, ") at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
          )
          
          reset_context_derived_state()
          removeModal()
          shinyalert("Success", "Synthetic Random Context generated & loaded successfully.", type = "success")
          
        } else if (input$rand_type == "random_distributive") {
          req(input$dist_elements)
          
          fc_new <- fcaR::RandomDistributiveContext(
            n_elements = input$dist_elements,
            density = input$dist_density
          )
          
          vals$fc <- fc_new
          vals$trigger <- vals$trigger + 1
          
          vals$current_doc <- list(
            title = "Synthetic Distributive Context",
            description = paste0("Randomly generated distributive lattice context. Elements=", input$dist_elements, 
                                 ", density=", input$dist_density),
            source = "Generated synthetically inside fcaRviz",
            creation_method = "Distributive Generation",
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            history_log = list(paste0("Generated random distributive context with ", input$dist_elements, " elements at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
          )
          
          reset_context_derived_state()
          removeModal()
          shinyalert("Success", "Random Distributive Context generated successfully.", type = "success")
        }
      }, error = function(e) {
        shinyalert("Generation Error", e$message, type = "error")
      })
    })
  })

  # --- HANDLER PARA PERTURBACIÓN / ALEATORIZACIÓN ---
  observeEvent(input$btnPerturbCurrent, {
    req(vals$fc, input$pert_method)
    
    withProgress(message = "Randomizing current context...", value = 0.5, {
      tryCatch({
        iters <- if (is.na(input$pert_iterations) || is.null(input$pert_iterations)) NULL else input$pert_iterations
        
        fc_new <- fcaR::randomize_context(
          fc = vals$fc,
          method = input$pert_method,
          iterations = iters
        )
        
        vals$fc <- fc_new
        vals$trigger <- vals$trigger + 1
        
        # Update current_doc history log
        current_history <- if (!is.null(vals$current_doc$history_log)) vals$current_doc$history_log else list()
        new_log_entry <- paste0("Perturbed context using method: ", input$pert_method, 
                                " (iterations: ", if(is.null(iters)) "auto" else iters, ") at ", 
                                format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        current_history <- c(current_history, new_log_entry)
        
        if (is.null(vals$current_doc)) {
          vals$current_doc <- list(
            title = "Perturbed/Randomized Context",
            description = paste0("A context generated by randomizing a previous state using method '", input$pert_method, "'."),
            source = "Context perturbation in fcaRviz",
            creation_method = "Randomization / Perturbation",
            timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            history_log = current_history
          )
        } else {
          vals$current_doc$history_log <- current_history
        }
        
        reset_context_derived_state()
        removeModal()
        shinyalert("Success", "Current context randomized and loaded successfully.", type = "success")
      }, error = function(e) {
        shinyalert("Perturbation Error", e$message, type = "error")
      })
    })
  })

  # --- OBSERVADOR PARA INFORMACIÓN DE GENERADORES SINTÉTICOS ---
  observeEvent(input$btnSyntheticInfo, {
    showModal(modalDialog(
      title = "Synthetic Data Generators Reference Guide",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <div style=\"font-family: 'Inter', sans-serif; line-height: 1.6;\">
          <h5 class=\"text-primary border-bottom pb-2\">Synthetic Data Generation & Perturbation</h5>
          
          <p><strong>1. Random Formal Contexts (Uniform vs. Dirichlet):</strong></p>
          <ul>
            <li><strong>Uniform Distribution:</strong> Generates incidence matrices where each cell has an independent, constant probability of being filled (determined by the density parameter). This yields standard, uncorrelated random structures.</li>
            <li><strong>Dirichlet Distribution:</strong> Mimics real-world datasets by generating attributes with unequal probabilities using a Dirichlet distribution. This models datasets where some attributes are highly frequent and others are rare, introducing more realistic correlation structures.</li>
          </ul>

          <p><strong>2. Random Distributive Contexts (Birkhoff's Theorem):</strong></p>
          <ul>
            <li><strong>Definition:</strong> Generates synthetic datasets that are mathematically guaranteed to produce distributive concept lattices. According to Birkhoff's representation theorem for distributive lattices, any finite distributive lattice is isomorphic to the lattice of down-sets of a partially ordered set (poset). The generator constructs a random poset and uses its order filter relations to populate the incidence matrix.</li>
          </ul>

          <p><strong>3. Context Randomization (Perturbation):</strong></p>
          <ul>
            <li><strong>Edge Swapping (swap):</strong> Swaps incidence values between random cells in a way that preserves the marginal sums (number of attributes per object and objects per attribute). This is useful for statistical significance tests, maintaining density and item frequencies while destroying patterns.</li>
            <li><strong>Rewiring (rewire):</strong> Randomly redistributes incidence values across the entire matrix while preserving the general density of the context.</li>
          </ul>

          <h5 class=\"text-primary border-bottom pb-2 mt-4\">Bibliographical References</h5>
          <div class=\"p-3 bg-light border rounded mb-3\">
            <ul>
              <li><strong>Birkhoff, G. (1937).</strong> <em>Rings of sets</em>. Duke Mathematical Journal, 3(3), 443-454.</li>
              <li><strong>Ganter, B., & Wille, R. (1999).</strong> <em>Formal Concept Analysis: Mathematical Foundations</em>. Springer-Verlag.</li>
              <li><strong>Snijders, T. A. B. (1991).</strong> <em>Enumeration and simulation methods for 0-1 matrices with given marginals</em>. Psychometrika, 56(3), 397-417.</li>
            </ul>
          </div>
        </div>
      ")
    ))
  })

  output$saveProject <- downloadHandler(
    filename = function() { paste0("fcaR_project_", format(Sys.time(), "%Y%m%d_%H%M"), ".rds") },
    content = function(file) { req(vals$fc); saveRDS(vals$fc, file) }
  )

  output$exportCex <- downloadHandler(
    filename = function() { paste0("fcaR_context_", format(Sys.time(), "%Y%m%d_%H%M"), ".cex") },
    content = function(file) { req(vals$fc); export_cex(vals$fc, file) }
  )

  output$hot_context <- renderRHandsontable({
    vals$trigger; req(vals$fc); mat <- t(as.matrix(vals$fc$I))
    grid_data <- matrix("", nrow = nrow(mat) + 1, ncol = ncol(mat) + 1)
    grid_data[1, 1] <- ""; grid_data[1, 2:ncol(grid_data)] <- colnames(mat)
    grid_data[2:nrow(grid_data), 1] <- rownames(mat)
    bin_data <- apply(mat, c(1,2), function(x) if(x > 0) "X" else "")
    grid_data[2:nrow(grid_data), 2:ncol(grid_data)] <- bin_data
    df_grid <- as.data.frame(grid_data, stringsAsFactors = FALSE)
    custom_renderer <- "function(instance, td, row, col, prop, value, cellProperties) { Handsontable.renderers.TextRenderer.apply(this, arguments); if (row === 0 || col === 0) { td.style.fontWeight = 'bold'; td.style.background = '#f0f0f0'; td.style.textAlign = 'center'; } else { td.style.textAlign = 'center'; } }"
    rhandsontable(df_grid, rowHeaders = NULL, colHeaders = NULL, width = "100%", height = "450px", useTypes = FALSE) %>% hot_cols(renderer = custom_renderer) %>% hot_context_menu(allowRowEdit=TRUE, allowColEdit=TRUE, allowInsertRow=TRUE, allowInsertCol=TRUE, allowRemoveRow=TRUE, allowRemoveCol=TRUE)
  })

  # Save changes automatically when disabling Edit Mode
  observeEvent(input$editMode, {
    if (isFALSE(input$editMode)) {
      req(input$hot_context, vals$fc)
      tryCatch({
        raw_data <- input$hot_context$data
        raw_matrix <- do.call(rbind, lapply(raw_data, function(x) { x[sapply(x, is.null)] <- ""; return(unlist(x)) }))
        new_atts <- as.character(raw_matrix[1, 2:ncol(raw_matrix)]); new_objs <- as.character(raw_matrix[2:nrow(raw_matrix), 1])
        if (any(new_atts == "") || any(is.na(new_atts))) {
          updateMaterialSwitch(session, "editMode", value = TRUE)
          stop("Headers cannot be empty.")
        }
        new_atts <- make.unique(new_atts); new_objs <- make.unique(new_objs)
        data_part <- raw_matrix[2:nrow(raw_matrix), 2:ncol(raw_matrix)]
        mat_bin <- matrix(0, nrow = length(new_objs), ncol = length(new_atts))
        for (i in 1:nrow(data_part)) { 
          for (j in 1:ncol(data_part)) { 
            val <- data_part[i, j]
            if (!is.na(val) && as.character(val) != "" && as.character(val) != "0") { 
              mat_bin[i, j] <- 1 
            } 
          } 
        }
        colnames(mat_bin) <- new_atts; rownames(mat_bin) <- new_objs
        
        current_mat <- t(as.matrix(vals$fc$I))
        if (!identical(current_mat, mat_bin)) {
          vals$fc <- FormalContext$new(mat_bin)
          vals$trigger <- vals$trigger + 1
          implications_df(NULL)
          vals$stats_orig <- NULL
          
          # Append manual edit to history log
          current_history <- if (!is.null(vals$current_doc$history_log)) vals$current_doc$history_log else list()
          new_log_entry <- paste0("Edited context manually (new dimensions: ", length(new_objs), "x", length(new_atts), ") at ", 
                                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
          current_history <- c(current_history, new_log_entry)
          
          if (is.null(vals$current_doc)) {
            vals$current_doc <- list(
              title = "Manually Edited Context",
              description = "A formal context created/modified manually by editing cell grid values.",
              source = "Direct User Grid Modification",
              creation_method = "Manual Editing",
              timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              history_log = current_history
            )
          } else {
            vals$current_doc$history_log <- current_history
          }
          
          shinyalert("Updated!", "Context updated.", type = "success")
        }
      }, error = function(e) {
        updateMaterialSwitch(session, "editMode", value = TRUE)
        shinyalert("Error updating", e$message, type = "error")
      })
    }
  })

  observeEvent(input$btnUpdateContext, {
    updateMaterialSwitch(session, "editMode", value = FALSE)
  })

  # Keep arrow-toggle state in sync for Basic Operations table
  observeEvent(input$showArrowRelations_basic, {
    vals$show_arrows <- isTRUE(input$showArrowRelations_basic)
  }, ignoreNULL = FALSE)

  context_table_dt <- function(fc, page_length = 15, show_arrows = FALSE) {
    d <- get_context_df(fc, show_arrows = show_arrows)
    DT::datatable(
      d,
      rownames = FALSE,
      options = list(scrollX = TRUE, pageLength = page_length, searching = FALSE),
      class = "cell-border stripe"
    )
  }

  output$contents <- DT::renderDT({
    vals$trigger
    req(vals$fc)
    context_table_dt(vals$fc, page_length = 10, show_arrows = FALSE)
  })
  outputOptions(output, "contents", suspendWhenHidden = FALSE)

  observeEvent(input$selectDataset, {
    req(input$selectDataset)
    if(input$selectDataset != "") {
      try({
        vals$fc <- returnFCFromRepo(input$selectDataset)
        vals$trigger <- vals$trigger + 1
        vals$stats_orig <- NULL
        vals$filtered_imps <- NULL
        implications_df(NULL)
        
        # Download and store metadata documentation
        meta_url <- "https://fcarepository.org/contexts.yaml"
        meta_data <- tryCatch(yaml::read_yaml(meta_url), error = function(e) NULL)
        if (!is.null(meta_data) && !is.null(meta_data[[input$selectDataset]])) {
          doc <- meta_data[[input$selectDataset]]
          doc$creation_method <- "Repository Import"
          doc$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          doc$history_log <- list(paste0("Loaded repository dataset '", doc$title, "' at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
          if (!is.null(doc$source)) {
            doc$source_citation <- doc$source
            doc$source <- "fcaRepository.org (Remote GitHub)"
          }
          vals$current_doc <- doc
        } else {
          vals$current_doc <- NULL
        }
        
        removeModal()
      })
    }
  })

  output$datasetDocumentationUI <- renderUI({
    if (is.null(vals$current_doc)) {
      return(div(
        class = "text-center py-5 text-muted",
        icon("book", class = "fa-4x mb-3 text-muted"),
        h5("No Documentation Available"),
        p("Please import a file, connect to the repository, or generate a context to view documentation/metadata.")
      ))
    }
    
    doc <- vals$current_doc
    
    # Calculate current dimensions dynamically from vals$fc
    cur_objs <- if(!is.null(vals$fc) && !is.null(vals$fc$objects)) length(vals$fc$objects) else 0
    cur_atts <- if(!is.null(vals$fc) && !is.null(vals$fc$attributes)) length(vals$fc$attributes) else 0
    
    # Metadata badges
    lang_badge <- if(!is.null(doc$language)) span(doc$language, class="badge bg-info text-white fs-6") else NULL
    objs_badge <- span(paste0(cur_objs, " objects"), class="badge bg-secondary fs-6")
    atts_badge <- span(paste0(cur_atts, " attributes"), class="badge bg-secondary fs-6")
    
    # History log list rendering
    history_items <- NULL
    if (!is.null(doc$history_log) && length(doc$history_log) > 0) {
      history_items <- tags$ul(
        class = "list-group list-group-flush border rounded mb-3",
        lapply(doc$history_log, function(item) {
          tags$li(class = "list-group-item py-2 px-3 small d-flex align-items-center gap-2",
                  icon("clock", class="text-muted"),
                  span(item))
        })
      )
    }
    
    tagList(
      div(class="border-bottom pb-3 mb-4",
          h3(class="fw-bold text-primary mb-1", if(!is.null(doc$title)) doc$title else "Dataset Metadata"),
          div(class="d-flex gap-2 mt-2", lang_badge, objs_badge, atts_badge)
      ),
      
      # Audit Grid Card
      card(
        class = "mb-4 border-0 bg-light shadow-sm",
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            div(
              h6(class="fw-bold text-uppercase text-muted small mb-1", "Creation Method"),
              p(class="text-dark fw-semibold mb-0", if(!is.null(doc$creation_method)) doc$creation_method else "N/A")
            ),
            div(
              h6(class="fw-bold text-uppercase text-muted small mb-1", "Source Origin"),
              p(class="text-dark mb-0 truncate-text", if(!is.null(doc$source)) doc$source else "N/A", style="max-width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
            ),
            div(
              h6(class="fw-bold text-uppercase text-muted small mb-1", "Creation Time"),
              p(class="text-dark mb-0", if(!is.null(doc$timestamp)) doc$timestamp else "N/A")
            )
          )
        )
      ),
      
      # Description Section
      if(!is.null(doc$description) && doc$description != "") {
        div(class="mb-4",
            h6(class="fw-bold text-uppercase text-muted small mb-2", "Description / Details"),
            p(class="fs-6 text-dark", doc$description)
        )
      } else NULL,
      
      # Bibliography Citation Section
      if(!is.null(doc$source_citation) && doc$source_citation != "") {
        div(class="mb-4",
            h6(class="fw-bold text-uppercase text-muted small mb-2", "Bibliographical Reference / Source"),
            div(class="p-3 bg-light border rounded text-dark fs-6",
                icon("quote-left", class="text-muted me-2"),
                tags$em(doc$source_citation)
            )
        )
      } else NULL,
      
      # History Log Timeline
      if(!is.null(history_items)) {
        div(class="mb-3",
            h6(class="fw-bold text-uppercase text-muted small mb-2", "Audit & History Log"),
            history_items
        )
      } else NULL
    )
  })


  # Dynamically show/hide Multivalued Scaling tab
  observe({
    if (is.null(vals$multivalued_df)) {
      hideTab(inputId = "data_management_tabs", target = "Multivalued Scaling")
    } else {
      showTab(inputId = "data_management_tabs", target = "Multivalued Scaling")
    }
  })


  # --- MULTIVALUED SCALING WIZARD SERVER LOGIC ---
  output$scalingWizardUI <- renderUI({
    req(vals$multivalued_df)
    df <- vals$multivalued_df
    cols <- colnames(df)
    
    ui_list <- lapply(cols, function(col) {
      col_data <- df[[col]]
      
      # Intelligent default strategy detection
      default_strategy <- "nominal"
      if (is_binary_vector(col_data)) {
        default_strategy <- "binary"
      } else if (is.numeric(col_data)) {
        default_strategy <- "ordinal"
      }
      
      # Calculate suggested thresholds if numeric
      sugg_thresholds <- ""
      if (is.numeric(col_data)) {
        quants <- quantile(col_data, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
        sugg_thresholds <- paste(round(unique(quants), 2), collapse = ", ")
      }
      
      # Premium row layout using bslib layout and cards
      card(
        class = "mb-3 p-3 bg-light border-0 shadow-sm",
        style = "border-radius: 8px;",
        div(
          class = "row align-items-center",
          div(
            class = "col-md-3",
            strong(col, class = "text-dark"),
            br(),
            span(class = "badge bg-secondary", 
                 if (is.numeric(col_data)) "Numeric" else if (is_binary_vector(col_data)) "Binary" else "Categorical")
          ),
          div(
            class = "col-md-3",
            selectInput(
              inputId = paste0("scale_type_", col),
              label = "Strategy:",
              choices = c("nominal", "ordinal", "interordinal", "binary", "ignore"),
              selected = default_strategy,
              width = "100%"
            )
          ),
          div(
            class = "col-md-6",
            # Conditional Panel for Thresholds
            conditionalPanel(
              condition = sprintf("input['scale_type_%s'] == 'ordinal' || input['scale_type_%s'] == 'interordinal'", col, col),
              div(
                class = "row",
                conditionalPanel(
                  condition = sprintf("input['scale_type_%s'] == 'ordinal'", col),
                  div(
                    class = "col-6",
                    selectInput(
                      inputId = paste0("scale_dir_", col),
                      label = "Direction:",
                      choices = c("<=" = "le", ">=" = "ge", "both" = "both"),
                      selected = "le",
                      width = "100%"
                    )
                  )
                ),
                div(
                  class = "col-6",
                  textInput(
                    inputId = paste0("scale_thresh_", col),
                    label = "Thresholds (comma-separated):",
                    value = sugg_thresholds,
                    placeholder = "e.g., 10, 20, 30",
                    width = "100%"
                  )
                )
              )
            ),
            conditionalPanel(
              condition = sprintf("input['scale_type_%s'] == 'nominal'", col),
              span(class = "text-muted small", "Nominal scaling will generate one binary attribute per unique category.")
            ),
            conditionalPanel(
              condition = sprintf("input['scale_type_%s'] == 'binary'", col),
              span(class = "text-muted small", "Binary values will be imported directly as-is.")
            ),
            conditionalPanel(
              condition = sprintf("input['scale_type_%s'] == 'ignore'", col),
              span(class = "text-danger small", "This column will be excluded from the formal context.")
            )
          )
        )
      )
    })
    
    do.call(tagList, ui_list)
  })

  observeEvent(input$btnPerformScaling, {
    req(vals$multivalued_df)
    df <- vals$multivalued_df
    cols <- colnames(df)
    config <- list()
    
    for (col in cols) {
      type <- input[[paste0("scale_type_", col)]]
      if (is.null(type)) next
      
      thresh_val <- input[[paste0("scale_thresh_", col)]]
      thresholds <- NULL
      if (!is.null(thresh_val) && nzchar(trimws(thresh_val))) {
        # Parse comma separated values
        parsed <- as.numeric(unlist(strsplit(thresh_val, ",")))
        thresholds <- parsed[!is.na(parsed)]
      }
      
      dir_val <- input[[paste0("scale_dir_", col)]]
      
      config[[col]] <- list(
        type = type,
        thresholds = thresholds,
        direction = dir_val
      )
    }
    
    withProgress(message = 'Scaling and importing context...', value = 0.5, {
      tryCatch({
        scaled_df <- perform_conceptual_scaling(df, config)
        
        # Convert to FormalContext
        scaled_mat <- as.matrix(scaled_df)
        vals$fc <- FormalContext$new(scaled_mat)
        vals$trigger <- vals$trigger + 1
        
        # Populate scaling metadata
        vals$current_doc <- list(
          title = paste0("Scaled Context: ", if(!is.null(vals$current_doc$title)) vals$current_doc$title else "Imported Dataset"),
          description = paste0("Created via Formal Conceptual Scaling of multivalued columns. Configured attributes scaled into ", ncol(scaled_df), " binary features."),
          source = if(!is.null(vals$current_doc$source)) vals$current_doc$source else "Conceptual scaling pipeline",
          creation_method = "Conceptual Scaling",
          timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          history_log = c(
            if(!is.null(vals$current_doc$history_log)) vals$current_doc$history_log else list(),
            paste0("Applied conceptual scaling on multivalued dataset to generate ", ncol(scaled_df), " attributes at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
          )
        )
        
        vals$stats_orig <- NULL
        vals$filtered_imps <- NULL
        implications_df(NULL)
        
        shinyalert("Success", "Conceptual scaling complete! Formal context updated.", type = "success")
        
        # Go back to Viewer tab
        nav_select("data_management_tabs", selected = "Context Viewer & Editor")
      }, error = function(e) {
        shinyalert("Scaling Error", e$message, type = "error")
      })
    })
  })

  # ===========================================================================
  # 3. OPERACIONES BÁSICAS
  # ===========================================================================
  output$tableData_DT <- DT::renderDT({
    vals$trigger
    vals$show_arrows
    req(vals$fc)
    context_table_dt(vals$fc, show_arrows = isTRUE(vals$show_arrows))
  })
  output$formalContext <- renderText({ vals$trigger; req(vals$fc); paste(capture.output(print(vals$fc)), collapse = "\n") })
  observeEvent(input$clarify, { req(vals$fc); vals$fc$clarify(); vals$trigger <- vals$trigger + 1; reset_context_derived_state() })
  observeEvent(input$reduce, { req(vals$fc); vals$fc$reduce(); vals$trigger <- vals$trigger + 1; reset_context_derived_state() })
  observeEvent(input$reduceArrows, {
    req(vals$fc)
    if (!has_arrow_relations_api(vals$fc)) {
      shinyalert("Not available", "reduce_arrows() requires fcaR >= 1.7.0.", type = "warning")
      return()
    }
    if (!is_binary_formal_context(vals$fc)) {
      shinyalert("Binary context required", "Arrow reduction only applies to binary formal contexts.", type = "warning")
      return()
    }
    tryCatch({
      n_objs_before <- length(vals$fc$objects)
      n_atts_before <- length(vals$fc$attributes)
      vals$fc <- vals$fc$reduce_arrows()
      vals$trigger <- vals$trigger + 1
      reset_context_derived_state()
      shinyalert(
        "Context reduced",
        sprintf(
          "Arrow-based reduction: %d → %d objects, %d → %d attributes.",
          n_objs_before, length(vals$fc$objects),
          n_atts_before, length(vals$fc$attributes)
        ),
        type = "success"
      )
    }, error = function(e) shinyalert("Reduce arrows failed", conditionMessage(e), type = "error"))
  })
  output$intentResult <- renderUI({ req(input$intentOptions, vals$fc); S <- Set$new(vals$fc$objects); sapply(input$intentOptions, function(x) do.call(S$assign, setNames(list(1), x))); res <- vals$fc$intent(S); HTML(paste("<pre>", paste(capture.output(print(res)), collapse = "\n"), "</pre>")) })
  output$extentResult <- renderUI({ req(input$extentOptions, vals$fc); S <- Set$new(vals$fc$attributes); sapply(input$extentOptions, function(x) do.call(S$assign, setNames(list(1), x))); res <- vals$fc$extent(S); HTML(paste("<pre>", paste(capture.output(print(res)), collapse = "\n"), "</pre>")) })
  output$closureResult <- renderUI({ req(input$closureOptions, vals$fc); S <- Set$new(vals$fc$attributes); sapply(input$closureOptions, function(x) do.call(S$assign, setNames(list(1), x))); res <- vals$fc$closure(S); HTML(paste("<pre>", paste(capture.output(print(res)), collapse = "\n"), "</pre>")) })
  observeEvent(input$createLatex, { showModal(modalDialog(title = "LaTeX Code", verbatimTextOutput("latexTable"), easyClose = TRUE)) })
  output$latexTable <- renderText({ req(vals$fc); vals$fc$to_latex() })
  output$downloadRds <- downloadHandler(filename = function() { "context.rds" }, content = function(file) { vals$fc$save(filename = "./fc.rds"); file.copy("./fc.rds", file) })
  output$downloadReport <- downloadHandler(filename = function() { paste("fca_report_", Sys.Date(), ".html", sep="") }, content = function(file) { req(vals$fc); id <- showNotification("Generating...", duration = NULL, closeButton = FALSE); on.exit(removeNotification(id), add = TRUE); tempReport <- file.path(tempdir(), "report.Rmd"); if(!file.exists("report.Rmd")) { writeLines(c("---", "title: 'Report'", "output: html_document", "---", "# Report", "Context loaded."), tempReport) } else { file.copy("report.Rmd", tempReport, overwrite = TRUE) }; rmarkdown::render(tempReport, output_file = file, params = list(fc = vals$fc), envir = new.env(parent = globalenv())) })

  # ===========================================================================
  # 4. CONCEPTOS Y RETÍCULO
  # ===========================================================================
  observeEvent(input$main_nav, {
    if (input$main_nav == "ui_concepts") {
      vals$trigger
      req(vals$fc)
      ready <- tryCatch(!vals$fc$concepts$is_empty(), error = function(e) FALSE)
      if (!ready) {
        if (isTRUE(input$ask_before_calc)) {
          showModal(modalDialog(
            title = "Compute Concepts?",
            "The concept lattice has not been computed yet for this context.",
            easyClose = TRUE,
            footer = tagList(
              input_task_button("getConcepts", "Compute Now"),
              actionButton("goBackConc", "Cancel")
            )
          ))
        } else {
          # Calculate automatically in background
          bg_calc$start_time <- Sys.time()
          bg_calc$task_type <- "concepts"
          bg_calc$process <- callr::r_bg(function(fc) {
            library(fcaR)
            fc$find_concepts()
            return(fc)
          }, args = list(fc = vals$fc))
        }
      }
    }
  })
  observeEvent(input$getConcepts, {
    req(vals$fc)
    removeModal()
    bg_calc$start_time <- Sys.time()
    bg_calc$task_type <- "concepts"
    bg_calc$process <- callr::r_bg(function(fc) {
      library(fcaR)
      fc$find_concepts()
      return(fc)
    }, args = list(fc = vals$fc))
  })
  observeEvent(input$goBackConc, { updateRadioGroupButtons(session, "main_nav", selected = "basic_operations"); removeModal() })

  observeEvent(input$btnForceRender, {
    req(vals$fc)
    vals$force_interactive_render <- TRUE
  })

  # --- ACTIVE SELECTED CONCEPT ID STATE & SYNCHRONIZATION ---
  vals$active_concept_id <- 1
  vals$navigation_history <- NULL
  vals$original_nodes_df <- NULL

  # Cache original styles whenever FormalContext is updated
  observeEvent(vals$fc, {
    req(vals$fc)
    if (!tryCatch(vals$fc$concepts$is_empty(), error = function(e) TRUE)) {
      n <- vals$fc$concepts$size()
      thresh <- 800
      if (!is.null(input$enable_threshold)) {
        thresh <- if (isTRUE(input$enable_threshold)) (if (!is.null(input$safety_threshold)) input$safety_threshold else 800) else Inf
      }
      if (n > 0 && n <= thresh) {
        try({
          g_data <- getGraph(vals$fc$concepts, vals$fc)
          vals$original_nodes_df <- data.frame(
            id = as.character(g_data$nodes$id),
            color.background = g_data$nodes$color.background,
            color.border = g_data$nodes$color.border,
            size = rep(22, nrow(g_data$nodes)),
            borderWidth = if ("borderWidth" %in% names(g_data$nodes)) g_data$nodes$borderWidth else 1,
            stringsAsFactors = FALSE
          )
        }, silent = TRUE)
      } else {
        vals$original_nodes_df <- NULL
      }
    } else {
      vals$original_nodes_df <- NULL
    }
  })

  # --- CONCEPT FILTERING LOGIC ---
  # Reset filters on new context or concepts computed
  observeEvent(vals$trigger, {
    vals$concept_filter_ids <- NULL
    updateSelectizeInput(session, "filterObjs", selected = character(0))
    updateSelectizeInput(session, "filterAtts", selected = character(0))
  })

  # Helper function to calculate filtered concept IDs
  calculate_filtered_concept_ids <- function(fc, sel_objs, sel_atts) {
    req(fc)
    if (tryCatch(fc$concepts$is_empty(), error = function(e) TRUE)) return(NULL)
    
    n_concepts <- fc$concepts$size()
    if (n_concepts == 0) return(integer(0))
    
    # Start with all concept IDs
    matching_ids <- 1:n_concepts
    
    # If no filters applied, return all IDs
    if (length(sel_objs) == 0 && length(sel_atts) == 0) {
      return(matching_ids)
    }
    
    # Get extents and intents matrices
    exts <- as.matrix(fc$concepts$extents())
    ints <- as.matrix(fc$concepts$intents())
    
    # Rownames of exts are object names, rownames of ints are attribute names
    rownames(exts) <- fc$objects
    rownames(ints) <- fc$attributes
    
    keep_by_objs <- rep(TRUE, n_concepts)
    if (length(sel_objs) > 0) {
      # Keep concepts where all selected objects are present in the extent
      keep_by_objs <- colSums(exts[sel_objs, , drop = FALSE]) == length(sel_objs)
    }
    
    keep_by_atts <- rep(TRUE, n_concepts)
    if (length(sel_atts) > 0) {
      # Keep concepts where all selected attributes are present in the intent
      keep_by_atts <- colSums(ints[sel_atts, , drop = FALSE]) == length(sel_atts)
    }
    
    matching_ids <- which(keep_by_objs & keep_by_atts)
    return(matching_ids)
  }

  # Apply button
  observeEvent(input$btnApplyConceptFilters, {
    req(vals$fc)
    if (tryCatch(vals$fc$concepts$is_empty(), error = function(e) TRUE)) {
      shinyalert("Oops!", "Please compute concepts first.", type = "warning")
      return()
    }
    
    # Calculate filtered IDs
    filtered <- calculate_filtered_concept_ids(vals$fc, input$filterObjs, input$filterAtts)
    vals$concept_filter_ids <- filtered
    vals$tab_concept_filter_ids <- filtered
    
    # Synchronize selectize inputs in the other tab
    updateSelectizeInput(session, "filterObjsTab", choices = vals$fc$objects, selected = input$filterObjs)
    updateSelectizeInput(session, "filterAttsTab", choices = vals$fc$attributes, selected = input$filterAtts)
    
    # Auto-focus synchronization
    if (!is.null(filtered) && length(filtered) > 0) {
      curr_active <- vals$active_concept_id
      if (!(curr_active %in% filtered)) {
        # Find the one with maximum extent size (largest extent colSum)
        exts <- as.matrix(vals$fc$concepts$extents())
        filtered_sizes <- colSums(exts[, filtered, drop = FALSE])
        new_active <- filtered[which.max(filtered_sizes)]
        
        vals$active_concept_id <- new_active
        # Update the visNetwork selected node if in macro mode
        if (isTRUE(input$exploration_mode == "macro")) {
          visNetwork::visNetworkProxy("interactivePlot") %>%
            visNetwork::visSelectNodes(id = as.character(new_active))
        } else {
          # micro mode: update the center concept
          updateNumericInput(session, "center_concept_id", value = new_active)
        }
      }
    }
  })

  # Clear button
  observeEvent(input$btnClearConceptFilters, {
    req(vals$fc)
    updateSelectizeInput(session, "filterObjs", choices = vals$fc$objects, selected = character(0))
    updateSelectizeInput(session, "filterAtts", choices = vals$fc$attributes, selected = character(0))
    updateSelectizeInput(session, "filterObjsTab", choices = vals$fc$objects, selected = character(0))
    updateSelectizeInput(session, "filterAttsTab", choices = vals$fc$attributes, selected = character(0))
    vals$concept_filter_ids <- NULL
    vals$tab_concept_filter_ids <- NULL
  })

  # Reactive to get the current filtered IDs
  current_filtered_ids <- reactive({
    req(vals$fc)
    if (tryCatch(vals$fc$concepts$is_empty(), error = function(e) TRUE)) return(NULL)
    
    n_concepts <- vals$fc$concepts$size()
    if (isTRUE(input$bypassFilters) || is.null(vals$concept_filter_ids)) {
      return(1:n_concepts)
    } else {
      return(vals$concept_filter_ids)
    }
  })

  # Render status badge
  output$filterStatusBadge <- renderText({
    req(vals$fc)
    if (tryCatch(vals$fc$concepts$is_empty(), error = function(e) TRUE)) return("")
    
    n_total <- vals$fc$concepts$size()
    if (isTRUE(input$bypassFilters)) {
      return(paste("Showing all", n_total, "concepts (Bypass active)"))
    }
    if (is.null(vals$concept_filter_ids)) {
      return(paste("Showing all", n_total, "concepts"))
    } else {
      n_filtered <- length(vals$concept_filter_ids)
      return(paste("Showing", n_filtered, "of", n_total, "concepts"))
    }
  })

  observeEvent(input$interactivePlot_selected, {
    req(vals$fc)
    n <- vals$fc$concepts$size()
    
    # Selection cleared: Restore original styles
    if (is.null(input$interactivePlot_selected) || input$interactivePlot_selected == "") {
      if (!is.null(vals$original_nodes_df) && isTRUE(input$exploration_mode == "macro")) {
        try({
          # Get current filtered ids
          curr_ids <- as.character(current_filtered_ids())
          df_to_update <- vals$original_nodes_df[vals$original_nodes_df$id %in% curr_ids, ]
          if (nrow(df_to_update) > 0) {
            visNetwork::visNetworkProxy("interactivePlot") %>%
              visNetwork::visUpdateNodes(nodes = df_to_update)
          }
        }, silent = TRUE)
      }
      return()
    }
    
    # Node selected
    C_0 <- as.numeric(input$interactivePlot_selected)
    req(C_0 > 0 && C_0 <= n)
    vals$active_concept_id <- C_0
    show_lattice_node_info(C_0, pin = TRUE)
    
    # Compute and highlight Ideal & Filter (ConExp style)
    thresh <- 800
    if (!is.null(input$enable_threshold)) {
      thresh <- if (isTRUE(input$enable_threshold)) (if (!is.null(input$safety_threshold)) input$safety_threshold else 800) else Inf
    }
    if (isTRUE(input$exploration_mode == "macro") && n <= thresh) {
      try({
        intents_mat <- as.matrix(vals$fc$concepts$intents())
        c_int <- intents_mat[, C_0]
        
        # Filter: superconcepts (intent(C) subset of intent(C_0))
        is_super <- colSums(intents_mat & !c_int) == 0
        filter_ids <- which(is_super)
        
        # Ideal: subconcepts (intent(C_0) subset of intent(C))
        is_sub <- colSums(c_int & !intents_mat) == 0
        ideal_ids <- which(is_sub)
        
        # Preserve the active color palette (irreducibles or metric).  Ideal,
        # filter and selection are indicated with borders and size only.
        node_ids <- as.character(seq_len(n))
        base_styles <- vals$original_nodes_df
        if (is.null(base_styles) || nrow(base_styles) == 0) {
          base_graph <- getGraph(
            vals$fc$concepts,
            vals$fc,
            filter_ids = current_filtered_ids(),
            scale_support = isTRUE(input$scale_node_support),
            labeling_mode = if (!is.null(input$labeling_mode)) input$labeling_mode else "reduced",
            color_mode = if (!is.null(input$color_mode)) input$color_mode else "irreducibles",
            metrics = tryCatch(concepts_metrics(), error = function(e) NULL)
          )
          base_styles <- data.frame(
            id = as.character(base_graph$nodes$id),
            color.background = base_graph$nodes$color.background,
            color.border = base_graph$nodes$color.border,
            size = if ("size" %in% names(base_graph$nodes)) base_graph$nodes$size else 22,
            borderWidth = if ("borderWidth" %in% names(base_graph$nodes)) base_graph$nodes$borderWidth else 1,
            stringsAsFactors = FALSE
          )
        }
        cached_idx <- match(node_ids, base_styles$id)
        bg_colors <- base_styles$color.background[cached_idx]
        bg_colors[is.na(bg_colors)] <- "rgba(220, 220, 220, 0.25)"
        border_colors <- base_styles$color.border[cached_idx]
        border_colors[is.na(border_colors)] <- "#666666"
        sizes <- base_styles$size[cached_idx]
        sizes[is.na(sizes)] <- 22
        border_widths <- rep(1, n)
        
        # Filter (Blue)
        border_colors[filter_ids] <- "#2E86C1"
        border_widths[filter_ids] <- 2
        
        # Ideal (Green)
        border_colors[ideal_ids] <- "#239B56"
        border_widths[ideal_ids] <- 2
        
        # Selected Concept (Red/Orange)
        border_colors[C_0] <- "#922B21"
        sizes[C_0] <- max(sizes[C_0] + 5, 27)
        border_widths[C_0] <- 4
        
        nodes_df <- data.frame(
          id = node_ids,
          color.background = bg_colors,
          color.border = border_colors,
          size = sizes,
          borderWidth = border_widths,
          stringsAsFactors = FALSE
        )
        
        # Subset nodes_df to only include current filtered concept IDs
        curr_ids <- as.character(current_filtered_ids())
        nodes_df <- nodes_df[nodes_df$id %in% curr_ids, ]
        
        visNetwork::visNetworkProxy("interactivePlot") %>%
          visNetwork::visUpdateNodes(nodes = nodes_df)
      }, silent = TRUE)
    }
  })
  
  observeEvent(input$clicked_macro_node, {
    req(input$clicked_macro_node)
    node_id <- parse_lattice_node_id(input$clicked_macro_node)
    req(!is.na(node_id))
    vals$active_concept_id <- node_id
    show_lattice_node_info(node_id, pin = TRUE)
    bslib::accordion_panel_open("lattice_sidebar_accordion", "Selected Concept")
  })

  observeEvent(input$clicked_micro_node, {
    req(input$clicked_micro_node)
    new_id <- parse_lattice_node_id(input$clicked_micro_node)
    req(!is.na(new_id))
    vals$active_concept_id <- new_id
    updateNumericInput(session, "center_concept_id", value = new_id)
    show_lattice_node_info(new_id, pin = TRUE)
    bslib::accordion_panel_open("lattice_sidebar_accordion", "Selected Concept")
  })
  
  observeEvent(input$center_concept_id, {
    req(input$center_concept_id)
    if (isTRUE(input$exploration_mode == "micro")) {
      vals$active_concept_id <- as.numeric(input$center_concept_id)
    }
  })

  observeEvent(input$exploration_mode, {
    req(input$exploration_mode)
    if (input$exploration_mode == "micro") {
      curr_active <- if (!is.null(vals$active_concept_id)) vals$active_concept_id else 1
      updateNumericInput(session, "center_concept_id", value = curr_active)
    } else if (input$exploration_mode == "macro") {
      curr_active <- if (!is.null(vals$active_concept_id)) vals$active_concept_id else 1
      # Programmatically select the active node in macro view
      try({
        visNetwork::visNetworkProxy("interactivePlot") %>%
          visNetwork::visSelectNodes(id = as.character(curr_active))
      }, silent = TRUE)
    }
  })

  observeEvent(vals$active_concept_id, {
    req(vals$active_concept_id, vals$fc)
    n <- vals$fc$concepts$size()
    req(vals$active_concept_id > 0 && vals$active_concept_id <= n)
    
    if (is.null(vals$navigation_history)) {
      vals$navigation_history <- vals$active_concept_id
    } else {
      last <- tail(vals$navigation_history, 1)
      if (last != vals$active_concept_id) {
        vals$navigation_history <- c(vals$navigation_history, vals$active_concept_id)
      }
    }
  })

  observeEvent(input$btnClearHistory, {
    vals$navigation_history <- vals$active_concept_id
  })

  observeEvent(input$jump_concept_id, {
    req(input$jump_concept_id)
    new_id <- as.numeric(input$jump_concept_id)
    vals$active_concept_id <- new_id
    if (isTRUE(input$exploration_mode == "micro")) {
      updateNumericInput(session, "center_concept_id", value = new_id)
    } else {
      visNetwork::visNetworkProxy("interactivePlot") %>%
        visNetwork::visSelectNodes(id = as.character(new_id))
    }
  })

  # --- LATTICE INFO PANEL & VIEW CONTROLS ---
  set_lattice_info_panel <- function(title, body_html, kind = "concept", pin = FALSE) {
    vals$lattice_info_title <- title
    vals$lattice_info_body <- body_html
    vals$lattice_info_kind <- kind
    vals$lattice_info_visible <- TRUE
    vals$lattice_info_pinned <- isTRUE(pin)
    vals$lattice_info_nonce <- vals$lattice_info_nonce + 1L
    invisible(NULL)
  }

  clear_lattice_info_panel <- function() {
    vals$lattice_info_visible <- FALSE
    vals$lattice_info_pinned <- FALSE
    vals$lattice_info_title <- NULL
    vals$lattice_info_body <- NULL
    vals$lattice_info_kind <- "concept"
    vals$lattice_info_nonce <- vals$lattice_info_nonce + 1L
    invisible(NULL)
  }

  show_lattice_node_info <- function(node_id, pin = FALSE) {
    if (is.null(vals$fc) || is.null(node_id)) return(invisible(NULL))
    node_id <- as.numeric(node_id)
    n <- vals$fc$concepts$size()
    if (is.na(node_id) || node_id < 1 || node_id > n) return(invisible(NULL))

    dt <- tryCatch(
      get_concepts_dataframe(vals$fc$concepts$sub(node_id), vals$fc),
      error = function(e) get_concepts_dataframe(vals$fc$concepts[node_id], vals$fc)
    )
    col_mode <- if (!is.null(input$color_mode)) input$color_mode else "irreducibles"
    met <- tryCatch(concepts_metrics(), error = function(e) NULL)

    labels_val <- ""
    status <- tryCatch(get_concept_irreducible_status(node_id, vals$fc$concepts, vals$fc), error = function(e) NULL)
    if (!is.null(status) && length(status$introduced_attributes) > 0) {
      labels_val <- paste(status$introduced_attributes, collapse = ", ")
    } else if (nrow(dt) > 0 && nzchar(dt$attributes[1])) {
      labels_val <- dt$attributes[1]
    }

    body_html <- get_concept_tooltip(
      node_id, labels_val, dt$objects[1], dt$attributes[1], col_mode, met, concepts = vals$fc$concepts
    )
    set_lattice_info_panel(
      title = paste0("C", node_id),
      body_html = body_html,
      kind = "concept",
      pin = pin
    )
    vals$active_edge_from <- NULL
    vals$active_edge_to <- NULL
    vals$active_edge_nonce <- vals$active_edge_nonce + 1L
  }

  show_lattice_edge_info <- function(edge_key = NULL, edge_from = NULL, edge_to = NULL, pin = FALSE) {
    if (is.null(vals$fc)) return(invisible(NULL))

    from_id <- if (!is.null(edge_from)) as.character(edge_from) else NULL
    to_id <- if (!is.null(edge_to)) as.character(edge_to) else NULL

    if ((is.null(from_id) || !nzchar(from_id) || is.null(to_id) || !nzchar(to_id)) &&
        !is.null(vals$lattice_edges_cache) && nrow(vals$lattice_edges_cache) > 0) {
      edges <- vals$lattice_edges_cache
      idx <- integer(0)
      if (!is.null(edge_key) && nzchar(as.character(edge_key))) {
        edge_key <- as.character(edge_key)
        idx <- which(
          paste0("e:", edges$from, ":", edges$to) == edge_key |
            paste(edges$from, edges$to, sep = "-") == edge_key |
            paste(edges$from, edges$to, sep = "|") == edge_key |
            as.character(edges$id) == edge_key
        )
      }
      if (length(idx) > 0) {
        row <- edges[idx[1], , drop = FALSE]
        from_id <- as.character(row$from)
        to_id <- as.character(row$to)
      }
    }

    if (is.null(from_id) || !nzchar(from_id) || is.null(to_id) || !nzchar(to_id)) {
      return(invisible(NULL))
    }

    title_txt <- paste0("C", from_id, " \u2192 C", to_id)
    body_html <- build_lattice_association_rule_html(from_id, to_id, vals$fc)

    set_lattice_info_panel(
      title = title_txt,
      body_html = body_html,
      kind = "edge",
      pin = pin
    )
    vals$active_edge_from <- from_id
    vals$active_edge_to <- to_id
    vals$active_edge_nonce <- vals$active_edge_nonce + 1L
  }

  lattice_viz_control <- function(action) {
    session$sendCustomMessage("latticeVizControl", list(action = action))
  }

  observeEvent(input$latticeZoomIn, { lattice_viz_control("zoomIn") })
  observeEvent(input$latticeZoomOut, { lattice_viz_control("zoomOut") })
  observeEvent(input$latticeFit, { lattice_viz_control("fit") })
  observeEvent(input$latticeMoveUp, { lattice_viz_control("moveUp") })
  observeEvent(input$latticeMoveDown, { lattice_viz_control("moveDown") })
  observeEvent(input$latticeMoveLeft, { lattice_viz_control("moveLeft") })
  observeEvent(input$latticeMoveRight, { lattice_viz_control("moveRight") })
  observeEvent(input$btnLatticeTrueFullscreen, {
    session$sendCustomMessage("latticeToggleFullscreen", list())
  }, ignoreInit = TRUE)

  observeEvent(input$btnCloseLatticeInfo, {
    clear_lattice_info_panel()
  }, ignoreNULL = TRUE)

  parse_lattice_node_id <- function(x) {
    if (is.null(x)) return(NA_real_)
    if (is.list(x) && !is.null(x$id)) return(as.numeric(x$id))
    as.numeric(x)
  }

  parse_lattice_edge_event <- function(x) {
    if (is.null(x)) {
      return(list(key = NULL, from = NULL, to = NULL))
    }
    if (is.list(x) || (is.vector(x) && !is.null(names(x)))) {
      key <- x[["key"]]
      if (is.null(key)) key <- x[["id"]]
      from <- x[["from"]]
      to <- x[["to"]]
      return(list(
        key = if (!is.null(key)) as.character(key) else NULL,
        from = if (!is.null(from)) as.character(from) else NULL,
        to = if (!is.null(to)) as.character(to) else NULL
      ))
    }
    list(key = as.character(x), from = NULL, to = NULL)
  }

  observeEvent(input$lattice_hover_node, {
    if (isTRUE(vals$lattice_info_pinned)) return()
    node_id <- parse_lattice_node_id(input$lattice_hover_node)
    if (is.na(node_id)) return()
    show_lattice_node_info(node_id, pin = FALSE)
  }, ignoreNULL = FALSE)

  observeEvent(input$lattice_hover_edge, {
    if (isTRUE(vals$lattice_info_pinned)) return()
    edge <- parse_lattice_edge_event(input$lattice_hover_edge)
    if ((is.null(edge$from) || !nzchar(edge$from) || is.null(edge$to) || !nzchar(edge$to)) &&
        (is.null(edge$key) || !nzchar(edge$key))) {
      return()
    }
    show_lattice_edge_info(
      edge_key = edge$key,
      edge_from = edge$from,
      edge_to = edge$to,
      pin = FALSE
    )
  }, ignoreNULL = FALSE)

  observeEvent(input$lattice_click_node, {
    node_id <- parse_lattice_node_id(input$lattice_click_node)
    req(!is.na(node_id))
    vals$active_concept_id <- node_id
    show_lattice_node_info(node_id, pin = TRUE)
    if (isTRUE(input$exploration_mode == "micro")) {
      if (is.null(input$center_concept_id) || as.numeric(input$center_concept_id) != node_id) {
        updateNumericInput(session, "center_concept_id", value = node_id)
      }
    }
    bslib::accordion_panel_open("lattice_sidebar_accordion", "Selected Concept")
  }, ignoreNULL = TRUE)

  observeEvent(input$lattice_click_edge, {
    edge <- parse_lattice_edge_event(input$lattice_click_edge)
    req(
      (!is.null(edge$from) && nzchar(edge$from) && !is.null(edge$to) && nzchar(edge$to)) ||
        (!is.null(edge$key) && nzchar(edge$key))
    )
    show_lattice_edge_info(
      edge_key = edge$key,
      edge_from = edge$from,
      edge_to = edge$to,
      pin = TRUE
    )
    bslib::accordion_panel_open("lattice_sidebar_accordion", "Association Rule")
  }, ignoreNULL = TRUE)

  output$latticeNodeInfoPanel <- renderUI({
    vals$lattice_info_nonce
    if (!isTRUE(vals$lattice_info_visible)) return(NULL)
    req(vals$lattice_info_title, vals$lattice_info_body)
    build_lattice_info_panel(
      title = vals$lattice_info_title,
      body_html = vals$lattice_info_body,
      kind = if (!is.null(vals$lattice_info_kind)) vals$lattice_info_kind else "concept"
    )
  })
  outputOptions(output, "latticeNodeInfoPanel", suspendWhenHidden = FALSE)

  # --- RENDER LATTICE CONTAINER ---
  output$latticeContainer <- renderUI({
    vals$trigger
    req(vals$fc)
    if (tryCatch(vals$fc$concepts$is_empty(), error = function(e) TRUE)) return(NULL)
    
    n_total <- vals$fc$concepts$size()
    curr_ids <- current_filtered_ids()
    n_filtered <- if (is.null(curr_ids)) n_total else length(curr_ids)
    
    style_tag <- tags$style(HTML("
      #latticeContainer {
        height: 100% !important;
        display: flex;
        flex-direction: column;
        flex: 1 1 auto;
        min-height: 0 !important;
      }
      #interactivePlot {
        height: 100% !important;
        flex: 1 1 auto;
        min-height: 0 !important;
      }
      .shiny-spinner-placeholder {
        height: 100% !important;
        flex: 1 1 auto;
        display: flex;
        flex-direction: column;
        min-height: 0 !important;
      }
    "))
    
    if (isTRUE(input$exploration_mode == "micro")) {
      return(tagList(style_tag, visNetworkOutput("interactivePlot", height = "100%")))
    }
    
    if (n_filtered > 5000) {
      return(div(class="alert alert-danger", 
                 tags$h4(tags$strong("⚠️ Visual Bottleneck: Lattice Too Large")),
                 p(paste("The filtered lattice contains", n_filtered, "concepts, which exceeds the absolute safety threshold of 5000 nodes.")),
                 p("To protect your browser from freezing, the full macro visualization is locked."),
                 p(class="fw-bold text-success", "However, you can explore the lattice safely and instantly using the Micro Explorer!"),
                 tags$ul(
                   tags$li("Switch to ", tags$strong("Micro Explorer"), " mode at the top right of this card."),
                   tags$li("Explore neighborhoods locally by clicking on parent and child nodes.")
                 )
             ))
    }
    
    thresh <- 800
    if (!is.null(input$enable_threshold)) {
      thresh <- if (isTRUE(input$enable_threshold)) (if (!is.null(input$safety_threshold)) input$safety_threshold else 800) else Inf
    }
    if (n_filtered > thresh) {
      if (vals$force_interactive_render) {
        return(tagList(style_tag, visNetworkOutput("interactivePlot", height = "100%")))
      } else {
        return(tagList(
          div(class="alert alert-warning",
              tags$h4(tags$strong("⚠️ Warning: Large Lattice Detected")),
              p(paste("This concept lattice contains", n_filtered, "concepts (filtered). Rendering the full interactive lattice automatically may cause your browser to lag or freeze (Visual Bottleneck).")),
              p("To explore this data safely, we recommend:"),
              tags$ul(
                tags$li("Reducing the formal context using ", tags$strong("Clarify"), " or ", tags$strong("Reduce"), " in the Basic Operations section."),
                tags$li("Using targeted attribute filters or sub-posets to focus on specific zones."),
                tags$li("Exploring via the ", tags$strong("Micro Explorer"), " (focal navigation mode).")
              ),
              p("Alternatively, you can view the static plot below or force the full interactive layout if you are on a high-performance system:"),
              div(class="d-flex gap-2 mt-2",
                  actionButton("btnForceRender", "Force Interactive Render Anyway", class="btn-warning btn-sm", icon = icon("play")),
                  downloadButton("downloadStaticPlot", "Download Static Plot (PDF)", class="btn-outline-secondary btn-sm")
              )
          ),
          h5("Static Concept Lattice Plot"),
          plotOutput("staticPlot", height="700px")
        ))
      }
    }
    
    # Default case: n_filtered <= threshold
    tagList(style_tag, visNetworkOutput("interactivePlot", height = "100%"))
  })

  # --- CONCEPT METRICS REACTIVE ---
  concepts_metrics <- reactive({
    vals$trigger
    req(vals$fc)
    ready <- tryCatch(!vals$fc$concepts$is_empty(), error = function(e) FALSE)
    req(ready)
    
    concepts <- vals$fc$concepts
    n_size <- concepts$size()
    
    if (n_size > 100) {
      withProgress(message = "Computing Concept Metrics...", value = 0.1, {
        setProgress(detail = "Calculating Stability...")
        stab <- tryCatch(concepts$stability(), error = function(e) rep(NA, n_size))
        
        setProgress(value = 0.5, detail = "Calculating Separation...")
        sep <- tryCatch(concepts$separation(), error = function(e) rep(NA, n_size))
        
        setProgress(value = 0.8, detail = "Calculating Density...")
        dens <- tryCatch(concepts$density(vals$fc$I), error = function(e) rep(NA, n_size))
      })
    } else {
      stab <- tryCatch(concepts$stability(), error = function(e) rep(NA, n_size))
      sep <- tryCatch(concepts$separation(), error = function(e) rep(NA, n_size))
      dens <- tryCatch(concepts$density(vals$fc$I), error = function(e) rep(NA, n_size))
    }
    
    list(stability = stab, separation = sep, density = dens)
  })

  # --- DYNAMIC LATTICE LEGEND ---
  output$latticeLegendUI <- renderUI({
    vals$trigger
    req(vals$fc)
    ready <- tryCatch(!vals$fc$concepts$is_empty(), error = function(e) FALSE)
    if (!ready) return(NULL)
    
    color_mode <- if (!is.null(input$color_mode)) input$color_mode else "irreducibles"
    
    if (color_mode == "irreducibles") {
      div(
        class = "d-flex align-items-center gap-2",
        span(style="color:#FFD580; font-weight:900; margin-left:10px;", "● Attribute (meet-irreducible)"),
        span(style="color:#97C2FC; font-weight:900; margin-left:10px;", "● Object (join-irreducible)"),
        span(style="color:#90EE90; font-weight:900; margin-left:10px;", "● Both (meet + join)"),
        span(style="color:#E0E0E0; font-weight:900; margin-left:10px;", "● Intersection")
      )
    } else {
      # Metrics legend with gradient scale
      met <- tryCatch(concepts_metrics(), error = function(e) NULL)
      if (is.null(met)) return(NULL)
      
      low_col <- ""
      high_col <- ""
      vals_vec <- NULL
      label_text <- ""
      
      if (color_mode == "stability") {
        label_text <- "Stability"
        low_col <- "#F3E5F5"
        high_col <- "#4A148C"
        vals_vec <- met$stability
      } else if (color_mode == "separation") {
        label_text <- "Separation"
        low_col <- "#E0F2F1"
        high_col <- "#004D40"
        vals_vec <- met$separation
      } else if (color_mode == "density") {
        label_text <- "Fuzzy Density"
        low_col <- "#FFF3E0"
        high_col <- "#E65100"
        vals_vec <- met$density
      }
      
      min_val <- if (length(vals_vec) > 0) round(min(vals_vec, na.rm = TRUE), 3) else 0
      max_val <- if (length(vals_vec) > 0) round(max(vals_vec, na.rm = TRUE), 3) else 1
      
      div(
        class = "d-flex align-items-center gap-2",
        style = "min-width: 300px; max-width: 400px; font-weight: 500;",
        span(class = "text-muted small", min_val),
        div(
          style = sprintf(
            "height: 12px; width: 150px; border-radius: 6px; background: linear-gradient(to right, %s, %s); border: 1px solid #dddddd;",
            low_col, high_col
          )
        ),
        span(class = "text-muted small", max_val),
        span(class = "badge bg-secondary ms-2", label_text)
      )
    }
  })

  # --- RENDER INTERACTIVE PLOT ---
  output$interactivePlot <- renderVisNetwork({
    vals$trigger
    req(vals$fc)
    
    # Auto-compute concepts if not computed yet
    if (is.null(vals$fc$concepts) || vals$fc$concepts$size() == 0) {
      withProgress(message = "Computing concept lattice...", value = 0.5, {
        vals$fc$find_concepts()
      })
    }
    
    n <- vals$fc$concepts$size()
    if (n == 0) return(NULL)
    
    scale_node_size <- if (!is.null(input$scale_node_support)) input$scale_node_support else FALSE
    lbl_mode <- if (!is.null(input$labeling_mode)) input$labeling_mode else "reduced"
    col_mode <- if (!is.null(input$color_mode)) input$color_mode else "irreducibles"
    metrics_list <- tryCatch(concepts_metrics(), error = function(e) NULL)
    
    if (isTRUE(input$exploration_mode == "micro")) {
      center_id <- as.numeric(input$center_concept_id)
      if (is.null(center_id) || is.na(center_id) || center_id < 1 || center_id > n) {
        center_id <- 1
      }
      
      intents_mat <- as.matrix(vals$fc$concepts$intents())
      neigh <- get_neighborhood_ids_binary(center_id, intents_mat)
      g_data <- getMicroGraph(
        vals$fc$concepts, 
        center_id, 
        neigh, 
        vals$fc, 
        scale_support = scale_node_size, 
        labeling_mode = lbl_mode,
        color_mode = col_mode,
        metrics = metrics_list
      )
      if (!is.null(g_data$edges) && nrow(g_data$edges) > 0) {
        vals$lattice_edges_cache <- g_data$edges
      } else {
        vals$lattice_edges_cache <- NULL
      }
      g_vis <- strip_lattice_vis_tooltips(g_data)
      
      layout_val <- input$vizLayout
      vis <- visNetwork::visNetwork(nodes = g_vis$nodes, edges = g_vis$edges)
      
      if (isTRUE(layout_val == "layout_with_sugiyama")) {
        vis <- vis %>% visNetwork::visHierarchicalLayout(
          direction = "UD",
          sortMethod = "directed",
          levelSeparation = 120,
          nodeSpacing = 150
        )
      } else {
        vis <- vis %>% visNetwork::visIgraphLayout(layout = "layout_with_kk")
      }
      
      vis %>%
        visNetwork::visOptions(
          highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"),
          nodesIdSelection = FALSE
        ) %>%
        visNetwork::visEdges(
          arrows = "to",
          color = list(color = "#888888"),
          smooth = list(type = "cubicBezier", roundness = 0.5)
        ) %>%
        visNetwork::visEvents(
          selectNode = "function(properties) {
            var clickedNode = properties.nodes[0];
            Shiny.setInputValue('clicked_micro_node', clickedNode, {priority: 'event'});
          }",
          hoverNode = "function(properties) {
            if (properties.node) {
              Shiny.setInputValue('lattice_hover_node', properties.node, {priority: 'event'});
            }
          }",
          hoverEdge = "function(properties) {
            if (properties.edge) {
              fcaRvizHandleLatticeEdgeHover(properties.edge, this.body.data.edges.get(properties.edge));
            }
          }",
          click = "function(properties) {
            var edgeId = (properties.edges && properties.edges.length > 0)
              ? properties.edges[0]
              : this.getEdgeAt({x: properties.pointer.DOM.x, y: properties.pointer.DOM.y});
            if (edgeId) {
              fcaRvizHandleLatticeEdgeClick(this, edgeId, this.body.data.edges.get(edgeId));
            } else if (properties.nodes && properties.nodes.length > 0) {
              var nodeId = properties.nodes[0];
              Shiny.setInputValue('lattice_click_node', {id: nodeId, t: Date.now()}, {priority: 'event'});
              Shiny.setInputValue('clicked_micro_node', nodeId, {priority: 'event'});
            }
          }"
        ) %>%
        visNetwork::visInteraction(
          navigationButtons = FALSE,
          zoomView = TRUE,
          dragView = TRUE,
          hover = TRUE,
          tooltipDelay = 999999
        )
      
    } else {
      # Macro view
      curr_ids <- current_filtered_ids()
      n_filtered <- if (is.null(curr_ids)) n else length(curr_ids)
      thresh <- 800
      if (!is.null(input$enable_threshold)) {
        thresh <- if (isTRUE(input$enable_threshold)) (if (!is.null(input$safety_threshold)) input$safety_threshold else 800) else Inf
      }
      if (n_filtered > thresh && !vals$force_interactive_render) return(NULL)
      tryCatch({ 
        g <- getGraph(
          vals$fc$concepts, 
          vals$fc, 
          filter_ids = current_filtered_ids(), 
          scale_support = scale_node_size, 
          labeling_mode = lbl_mode,
          color_mode = col_mode,
          metrics = metrics_list
        )
        if (!is.null(g$edges) && nrow(g$edges) > 0) {
          vals$lattice_edges_cache <- g$edges
        } else {
          vals$lattice_edges_cache <- NULL
        }
        vals$original_nodes_df <- data.frame(
          id = as.character(g$nodes$id),
          color.background = g$nodes$color.background,
          color.border = g$nodes$color.border,
          size = if ("size" %in% names(g$nodes)) g$nodes$size else 22,
          borderWidth = if ("borderWidth" %in% names(g$nodes)) g$nodes$borderWidth else 1,
          stringsAsFactors = FALSE
        )
        g_vis <- strip_lattice_vis_tooltips(g)
        
        # Configure layout
        layout_val <- input$vizLayout
        vis <- visNetwork::visNetwork(nodes = g_vis$nodes, edges = g_vis$edges)
        
        if (isTRUE(layout_val == "layout_with_sugiyama")) {
          vis <- vis %>% visNetwork::visIgraphLayout(layout = "layout_with_sugiyama")
        } else {
          vis <- vis %>% visNetwork::visIgraphLayout(layout = "layout_with_kk")
        }
        
        vis %>%
          visNetwork::visOptions(
            highlightNearest = list(enabled = TRUE, algorithm = "hierarchical", labelOnly = FALSE),
            nodesIdSelection = TRUE
          ) %>%
          visNetwork::visEdges(
            arrows = list("to" = list(enabled = TRUE, scaleFactor = 0.5)),
            color = list(color = "#AAAAAA"),
            smooth = list(type = "cubicBezier", roundness = 0.5)
          ) %>%
          visNetwork::visEvents(
            selectNode = "function(properties) {
              var clickedNode = properties.nodes[0];
              Shiny.setInputValue('clicked_macro_node', clickedNode, {priority: 'event'});
            }",
            deselectNode = "function(properties) {
              Shiny.setInputValue('clicked_macro_node', null, {priority: 'event'});
              Shiny.setInputValue('interactivePlot_selected', '', {priority: 'event'});
            }",
            hoverNode = "function(properties) {
              if (properties.node) {
                Shiny.setInputValue('lattice_hover_node', properties.node, {priority: 'event'});
              }
            }",
            hoverEdge = "function(properties) {
              if (properties.edge) {
                fcaRvizHandleLatticeEdgeHover(properties.edge, this.body.data.edges.get(properties.edge));
              }
            }",
            click = "function(properties) {
              var edgeId = (properties.edges && properties.edges.length > 0)
                ? properties.edges[0]
                : this.getEdgeAt({x: properties.pointer.DOM.x, y: properties.pointer.DOM.y});
              if (edgeId) {
                fcaRvizHandleLatticeEdgeClick(this, edgeId, this.body.data.edges.get(edgeId));
              } else if (properties.nodes && properties.nodes.length > 0) {
                var nodeId = properties.nodes[0];
                Shiny.setInputValue('lattice_click_node', {id: nodeId, t: Date.now()}, {priority: 'event'});
                Shiny.setInputValue('clicked_macro_node', nodeId, {priority: 'event'});
              }
            }"
          ) %>%
          visNetwork::visPhysics(stabilization = FALSE, barnesHut = list(gravitationalConstant = -2000)) %>%
          visNetwork::visInteraction(
            navigationButtons = FALSE,
            zoomView = TRUE,
            dragView = TRUE,
            hover = TRUE,
            tooltipDelay = 999999
          )
      }, error = function(e) NULL)
    }
  })

  output$staticPlot <- renderPlot({ 
    vals$trigger
    req(vals$fc)
    if(vals$fc$concepts$size() <= 5000) {
      plot_lattice_static(vals$fc$concepts, vals$fc, layout_val = input$vizLayout, filter_ids = current_filtered_ids())
    }
  })

  output$downloadStaticPlot <- downloadHandler(
    filename = function() {
      paste0("static_lattice_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
    },
    content = function(file) {
      req(vals$fc)
      pdf(file, width = 10, height = 8)
      tryCatch({
        plot_lattice_static(vals$fc$concepts, vals$fc, layout_val = input$vizLayout, filter_ids = current_filtered_ids())
      }, error = function(e) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "")
        text(0.5, 0.5, "Lattice too large or plot error.")
      })
      dev.off()
    }
  )

  # --- DETALLES DE CONCEPTOS ACTUALIZADOS ---
  output$conceptSelected <- renderUI({
    req(vals$fc, vals$active_concept_id)
    index <- vals$active_concept_id
    n <- vals$fc$concepts$size()
    req(index > 0 && index <= n)
    
    # Absolute irreducible status
    status <- tryCatch(get_concept_irreducible_status(index, vals$fc$concepts, vals$fc), error = function(e) NULL)
    
    # Calculate metrics
    met <- tryCatch(concepts_metrics(), error = function(e) NULL)
    stab_val <- if (!is.null(met)) round(met$stability[index], 4) else NA
    sep_val <- if (!is.null(met)) round(met$separation[index], 4) else NA
    dens_val <- if (!is.null(met)) round(met$density[index], 4) else NA
    
    # Renders card with metrics and irreducible status
    tagList(
      div(
        class = "card border-secondary mb-3 shadow-sm",
        div(
          class = "card-header bg-secondary text-white fw-bold d-flex justify-content-between align-items-center",
          span(sprintf("Concept details: C%s", index)),
          if (!is.null(status)) span(class = "badge bg-light text-dark", status$type)
        ),
        div(
          class = "card-body p-3",
          # Grid of metrics
          div(
            class = "row g-2 text-center mb-3",
            div(
              class = "col-4",
              div(
                class = "border rounded p-2 bg-light",
                span(class = "text-muted small d-block", "Stability"),
                strong(class = "text-primary", ifelse(is.na(stab_val), "-", stab_val))
              )
            ),
            div(
              class = "col-4",
              div(
                class = "border rounded p-2 bg-light",
                span(class = "text-muted small d-block", "Separation"),
                strong(class = "text-success", ifelse(is.na(sep_val), "-", sep_val))
              )
            ),
            div(
              class = "col-4",
              div(
                class = "border rounded p-2 bg-light",
                span(class = "text-muted small d-block", "Fuzzy Density"),
                strong(class = "text-warning", ifelse(is.na(dens_val), "-", dens_val))
              )
            )
          ),
          
          # Introduced objects and attributes if any
          if (!is.null(status) && (length(status$introduced_attributes) > 0 || length(status$introduced_objects) > 0)) {
            div(
              class = "mb-3 small",
              if (length(status$introduced_attributes) > 0) {
                div(
                  class = "mb-1",
                  strong("Introduces Attributes: "),
                  span(class = "text-muted", paste(status$introduced_attributes, collapse = ", "))
                )
              },
              if (length(status$introduced_objects) > 0) {
                div(
                  class = "mb-1",
                  strong("Introduces Objects: "),
                  span(class = "text-muted", paste(status$introduced_objects, collapse = ", "))
                )
              }
            )
          },
          
          # The table of Extent and Intent
          h6("Extent and Intent Contents", class = "border-bottom pb-1 mb-2 fw-bold"),
          tableOutput("conceptSelectedTable")
        )
      )
    )
  })

  output$associationRuleSelected <- renderUI({
    vals$active_edge_nonce
    req(vals$fc)
    if (is.null(vals$active_edge_from) || is.null(vals$active_edge_to)) {
      return(div(
        class = "text-muted small p-2",
        icon("info-circle"),
        " Click an edge (arrow) in the lattice to see its association rule."
      ))
    }
    rule <- get_association_rule_details(vals$active_edge_from, vals$active_edge_to, vals$fc)
    req(!is.null(rule))

    lhs_str <- if (length(rule$lhs) > 0) paste(rule$lhs, collapse = ", ") else "\u2205"
    rhs_str <- if (length(rule$rhs) > 0) paste(rule$rhs, collapse = ", ") else "\u2205"

    div(
      class = "card border-success mb-3 shadow-sm",
      div(
        class = "card-header bg-success text-white fw-bold",
        sprintf("Association rule: C%s \u2192 C%s", rule$from_id, rule$to_id)
      ),
      div(
        class = "card-body p-3",
        div(
          class = "mb-3",
          h6("Rule (LHS \u2192 RHS)", class = "fw-bold border-bottom pb-1 mb-2"),
          div(
            class = "small",
            span(class = "badge bg-light text-dark border me-1", "LHS"),
            span("{", lhs_str, "}"),
            span(class = "mx-2", "\u2192"),
            span(class = "badge bg-light text-dark border me-1", "RHS"),
            span("{", rhs_str, "}")
          )
        ),
        div(
          class = "row g-2 text-center",
          div(
            class = "col-4",
            div(
              class = "border rounded p-2 bg-light",
              span(class = "text-muted small d-block", "Confidence"),
              strong(class = "text-success", sprintf("%.1f%%", rule$confidence * 100))
            )
          ),
          div(
            class = "col-4",
            div(
              class = "border rounded p-2 bg-light",
              span(class = "text-muted small d-block", "Parent support"),
              strong(sprintf("%.1f%%", rule$parent_support_pct)),
              span(class = "text-muted small d-block", paste0("(", rule$parent_support_n, " objs)"))
            )
          ),
          div(
            class = "col-4",
            div(
              class = "border rounded p-2 bg-light",
              span(class = "text-muted small d-block", "Child support"),
              strong(sprintf("%.1f%%", rule$child_support_pct)),
              span(class = "text-muted small d-block", paste0("(", rule$child_support_n, " objs)"))
            )
          )
        ),
        div(
          class = "text-muted small mt-3 mb-0",
          icon("mouse-pointer"),
          " Click a concept node to return to concept details."
        )
      )
    )
  })

  output$latticePropertiesUI <- renderUI({
    vals$trigger
    req(vals$fc)
    ready <- tryCatch(!vals$fc$concepts$is_empty(), error = function(e) FALSE)
    if (!ready) return(p(class="text-muted small", "No concept lattice loaded yet."))
    
    cl <- vals$fc$concepts
    
    # Calculate properties with tryCatch
    is_dist <- tryCatch(cl$is_distributive(), error = function(e) NA)
    is_mod <- tryCatch(cl$is_modular(), error = function(e) NA)
    is_semi <- tryCatch(cl$is_semimodular(), error = function(e) NA)
    is_atom <- tryCatch(cl$is_atomic(), error = function(e) NA)
    w_val <- tryCatch(cl$width(), error = function(e) NA)
    d_val <- tryCatch(cl$dimension(), error = function(e) NA)
    
    make_badge <- function(val) {
      if (is.na(val)) return(span(class="badge bg-secondary", "Unknown"))
      if (isTRUE(val)) {
        span(class="badge bg-success", "Yes")
      } else {
        span(class="badge bg-danger", "No")
      }
    }
    
    tagList(
      div(
        class = "p-2",
        div(
          class = "d-flex justify-content-between align-items-center mb-2 pb-2 border-bottom",
          span(class="text-muted small", "Algebraic and structural metrics"),
          actionLink("btnLatticeInfo", "", icon = icon("info-circle"), style = "cursor: pointer; font-size: 1.1em; color: #0d6efd;", title = "Show definitions & scientific references")
        ),
        tags$table(
          class = "table table-sm table-borderless small mb-0",
          tags$tbody(
            tags$tr(tags$td(strong("Number of Concepts:")), tags$td(class="text-end", cl$size())),
            tags$tr(tags$td(strong("Distributive:")), tags$td(class="text-end", make_badge(is_dist))),
            tags$tr(tags$td(strong("Modular:")), tags$td(class="text-end", make_badge(is_mod))),
            tags$tr(tags$td(strong("Semimodular:")), tags$td(class="text-end", make_badge(is_semi))),
            tags$tr(tags$td(strong("Atomic:")), tags$td(class="text-end", make_badge(is_atom))),
            tags$tr(tags$td(strong("Lattice Width:")), tags$td(class="text-end", ifelse(is.na(w_val), "-", w_val))),
            tags$tr(tags$td(strong("Lattice Dimension:")), tags$td(class="text-end", ifelse(is.na(d_val), "-", d_val)))
          )
        )
      )
    )
  })

  # --- OBSERVADOR PARA EL MODAL INFORMATIVO ---
  observeEvent(input$btnLatticeInfo, {
    showModal(modalDialog(
      title = "FCA Metrics & Properties Reference Guide",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <div style=\"font-family: 'Inter', sans-serif; line-height: 1.6;\">
          <h5 class=\"text-primary border-bottom pb-2\">Concept Quality Metrics</h5>
          
          <p><strong>1. Stability</strong></p>
          <ul>
            <li><strong>Definition:</strong> Measures the robustness of a formal concept. It represents the probability that the concept's intent remains closed if some objects are randomly removed from the formal context.</li>
            <li><strong>Scientific Reference:</strong> 
              <ul>
                <li>Kuznetsov, S. O. (1990). <em>On stability of a formal concept</em>. Rostov-on-Don (in Russian).</li>
                <li>Kuznetsov, S. O. (2007). <em>Concepts' Stability</em>. In: Priss, U., Pollandt, S., Ernst, G. (eds) Conceptual Structures: Knowledge Architectures for Smart Applications. Lecture Notes in Computer Science, vol 4604. Springer, Berlin.</li>
              </ul>
            </li>
          </ul>

          <p><strong>2. Separation</strong></p>
          <ul>
            <li><strong>Definition:</strong> Measures the degree to which the concept's intent isolates the objects in its extent from other objects in the context. High separation indicates a well-defined, independent conceptual cluster.</li>
            <li><strong>Scientific Reference:</strong> 
              <ul>
                <li>Klimushkin, M., Obiedkov, S., & Roth, C. (2010). <em>Faceted taxonomy construction with formal concept analysis</em>. In: Kryszkiewicz, M., Obiedkov, S. (eds) Formal Concept Analysis. ICFCA 2010. Lecture Notes in Computer Science, vol 5986. Springer, Heidelberg.</li>
              </ul>
            </li>
          </ul>

          <p><strong>3. Fuzzy Density</strong></p>
          <ul>
            <li><strong>Definition:</strong> The proportion of cross cells (or membership values in fuzzy contexts) inside the submatrix defined by the extent and intent of the concept, relative to the total possible cells in that subcontext.</li>
            <li><strong>Scientific Reference:</strong> 
              <ul>
                <li>Belohlavek, R. (1999). <em>Fuzzy Galois connections</em>. Mathematical Logic Quarterly, 45(4), 497-504.</li>
                <li>Belohlavek, R. (2002). <em>Fuzzy Relational Systems: Foundations and Principles</em>. Kluwer Academic Publishers.</li>
              </ul>
            </li>
          </ul>

          <h5 class=\"text-primary border-bottom pb-2 mt-4\">Structural and Algebraic Properties of the Lattice</h5>

          <p><strong>4. Distributivity, Modularity and Semimodularity</strong></p>
          <ul>
            <li><strong>Definitions:</strong> 
              <ul>
                <li><em>Distributive Lattice:</em> A complete lattice where meet and join operations distribute over each other. It contains no sublattices isomorphic to the pentagon N<sub>5</sub> or the diamond M<sub>5</sub>.</li>
                <li><em>Modular Lattice:</em> Satisfies the modular identity: if x &le; z, then x v (y ^ z) = (x v y) ^ z.</li>
                <li><em>Semimodular Lattice:</em> If x and y cover their meet x ^ y, then their join x v y covers both x and y.</li>
              </ul>
            </li>
            <li><strong>Scientific Reference:</strong> 
              <ul>
                <li>Birkhoff, G. (1940). <em>Lattice Theory</em>. American Mathematical Society Colloquium Publications, vol. 25.</li>
                <li>Ganter, B., & Wille, R. (1999). <em>Formal Concept Analysis: Mathematical Foundations</em>. Springer-Verlag.</li>
              </ul>
            </li>
          </ul>

          <p><strong>5. Lattice Width</strong></p>
          <ul>
            <li><strong>Definition:</strong> The maximum size of an antichain; that is, the largest set of pairwise incomparable concepts. It measures the maximum degree of divergence or parallelism of the taxonomy.</li>
            <li><strong>Scientific Reference:</strong> 
              <ul>
                <li>Dilworth, R. P. (1950). <em>A Decomposition Theorem for Partially Ordered Sets</em>. Annals of Mathematics, 51(1), 161-166.</li>
              </ul>
            </li>
          </ul>

          <p><strong>6. Lattice Dimension</strong></p>
          <ul>
            <li><strong>Definition:</strong> The Dushnik-Miller dimension. It represents the minimum number of linear orders (chains) whose intersection yields exactly the lattice order relation. Graphically, it indicates the minimum number of independent coordinates needed to position the concepts.</li>
            <li><strong>Scientific Reference:</strong> 
              <ul>
                <li>Dushnik, B., & Miller, E. W. (1941). <em>Partially ordered sets</em>. American Journal of Mathematics, 63(3), 600-610.</li>
              </ul>
            </li>
          </ul>
        </div>
      ")
    ))
  })

  # --- OBSERVADOR PARA INFORMACIÓN GENERAL DE FCA ---
  observeEvent(input$btnFcaInfo, {
    showModal(modalDialog(
      title = "Introduction to Formal Concept Analysis (FCA)",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <div style=\"font-family: 'Inter', sans-serif; line-height: 1.6;\">
          <h5 class=\"text-primary border-bottom pb-2\">What is Formal Concept Analysis (FCA)?</h5>
          <p>Formal Concept Analysis is a mathematical data analysis discipline that identifies conceptual structures from binary relations between objects and attributes.</p>
          
          <p><strong>The Formal Context:</strong></p>
          <p>It is mathematically defined as a triple K = (G, M, I) where:</p>
          <ul>
            <li><strong>G (Objects):</strong> A set of objects (Gegenstände).</li>
            <li><strong>M (Attributes):</strong> A set of attributes (Merkmale).</li>
            <li><strong>I (Incidence):</strong> A binary relation between them (I &sube; G &times; M). If (g, m) &in; I, it indicates that object <em>g</em> has attribute <em>m</em>.</li>
          </ul>

          <h5 class=\"text-primary border-bottom pb-2 mt-4\">Primary Reference of FCA</h5>
          <p>The foundational formulation of FCA was described in depth in the classic book:</p>
          <div class=\"p-3 bg-light border rounded mb-3\">
            <strong>Ganter, B., & Wille, R. (1999).</strong> <em>Formal Concept Analysis: Mathematical Foundations</em>. Springer-Verlag.
          </div>
          <p>This work sets the foundations of Galois connection, derivation operators, and the fundamental theorem of concept lattices.</p>
        </div>
      ")
    ))
  })

  # --- OBSERVADOR PARA OPERACIONES BÁSICAS Y REDUCCIÓN ---
  observeEvent(input$btnBasicOpsInfo, {
    showModal(modalDialog(
      title = "Set Operations & Context Simplification",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <div style=\"font-family: 'Inter', sans-serif; line-height: 1.6;\">
          <h5 class=\"text-primary border-bottom pb-2\">Derivation Operators</h5>
          <p>In FCA, the Galois connection is materialized via Intent and Extent operators (typically denoted by a prime symbol '):</p>
          <ul>
            <li><strong>Intent:</strong> Given a set of objects A, A' is the set of all attributes shared by all objects in A.</li>
            <li><strong>Extent:</strong> Given a set of attributes B, B' is the set of all objects that possess all attributes in B.</li>
            <li><strong>Closure:</strong> The composite operator (A'') is a closure operator. A set is closed if A = A''.</li>
          </ul>

          <h5 class=\"text-primary border-bottom pb-2 mt-4\">Context Simplification & Reduction</h5>
          <ul>
            <li><strong>Clarify:</strong> Groups objects or attributes with identical incidence patterns in the context (i.e. equivalent elements), leaving a single representative.</li>
            <li><strong>Reduce:</strong> Removes redundant attributes or objects (those that are clarify-able or expressible as the intersection/closure of others), simplifying the context without losing conceptual information.</li>
            <li><strong>Arrow Relations:</strong> Structured relations (&swarrow;, &nearrow;, &updownarrow;) in the context that efficiently characterize the irreducibility of attributes and objects for their removal.</li>
          </ul>

          <h5 class=\"text-primary border-bottom pb-2 mt-4\">Bibliographical References</h5>
          <div class=\"p-3 bg-light border rounded mb-3\">
            <ul>
              <li><strong>Ganter, B., & Wille, R. (1999).</strong> <em>Formal Concept Analysis: Mathematical Foundations</em>. Springer-Verlag.</li>
              <li><strong>Wille, R. (1982).</strong> <em>Restructuring lattice theory: an approach based on hierarchies of concepts</em>. Ordered Sets, Reidel, Dordrecht, 445-470.</li>
            </ul>
          </div>
        </div>
      ")
    ))
  })

  # --- OBSERVADOR PARA ALGORITMOS DE CÁLCULO DEL RETÍCULO ---
  observeEvent(input$btnLatticeAlgoInfo, {
    showModal(modalDialog(
      title = "Concept Lattice Construction Algorithms",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <div style=\"font-family: 'Inter', sans-serif; line-height: 1.6;\">
          <h5 class=\"text-primary border-bottom pb-2\">Computing the Concept Lattice</h5>
          <p>A formal concept is a pair (A, B) such that A' = B and B' = A. The concept lattice is the set of all concepts ordered hierarchically by extent inclusion (or reverse intent inclusion).</p>
          
          <h5 class=\"text-primary border-bottom pb-2 mt-4\">Implemented Algorithms</h5>
          <p>The <code>fcaR</code> package implements efficient state-of-the-art algorithms to find all formal concepts:</p>
          <ul>
            <li><strong>NextClosure:</strong> Ganter's foundational lexicographical traversal algorithm. Sequential and deterministic, with high theoretical value.</li>
            <li><strong>InClose (Close-by-One / In-Close):</strong> A modern, highly efficient algorithm developed by Stephen Andrews, which optimizes computations using local checks and avoiding candidate repetition via attribute tree traversal.</li>
          </ul>

          <h5 class=\"text-primary border-bottom pb-2 mt-4\">Bibliographical References</h5>
          <div class=\"p-3 bg-light border rounded mb-3\">
            <ul>
              <li><strong>Ganter, B. (1984).</strong> <em>Two basic algorithms in formal concept analysis</em>. Technische Hochschule Darmstadt, Preprint 831.</li>
              <li><strong>Andrews, S. (2009).</strong> <em>In-Close, a Fast Algorithm for Computing Formal Concepts</em>. In: Conceptual Structures: Leveraging Semantic Technologies. ICCS 2009. Lecture Notes in Computer Science, vol 5662. Springer.</li>
              <li><strong>Kuznetsov, S. O. (1999).</strong> <em>Learning of simple conceptual graphs from positive and negative examples</em>. Principles of Data Mining and Knowledge Discovery, PKDD '99.</li>
            </ul>
          </div>
        </div>
      ")
    ))
  })

  # --- OBSERVADOR PARA ALGORITMOS DE IMPLICACIONES Y LÓGICA DE SIMPLIFICACIÓN ---
  observeEvent(input$btnImplicationsInfo, {
    showModal(modalDialog(
      title = "Implications & Simplification Logic",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <div style=\"font-family: 'Inter', sans-serif; line-height: 1.6;\">
          <h5 class=\"text-primary border-bottom pb-2\">Implication Mining (LinCbO)</h5>
          <p>An implication A &rarr; B represents a rule stating that any object that possesses the set of attributes A will also possess the set B. In FCA, we aim to compute a minimal informative basis (typically the Duquenne-Guigues basis) to capture all such rules.</p>
          <ul>
            <li><strong>LinCbO (Linear Close-by-One):</strong> An optimized implication basis extraction algorithm used by <code>fcaR</code> to compute rules linearly through a combined search of closures and lexicographical order.</li>
          </ul>

          <h5 class=\"text-primary border-bottom pb-2 mt-4\">Simplification Logic</h5>
          <p>Simplification Logic is an alternative logical system to Armstrong's Axioms for reasoning with implications, designed specifically for automated computational processing via logical equivalence rules:</p>
          <ul>
            <li>Allows drastic reduction of the size and attribute count of redundant implications.</li>
            <li>Key simplification rules: premise reduction, composition, and redundant attribute elimination via closures.</li>
          </ul>

          <h5 class=\"text-primary border-bottom pb-2 mt-4\">Bibliographical References</h5>
          <div class=\"p-3 bg-light border rounded mb-3\">
            <ul>
              <li><strong>Obiedkov, S., & Duquenne, V. (2007).</strong> <em>Attribute exploration with background knowledge</em>. Annals of Mathematics and Artificial Intelligence, 49(1-4), 201-219.</li>
              <li><strong>Guigues, J.-L., & Duquenne, V. (1986).</strong> <em>Familles minimales d'implications informatives dans un tableau de données binaires</em>. Mathématiques et Sciences Humaines, 95, 5-18.</li>
              <li><strong>Cordero, P., Enciso, M., Mora, A., & Pérez de Guzmán, I. (2002).</strong> <em>Simplification logic for S-implications</em>. Logic Journal of the IGPL, 10(4), 361-381.</li>
              <li><strong>Mora, A., Cordero, P., Enciso, M., Fortes, I., & Guzmán, I. (2012).</strong> <em>Closure via simplification logic</em>. International Journal of Computer Mathematics, 89(3), 380-399.</li>
            </ul>
          </div>
        </div>
      ")
    ))
  })

  output$conceptSelectedTable <- renderTable({ index <- vals$active_concept_id; req(index > 0 && index <= vals$fc$concepts$size()); get_concepts_dataframe(vals$fc$concepts[index], vals$fc) })
  output$upperNeighbours <- renderUI({
    req(vals$fc, vals$active_concept_id)
    index <- vals$active_concept_id
    req(index > 0 && index <= vals$fc$concepts$size())
    upper <- vals$fc$concepts$upper_neighbours(vals$fc$concepts[index])
    if (upper$size() == 0) {
      return(div(class = "text-muted small p-2", icon("info-circle"), " No upper neighbours (this is a top concept)."))
    }
    tableOutput("upperNeighboursTable")
  })
  output$upperNeighboursTable <- renderTable({
    index <- vals$active_concept_id
    req(index > 0 && index <= vals$fc$concepts$size())
    get_concepts_dataframe(vals$fc$concepts$upper_neighbours(vals$fc$concepts[index]), vals$fc)
  })
  output$lowerNeighbours <- renderUI({
    req(vals$fc, vals$active_concept_id)
    index <- vals$active_concept_id
    req(index > 0 && index <= vals$fc$concepts$size())
    lower <- vals$fc$concepts$lower_neighbours(vals$fc$concepts[index])
    if (lower$size() == 0) {
      return(div(class = "text-muted small p-2", icon("info-circle"), " No lower neighbours (this is a bottom concept)."))
    }
    tableOutput("lowerNeighboursTable")
  })
  output$lowerNeighboursTable <- renderTable({
    index <- vals$active_concept_id
    req(index > 0 && index <= vals$fc$concepts$size())
    get_concepts_dataframe(vals$fc$concepts$lower_neighbours(vals$fc$concepts[index]), vals$fc)
  })

  # --- TAB LATTICE FILTERING LOGIC ---
  vals$tab_concept_filter_ids <- NULL

  observeEvent(input$btnApplyTabFilters, {
    req(vals$fc)
    if (tryCatch(vals$fc$concepts$is_empty(), error = function(e) TRUE)) {
      shinyalert("Oops!", "Please compute concepts first.", type = "warning")
      return()
    }
    filtered <- calculate_filtered_concept_ids(vals$fc, input$filterObjsTab, input$filterAttsTab)
    vals$tab_concept_filter_ids <- filtered
    vals$concept_filter_ids <- filtered
    
    # Synchronize selectize inputs in the sidebar
    updateSelectizeInput(session, "filterObjs", choices = vals$fc$objects, selected = input$filterObjsTab)
    updateSelectizeInput(session, "filterAtts", choices = vals$fc$attributes, selected = input$filterAttsTab)
  })

  observeEvent(input$btnClearTabFilters, {
    req(vals$fc)
    updateSelectizeInput(session, "filterObjsTab", choices = vals$fc$objects, selected = character(0))
    updateSelectizeInput(session, "filterAttsTab", choices = vals$fc$attributes, selected = character(0))
    updateSelectizeInput(session, "filterObjs", choices = vals$fc$objects, selected = character(0))
    updateSelectizeInput(session, "filterAtts", choices = vals$fc$attributes, selected = character(0))
    vals$tab_concept_filter_ids <- NULL
    vals$concept_filter_ids <- NULL
  })

  output$tabFilterStatusBadge <- renderText({
    req(vals$fc)
    if (tryCatch(vals$fc$concepts$is_empty(), error = function(e) TRUE)) return("")
    n_total <- vals$fc$concepts$size()
    if (is.null(vals$tab_concept_filter_ids)) {
      return(paste("Showing all", n_total, "concepts"))
    } else {
      n_filtered <- length(vals$tab_concept_filter_ids)
      return(paste("Showing", n_filtered, "of", n_total, "concepts"))
    }
  })

  output$tabFilteredTable <- renderTable({
    req(vals$fc)
    if (tryCatch(vals$fc$concepts$is_empty(), error = function(e) TRUE)) return(NULL)
    
    n_total <- vals$fc$concepts$size()
    ids <- if (is.null(vals$tab_concept_filter_ids)) 1:n_total else vals$tab_concept_filter_ids
    
    if (length(ids) == 0) return(data.frame(Message = "No matching concepts found."))
    
    get_concepts_dataframe(vals$fc$concepts[ids], vals$fc)
  })

  observeEvent(input$btnFilterHelp, {
    showModal(modalDialog(
      title = "Concept Lattice Filtering Guide",
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      HTML("
        <div style=\"font-family: 'Inter', sans-serif; line-height: 1.6;\">
          <h5 class=\"text-primary border-bottom pb-2\">How does the Lattice Filter work?</h5>
          <p>Filtering allows you to focus on specific sub-regions of the concept lattice by restricting the concepts shown in the graph and table.</p>
          <ul>
            <li><strong>Filter by Objects:</strong> Retains only the concepts whose <strong>Extent</strong> contains ALL the selected objects.</li>
            <li><strong>Filter by Attributes:</strong> Retains only the concepts whose <strong>Intent</strong> contains ALL the selected attributes.</li>
          </ul>
          <p>When you apply the filter, the graph displays the corresponding <strong>sub-poset</strong>, drawing the hierarchical connections (meet/join lines) between the selected concepts only. The concepts table shows their full extents, intents, and algebraic classifications.</p>
        </div>
      ")
    ))
  })

  # --- TAB LATTICE ASSOCIATION RULES LOGIC ---
  observeEvent(input$btnComputeAssocRules, {
    req(vals$fc)
    if (tryCatch(vals$fc$concepts$is_empty(), error = function(e) TRUE)) {
      shinyalert("Oops!", "Please compute concepts first.", type = "warning")
      return()
    }
    
    selected_cid <- NULL
    if (isTRUE(input$assoc_use_selected_concept)) {
      selected_cid <- vals$active_concept_id
      if (is.null(selected_cid) || selected_cid == 0) {
        shinyalert("No Concept Selected", "Please select a concept in the Interactive Explorer first.", type = "warning")
        return()
      }
    }
    
    withProgress(message = "Extracting association rules from lattice...", value = 0.5, {
      tryCatch({
        df <- get_association_rules(
          fc = vals$fc,
          min_support = input$assoc_support,
          min_confidence = input$assoc_confidence,
          selected_concept_id = selected_cid,
          method = if (!is.null(input$assoc_method)) input$assoc_method else "covering"
        )
        vals$assoc_rules_df <- df
        
        n_rules <- if (is.null(df)) 0 else nrow(df)
        if (n_rules > 0) {
          shinyalert("Success", sprintf("Successfully extracted %d association rules from the concept lattice.", n_rules), type = "success")
        } else {
          shinyalert("No Rules", "No association rules met the specified support and confidence thresholds.", type = "warning")
        }
      }, error = function(e) {
        shinyalert("Extraction Error", e$message, type = "error")
      })
    })
  })

  observe({
    req(vals$fc)
    req(input$assoc_use_selected_concept)
    req(vals$active_concept_id)
    
    selected_cid <- vals$active_concept_id
    if (selected_cid > 0 && selected_cid <= vals$fc$concepts$size()) {
      tryCatch({
        df <- get_association_rules(
          fc = vals$fc,
          min_support = input$assoc_support,
          min_confidence = input$assoc_confidence,
          selected_concept_id = selected_cid,
          method = if (!is.null(input$assoc_method)) input$assoc_method else "covering"
        )
        vals$assoc_rules_df <- df
      }, error = function(e) NULL)
    }
  })

  output$assocRulesStatusBadge <- renderText({
    req(vals$fc)
    if (is.null(vals$assoc_rules_df)) {
      return("Extract rules to view results.")
    }
    n_rules <- nrow(vals$assoc_rules_df)
    return(sprintf("Extracted %d association rules", n_rules))
  })

  output$assocRulesTable <- DT::renderDT({
    req(vals$assoc_rules_df)
    df <- vals$assoc_rules_df
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No rules extracted."), options = list(dom = "t")))
    }
    
    df_disp <- df
    df_disp$Support <- sprintf("%.1f%%", df_disp$Support * 100)
    df_disp$Confidence <- sprintf("%.1f%%", df_disp$Confidence * 100)
    
    colnames(df_disp) <- c("LHS (Antecedent)", "RHS (Consequent)", "Support", "Confidence", "LHS Concept ID", "RHS Concept ID")
    
    DT::datatable(
      df_disp,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        searchDelay = 500,
        dom = "lfrtip"
      ),
      selection = "none",
      rownames = FALSE
    )
  })

  output$downloadAssocRulesRds <- downloadHandler(
    filename = function() {
      paste0("lattice_association_rules_", format(Sys.time(), "%Y%m%d_%H%M"), ".rds")
    },
    content = function(file) {
      req(vals$assoc_rules_df)
      saveRDS(vals$assoc_rules_df, file)
    }
  )

  # --- SEMANTIC JOURNEY TIMELINE ---
  output$semanticJourneyTimeline <- renderUI({
    req(vals$fc, vals$navigation_history)
    history_ids <- vals$navigation_history
    if (length(history_ids) == 0) {
      return(p(class="text-muted text-center py-3", "Your semantic journey is empty. Visit concepts to start your timeline!"))
    }
    
    intents_list <- vals$fc$concepts$intents()
    att_names <- clean_latex_names(vals$fc$attributes)
    
    timeline_items <- list()
    
    # Let's add a clear button at the top
    timeline_items[[1]] <- div(
      class = "d-flex justify-content-between align-items-center mb-3 pb-2 border-bottom",
      span(class="fw-bold text-secondary", paste("Steps visited:", length(history_ids))),
      actionButton("btnClearHistory", "Clear Journey", class="btn-xs btn-outline-danger", icon = icon("trash-can"))
    )
    
    for (i in 1:length(history_ids)) {
      curr_id <- history_ids[i]
      
      curr_intent_idx <- which(as.matrix(intents_list[, curr_id]) > 0)
      curr_intent_names <- att_names[curr_intent_idx]
      curr_intent_str <- if(length(curr_intent_names) > 0) paste(curr_intent_names, collapse = ", ") else "{}"
      
      added_str <- ""
      removed_str <- ""
      
      if (i > 1) {
        prev_id <- history_ids[i - 1]
        prev_intent_idx <- which(as.matrix(intents_list[, prev_id]) > 0)
        prev_intent_names <- att_names[prev_intent_idx]
        
        added <- setdiff(curr_intent_names, prev_intent_names)
        removed <- setdiff(prev_intent_names, curr_intent_names)
        
        if (length(added) > 0) {
          added_str <- div(
            class = "mt-1",
            span(class="badge bg-success-subtle text-success me-1", "+ Added:"),
            span(class="text-success small fw-semibold", paste(added, collapse = ", "))
          )
        }
        if (length(removed) > 0) {
          removed_str <- div(
            class = "mt-1",
            span(class="badge bg-danger-subtle text-danger me-1", "- Relaxed:"),
            span(class="text-danger small fw-semibold", paste(removed, collapse = ", "))
          )
        }
      }
      
      isActive <- (curr_id == vals$active_concept_id)
      card_class <- if (isActive) "border-primary bg-primary-subtle shadow-sm" else "border-light"
      
      item_ui <- div(
        class = paste("card mb-2", card_class),
        style = "transition: all 0.2s ease;",
        div(
          class = "card-body p-2",
          div(
            class = "d-flex justify-content-between align-items-center",
            span(class="fw-bold", paste("Step", i, ": Concept", curr_id)),
            tags$a(
              href = "#",
              class = "btn btn-link btn-sm p-0 text-decoration-none",
              onclick = sprintf("Shiny.setInputValue('jump_concept_id', %d, {priority: 'event'}); return false;", curr_id),
              icon("circle-chevron-right"),
              " Jump"
            )
          ),
          div(class="text-muted small mt-1", paste("Profile:", curr_intent_str)),
          added_str,
          removed_str
        )
      )
      
      timeline_items[[i + 1]] <- item_ui
    }
    
    n_items <- length(timeline_items)
    if (n_items > 2) {
      timeline_items[2:n_items] <- rev(timeline_items[2:n_items])
    }
    
    div(style = "max-height: 450px; overflow-y: auto; padding-right: 5px;", timeline_items)
  })

  output$downloadRdsConp <- downloadHandler(filename = function() { "concepts.rds" }, content = function(file) { saveRDS(vals$fc$concepts, file) })

  # --- ADVANCED EXPORT DOWNLOAD HANDLERS ---
  output$downloadSVG <- downloadHandler(
    filename = function() {
      paste0("concept_lattice_", format(Sys.time(), "%Y%m%d_%H%M"), ".svg")
    },
    content = function(file) {
      req(vals$fc)
      # Setup margins and plotting device
      svg(file, width = 10, height = 8)
      op <- par(mar = c(1, 1, 3, 1))
      on.exit({
        par(op)
        dev.off()
      })
      
      tryCatch({
        plot_lattice_static(
          concepts = vals$fc$concepts, 
          fc = vals$fc, 
          layout_val = input$vizLayout,
          filter_ids = current_filtered_ids()
        )
      }, error = function(e) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "")
        text(0.5, 0.5, paste("Error rendering SVG:", e$message))
      })
    }
  )

  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0("concept_lattice_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf")
    },
    content = function(file) {
      req(vals$fc)
      pdf(file, width = 10, height = 8)
      op <- par(mar = c(1, 1, 3, 1))
      on.exit({
        par(op)
        dev.off()
      })
      
      tryCatch({
        plot_lattice_static(
          concepts = vals$fc$concepts, 
          fc = vals$fc, 
          layout_val = input$vizLayout,
          filter_ids = current_filtered_ids()
        )
      }, error = function(e) {
        plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "")
        text(0.5, 0.5, paste("Error rendering PDF:", e$message))
      })
    }
  )

  output$downloadDOT <- downloadHandler(
    filename = function() {
      paste0("concept_lattice_", format(Sys.time(), "%Y%m%d_%H%M"), ".dot")
    },
    content = function(file) {
      req(vals$fc)
      tryCatch({
        dot_code <- generate_dot_code(vals$fc$concepts, vals$fc, filter_ids = current_filtered_ids())
        writeLines(dot_code, file)
      }, error = function(e) {
        writeLines(paste("// Error generating DOT code:", e$message), file)
      })
    }
  )

  output$downloadGraphML <- downloadHandler(
    filename = function() {
      paste0("concept_lattice_", format(Sys.time(), "%Y%m%d_%H%M"), ".graphml")
    },
    content = function(file) {
      req(vals$fc)
      tryCatch({
        scale_node_size <- if (!is.null(input$scale_node_support)) input$scale_node_support else FALSE
        lbl_mode <- if (!is.null(input$labeling_mode)) input$labeling_mode else "reduced"
        g_data <- getGraph(
          vals$fc$concepts, 
          vals$fc, 
          filter_ids = current_filtered_ids(),
          scale_support = scale_node_size,
          labeling_mode = lbl_mode
        )
        nodes <- g_data$nodes
        edges <- g_data$edges
        
        # Create igraph
        if (nrow(edges) > 0) {
          g_igraph <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
        } else {
          g_igraph <- igraph::make_empty_graph(n = nrow(nodes))
          igraph::V(g_igraph)$name <- nodes$id
        }
        
        # Add attributes to vertices for Gephi to recognize
        igraph::V(g_igraph)$label <- nodes$label
        igraph::V(g_igraph)$concept_id <- nodes$id
        igraph::V(g_igraph)$color <- nodes$color.background
        
        # Write to file
        igraph::write_graph(g_igraph, file, format = "graphml")
      }, error = function(e) {
        writeLines(paste0("<!-- Error generating GraphML: ", e$message, " -->"), file)
      })
    }
  )

  observeEvent(input$createLatexConcepts, {
    req(vals$fc)
    showModal(modalDialog(
      title = "LaTeX Premium Export",
      size = "l",
      easyClose = TRUE,
      tabsetPanel(
        tabPanel("Concept Lattice (TikZ)",
                 p(class="text-muted small mt-2", "This generates native TikZ graphical code for the currently active concept lattice (respecting your current filters)."),
                 div(style="position:relative;",
                     verbatimTextOutput("latexTikzCodeText"),
                     div(class="text-end mt-2",
                         downloadButton("downloadTikzFile", "Download .tex", class="btn-primary btn-sm")
                     )
                 )
        ),
        tabPanel("Formal Context (Tabular)",
                 p(class="text-muted small mt-2", "This generates a clean tabular LaTeX code representing the formal context incidence matrix."),
                 div(style="position:relative;",
                     verbatimTextOutput("latexTabularCodeText"),
                     div(class="text-end mt-2",
                         downloadButton("downloadTabularFile", "Download .tex", class="btn-primary btn-sm")
                     )
                 )
        ),
        tabPanel("Raw Concept List",
                 p(class="text-muted small mt-2", "The standard fcaR representation of concepts in LaTeX format."),
                 div(style="position:relative;",
                     verbatimTextOutput("latexRawConceptsText"),
                     div(class="text-end mt-2",
                         downloadButton("downloadRawConceptsFile", "Download .tex", class="btn-primary btn-sm")
                     )
                 )
        )
      ),
      footer = modalButton("Close")
    ))
  })

  # Renderers for the LaTeX Modal
  output$latexTikzCodeText <- renderText({
    req(vals$fc)
    lattice_to_tikz(concepts = vals$fc$concepts, fc = vals$fc, layout_val = input$vizLayout, filter_ids = current_filtered_ids())
  })

  output$latexTabularCodeText <- renderText({
    req(vals$fc)
    context_to_latex_tabular(vals$fc)
  })

  output$latexRawConceptsText <- renderText({
    req(vals$fc)
    vals$fc$concepts$to_latex()
  })

  # Download Handlers for the LaTeX snippets
  output$downloadTikzFile <- downloadHandler(
    filename = function() { "concept_lattice_tikz.tex" },
    content = function(file) {
      writeLines(lattice_to_tikz(concepts = vals$fc$concepts, fc = vals$fc, layout_val = input$vizLayout, filter_ids = current_filtered_ids()), file)
    }
  )

  output$downloadTabularFile <- downloadHandler(
    filename = function() { "formal_context_tabular.tex" },
    content = function(file) {
      writeLines(context_to_latex_tabular(vals$fc), file)
    }
  )

  output$downloadRawConceptsFile <- downloadHandler(
    filename = function() { "raw_concepts_list.tex" },
    content = function(file) {
      writeLines(vals$fc$concepts$to_latex(), file)
    }
  )

  # ===========================================================================
  # 5. IMPLICACIONES
  # ===========================================================================

  # 1. Compute Canonical Basis Option (from formal context)
  observeEvent(input$optComputeBasis, {
    req(vals$fc)
    withProgress(message = "Computing canonical basis...", value = 0.5, {
      tryCatch({
        run_canonical_basis(from_context = TRUE)
        shinyalert("Success", "Canonical basis computed successfully.", type = "success")
      }, error = function(e) {
        shinyalert("Error", e$message, type = "error")
      })
    })
  })

  # 2. Implication Mining Option (Show Modal matching arules2fcaR design)
  observeEvent(input$optMineImps, {
    req(vals$fc)
    showModal(modalDialog(
      title = "Mine Implications (arules apriori)",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btnExecuteMining", "Run Mining", class = "btn-primary", icon = icon("play"))
      ),
      fluidPage(
        p("Mine rules from the formal context using association rule algorithms. Mined rules will automatically have their confidence set to 1.0 and be converted into fcaR implications."),
        sliderInput("mineSupp", "Min Support:", min = 0.01, max = 1, value = 0.05, step = 0.01),
        layout_columns(
          col_widths = c(6, 6),
          numericInput("mineMinLen", "Min Len:", value = 1, min = 1),
          numericInput("mineMaxLen", "Max Len:", value = 10, min = 1)
        ),
        div(class="alert alert-info small p-2 mb-2", "Confidence fixed at 1.0 (Implications).")
      )
    ))
  })

  # Execute Implication Mining
  observeEvent(input$btnExecuteMining, {
    req(vals$fc)
    removeModal()
    withProgress(message = "Mining implications...", value = 0.5, {
      tryCatch({
        mat <- t(as.matrix(vals$fc$I))
        trans <- as(mat, "transactions")
        params <- list(supp = input$mineSupp, conf = 1.0, minlen = input$mineMinLen, maxlen = input$mineMaxLen)
        
        rules_found <- apriori(trans, parameter = params, control = list(verbose = FALSE))
        
        n_rules <- if (is.null(rules_found)) 0L else length(rules_found)
        if (n_rules > 0) {
          vals$fc$implications <- convert_rules_to_fcaR_safe(rules_found, vals$fc)
          vals$stats_orig <- list(
            count = vals$fc$implications$cardinality(),
            size_lhs = sum(vals$fc$implications$get_LHS_matrix()),
            size_rhs = sum(vals$fc$implications$get_RHS_matrix())
          )
          vals$filtered_imps <- vals$fc$implications
          implications_df(get_implications_dataframe(vals$fc$implications))
          vals$trigger <- vals$trigger + 1
          shinyalert("Success", paste("Successfully mined", length(rules_found), "implications."), type = "success")
        } else {
          shinyalert("No Rules", "No implications satisfied the specified support threshold.", type = "warning")
        }
      }, error = function(e) {
        shinyalert("Mining Error", e$message, type = "error")
      })
    })
  })

  # 3. Import from File Option (Show Modal)
  observeEvent(input$optImportImplicationsFile, {
    req(vals$fc)
    showModal(modalDialog(
      title = "Import Implications from File (.rds)",
      easyClose = TRUE,
      footer = tagList(modalButton("Cancel")),
      fileInput("fileImportImps", "Select RDS File (ImplicationSet or arules rules):", accept = c(".rds"))
    ))
  })

  # Execute File Import
  observeEvent(input$fileImportImps, {
    req(input$fileImportImps, vals$fc)
    removeModal()
    tryCatch({
      obj <- readRDS(input$fileImportImps$datapath)
      
      if (inherits(obj, "ImplicationSet")) {
        vals$fc$implications <- obj
        shinyalert("Success", "Loaded ImplicationSet from RDS file.", type = "success")
      } else if (inherits(obj, "rules")) {
        vals$fc$implications <- convert_rules_to_fcaR_safe(obj, vals$fc)
        shinyalert("Success", "Loaded and converted arules rules from RDS file.", type = "success")
      } else {
        stop("Unsupported file object. File must contain an ImplicationSet (fcaR) or rules (arules).")
      }
      
      vals$stats_orig <- list(
        count = vals$fc$implications$cardinality(),
        size_lhs = sum(vals$fc$implications$get_LHS_matrix()),
        size_rhs = sum(vals$fc$implications$get_RHS_matrix())
      )
      vals$filtered_imps <- vals$fc$implications
      implications_df(get_implications_dataframe(vals$fc$implications))
      vals$trigger <- vals$trigger + 1
    }, error = function(e) {
      shinyalert("Import Error", e$message, type = "error")
    })
  })

  # 4. Attribute Exploration Option (Show modal exploration wizard)
  observeEvent(input$optAttrExploration, {
    req(vals$fc)
    showModal(modalDialog(
      title = span(icon("route", class="text-primary"), " Interactive Attribute Exploration (Ganter's Algorithm)"),
      size = "xl",
      easyClose = FALSE,
      footer = tagList(
        actionButton("btnResetExplorationModal", "Reset Exploration", class = "btn-outline-danger", icon = icon("rotate")),
        modalButton("Close")
      ),
      fluidPage(
        layout_columns(
          col_widths = c(4, 8),
          
          # --- COLUMNA IZQUIERDA: ESTADO Y CONTROL ---
          card(
            card_header("Control Panel", class="bg-primary text-white"),
            height = "100%",
            
            div(class="p-2 bg-light border rounded mb-3 text-center",
                h5("Status", class="fw-bold text-muted small uppercase mb-1"),
                uiOutput("explorationStatusBadge")
            ),
            
            # PANEL DE INICIO
            conditionalPanel(
              condition = "!output.isExplorationActive",
              p(class="text-muted small", 
                "Attribute Exploration is a systematic method based on Bernhard Ganter and Rudolf Wille's Formal Concept Analysis (1999) for interactively acquiring domain knowledge. The system will ask you about implications that hold in your current dataset, and you will confirm them or provide counterexamples."),
              actionButton("btnStartExploration", "Start Knowledge Acquisition", 
                           icon = icon("play"), class = "btn-success w-100 btn-lg")
            ),
            
            # PANEL DE ESTADÍSTICAS ACTIVAS
            conditionalPanel(
              condition = "output.isExplorationActive",
              h6("Acquired Domain Rules", class="text-primary fw-bold mt-2"),
              uiOutput("confirmedRulesCount"),
              hr(),
              h6("Refuted Implications", class="text-danger fw-bold"),
              uiOutput("refutedRulesCount"),
              hr(),
              p(class="text-muted small italic", 
                icon("info-circle"), " Confirmed rules are saved permanently. Counterexamples automatically update your formal context matrix by adding new objects.")
            )
          ),

          # --- COLUMNA DERECHA: INTERACCIÓN Y PREGUNTAS ---
          card(
            card_header("Attribute Knowledge Wizard"),
            height = "580px",
            
            # CASO 1: NO ACTIVO
            conditionalPanel(
              condition = "!output.isExplorationActive",
              div(class="text-center py-5",
                  icon("brain", class="fa-5x text-muted mb-4"),
                  h4("Ready to Acquire Knowledge", class="fw-bold"),
                  p(class="text-muted", "Click the 'Start Knowledge Acquisition' button on the left to begin the interactive exploration session.")
              )
            ),
            
            # CASO 2: ACTIVO Y PREGUNTA DISPONIBLE (NO REFUTANDO)
            conditionalPanel(
              condition = "output.isExplorationActive && !output.isExplorationComplete && !output.isRefutingActive",
              div(class="p-4 border border-primary rounded bg-light mb-4", style="min-height: 220px;",
                  h5(class="text-primary fw-bold mb-3", icon("question-circle"), " Proposed Domain Implication:"),
                  uiOutput("questionText"),
                  hr(),
                  p(class="text-muted small", "Does this implication hold universally in your domain?")
              ),
              
              layout_columns(
                col_widths = c(6, 6),
                actionButton("btnConfirmRule", "Yes, it is a Domain Law", 
                             icon = icon("check-circle"), class = "btn-success btn-lg d-block w-100 py-3"),
                actionButton("btnRefuteRule", "No, Refute with Counterexample", 
                             icon = icon("times-circle"), class = "btn-danger btn-lg d-block w-100 py-3")
              )
            ),
            
            # CASO 3: EXPLORACIÓN COMPLETADA
            conditionalPanel(
              condition = "output.isExplorationComplete",
              div(class="text-center py-5",
                  icon("circle-check", class="fa-5x text-success mb-4"),
                  h3("Exploration Complete!", class="fw-bold text-success"),
                  p(class="text-muted fs-5 px-4", 
                    "All implications in the canonical basis of your formal context have been successfully evaluated. Your domain rules are fully established!"),
                  hr(class="my-4"),
                  h5("Confirmed Rules:", class="fw-bold text-start px-4"),
                  div(style="max-height: 180px; overflow-y: auto; text-align: left; padding: 0 25px;", 
                      uiOutput("listConfirmedRules"))
              )
            ),
            
            # PANEL DE REFUTACIÓN (COUNTEREXAMPLE INLINE)
            conditionalPanel(
              condition = "output.isRefutingActive",
              div(class="p-3 border border-danger rounded bg-light mb-3",
                  h5(class="text-danger fw-bold mb-2", icon("times-circle"), " Provide Counterexample"),
                  p(class="text-muted small mb-2", 
                    "A valid counterexample must possess ALL attributes in the Premise (LHS), but MUST LACK at least one attribute in the Conclusion (RHS)."),
                  div(class="p-2 bg-white border rounded",
                      tags$strong("Rule being refuted:"), br(),
                      uiOutput("refutingRuleKeyText")
                  )
              ),
              textInput("cex_name", "Name of the new Object", value = "", width = "100%"),
              div(style = "max-height: 150px; overflow-y: auto; margin-bottom: 15px;",
                  uiOutput("cexAttributesSelectorUI")
              ),
              layout_columns(
                col_widths = c(6, 6),
                actionButton("btnCancelRefute", "Cancel", class = "btn-outline-secondary btn-lg d-block w-100"),
                actionButton("btnSaveCex", "Save Counterexample", class = "btn-danger btn-lg d-block w-100")
              )
            )
          )
        )
      )
    ))
  })

  # Handle the reset from exploration modal button
  observeEvent(input$btnResetExplorationModal, {
    shinyjs::click("btnResetExploration")
  })

  # --- IMPLICATIONS PRESENCE TRACKING & BUTTON STATE ---
  output$has_implications <- reactive({
    vals$trigger
    req(vals$fc)
    tryCatch(vals$fc$implications$cardinality() > 0, error = function(e) FALSE)
  })
  outputOptions(output, "has_implications", suspendWhenHidden = FALSE)

  observe({
    vals$trigger
    req(vals$fc)
    has_imps <- tryCatch(vals$fc$implications$cardinality() > 0, error = function(e) FALSE)
    if (has_imps) {
      shinyjs::enable("createLatexImplications")
      shinyjs::enable("downloadRdsImp")
    } else {
      shinyjs::disable("createLatexImplications")
      shinyjs::disable("downloadRdsImp")
    }
  })


  output$dynamicRulesUI <- renderUI({ available_rules <- tryCatch(fcaR::equivalencesRegistry$get_entry_names(), error = function(e) c("Composition", "Generalization", "Simplification", "Reduction")); checkboxGroupInput("selectRulesImplications", "Equivalence Rules:", choices = c("ALL RULES"="all", setNames(available_rules, available_rules)), selected = c("Composition"), inline = TRUE, width = "100%") })
  output$fcImplications <- renderTable({ if (isTRUE(input$ignoreFilters)) { req(vals$fc); get_implications_dataframe(vals$fc$implications) } else { req(implications_df()); implications_df() } })
  output$fcImplicationsTop <- renderTable({ req(implications_df()); implications_df() })
  observeEvent(input$btnApplyFilters, { req(vals$fc); imp <- vals$fc$implications; if(input$support > 0) { indx <- which(vals$fc$implications$support() >= input$support); imp <- vals$fc$implications[indx] }; filtered <- imp$filter(lhs = c(input$selectLHS), not_lhs = c(input$selectNotLHS), rhs = c(input$selectRHS), not_rhs = c(input$selectNotRHS)); vals$filtered_imps <- filtered; implications_df(get_implications_dataframe(filtered)) })
  observeEvent(input$btnClearFilters, {
    req(vals$fc)
    updateSelectInput(session, "selectLHS", selected = character(0))
    updateSelectInput(session, "selectNotLHS", selected = character(0))
    updateSelectInput(session, "selectRHS", selected = character(0))
    updateSelectInput(session, "selectNotRHS", selected = character(0))
    updateSliderInput(session, "support", value = 0)
    updateNumericInput(session, "minLift", value = 1.0)
    vals$filtered_imps <- vals$fc$implications
    implications_df(get_implications_dataframe(vals$fc$implications))
  })
  # --- BASE COMPUTATION TAB HANDLERS ---
  observeEvent(input$btnComputeBasis, {
    req(vals$fc)
    withProgress(message = "Reducing to canonical basis...", value = 0.5, {
      tryCatch({
        run_canonical_basis(from_context = FALSE)
        shinyalert("Success", "Implication set reduced to canonical basis.", type = "success")
      }, error = function(e) {
        shinyalert("Error", paste("Failed to compute canonical basis:", e$message), type = "error")
      })
    })
  })

  observeEvent(input$btnComputeDirectOptimal, {
    req(vals$fc)
    if (is.null(vals$fc$implications) || vals$fc$implications$cardinality() == 0) {
      shinyalert("No Implications", "Please compute implications first.", type = "warning")
      return()
    }
    withProgress(message = "Computing direct-optimal system...", value = 0.5, {
      tryCatch({
        # Correct assignment: to_direct_optimal() returns a new ImplicationSet, assign it back
        vals$fc$implications <- vals$fc$implications$to_direct_optimal()
        vals$trigger <- vals$trigger + 1
        vals$filtered_imps <- vals$fc$implications
        implications_df(get_implications_dataframe(vals$fc$implications))
        shinyalert("Success", "Direct-optimal system computed successfully.", type = "success")
      }, error = function(e) {
        shinyalert("Error", paste("Failed to compute direct-optimal system:", e$message), type = "error")
      })
    })
  })

  observeEvent(input$btnApplyLogic, { req(vals$fc, input$selectRulesImplications); rules_sel <- input$selectRulesImplications; avail <- fcaR::equivalencesRegistry$get_entry_names(); to_apply <- if ("all" %in% rules_sel) avail else intersect(rules_sel, avail); withProgress(message = "Applying Rules...", value = 0.5, { if(length(to_apply) > 0){ vals$fc$implications$apply_rules(rules = to_apply, batch_size = input$batchSize, parallelize = FALSE, reorder = isTRUE(input$reorder)); vals$trigger <- vals$trigger + 1; vals$filtered_imps <- vals$fc$implications; implications_df(get_implications_dataframe(vals$fc$implications)); showNotification("Rules applied.", type = "message") } }) })
  output$logicStats <- renderUI({ vals$trigger; req(vals$fc); imp <- vals$fc$implications; tagList(h4(paste("Rules:", imp$cardinality())), p(class="text-muted", paste("Original:", if(!is.null(vals$stats_orig)) vals$stats_orig$count else imp$cardinality()))) })
  output$implicationsForClosurePreview <- renderTable({ if(isTRUE(input$ignoreFiltersClosure)) { req(vals$fc); get_implications_dataframe(vals$fc$implications) } else { req(implications_df()); implications_df() } })
  closure_result <- reactiveVal(NULL); get_imp_set_closure <- function() { if(isTRUE(input$ignoreFiltersClosure)) return(vals$fc$implications); if(!is.null(vals$filtered_imps)) return(vals$filtered_imps); return(vals$fc$implications) }
  observeEvent(input$btnComputeClosure, { req(vals$fc, input$selectClosure); S <- Set$new(vals$fc$attributes); sapply(input$selectClosure, function(x){ do.call(S$assign, setNames(list(1), x)) }); closure_result(get_imp_set_closure()$closure(S, reduce = TRUE)); nav_select("closure_results_tabs", selected = "Closure (Set)") })
  output$closureResultText <- renderText({ req(closure_result()); paste(capture.output(print(closure_result()$closure)), collapse = "\n") })
  output$closureReducedImplications <- renderTable({ req(closure_result()); get_implications_dataframe(closure_result()$implications) })
  output$latexImplicationsText <- renderText({
    req(vals$fc)
    vals$fc$implications$to_latex()
  })
  observeEvent(input$createLatexImplications, {
    req(vals$fc)
    showModal(modalDialog(
      title = "Implications LaTeX",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(style = "max-height: 65vh; overflow-y: auto;", verbatimTextOutput("latexImplicationsText"))
    ))
  })
  output$downloadRdsImp <- downloadHandler(filename = function() { "implications.rds" }, content = function(file) { saveRDS(vals$fc$implications, file) })
  output$validationReportUI <- renderUI({
    div(class="text-center py-5 text-muted",
        icon("flask", class="fa-4x mb-3"),
        h5("Ready for Implication Verification"),
        p("Select LHS and RHS attributes on the left panel, and click 'Check Implication' to run the hypothesis testing report.")
    )
  })

  observeEvent(input$btnValidate, {
    req(vals$fc, input$hypLHS, input$hypRHS)
    # Correct R6 Set assignment for premise
    LHS <- Set$new(vals$fc$attributes)
    LHS$assign(attributes = input$hypLHS, values = rep(1, length(input$hypLHS)))
    
    # Compute LHS closure under implications
    res_clo <- vals$fc$implications$closure(LHS)
    clo_atts <- vals$fc$attributes[as.numeric(res_clo$closure$get_vector()) > 0]
    
    # Check if the implication holds (if RHS is a subset of LHS closure)
    holds <- all(input$hypRHS %in% clo_atts)
    # Find missing attributes in RHS
    missing_atts <- setdiff(input$hypRHS, clo_atts)
    
    # Find counterexamples in context objects
    mat <- as.matrix(vals$fc$I)
    objs <- vals$fc$objects
    cex_list <- list()
    
    if (length(objs) > 0) {
      for (o in objs) {
        # Check if o has all LHS attributes
        has_lhs <- all(mat[input$hypLHS, o, drop = FALSE] == 1)
        # Check if o lacks at least one RHS attribute
        lacks_rhs <- any(mat[input$hypRHS, o, drop = FALSE] == 0)
        
        if (has_lhs && lacks_rhs) {
          # Get which RHS attributes are lacked by this object
          lacked_rhs <- input$hypRHS[mat[input$hypRHS, o, drop = FALSE] == 0]
          cex_list[[length(cex_list) + 1]] <- data.frame(
            Object = o,
            Lacked = paste(lacked_rhs, collapse = ", "),
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    output$validationReportUI <- renderUI({
      if (holds) {
        card(
          class = "border-success shadow-sm mb-3",
          card_header(class = "bg-success-subtle text-success d-flex align-items-center",
                      icon("circle-check", class="me-2 fs-4"), 
                      span("Hypothesis Holds!", class="fw-bold fs-5")),
          card_body(
            p(class="lead text-success", "This implication rule is mathematically valid in your current system."),
            p("Any object that has the premise attributes is guaranteed to also have the conclusion attributes."),
            hr(),
            h6("Proof Details", class="fw-bold text-muted small uppercase mb-2"),
            p("The closure of your premise contains all attributes of your conclusion:"),
            div(class="p-3 bg-light border rounded",
                tags$strong("Premise (LHS): "), br(),
                lapply(input$hypLHS, function(a) span(a, class="badge bg-dark me-1")),
                br(), br(),
                tags$strong("Closure(LHS): "), br(),
                lapply(clo_atts, function(a) {
                  is_in_rhs <- a %in% input$hypRHS
                  span(a, class = paste("badge me-1", if(is_in_rhs) "bg-success" else "bg-secondary"))
                })
            )
          )
        )
      } else {
        # Implication does not hold
        cex_table <- NULL
        if (length(cex_list) > 0) {
          df_cex <- do.call(rbind, cex_list)
          cex_table <- div(
            class = "table-responsive mt-3",
            tags$table(
              class = "table table-striped table-hover table-sm border",
              tags$thead(
                tags$tr(
                  tags$th("Counterexample Object"),
                  tags$th("Lacked Conclusion Attributes")
                )
              ),
              tags$tbody(
                lapply(1:nrow(df_cex), function(row_idx) {
                  tags$tr(
                    tags$td(tags$strong(df_cex$Object[row_idx])),
                    tags$td(
                      lapply(strsplit(df_cex$Lacked[row_idx], ", ")[[1]], function(a) {
                        span(a, class="badge bg-danger me-1")
                      })
                    )
                  )
                })
              )
            )
          )
        } else {
          cex_table <- div(class="alert alert-info py-2 small mt-3", 
                           icon("info-circle"), " No counterexamples were found in the current formal context objects, but the implication fails due to the global rules of the system.")
        }
        
        card(
          class = "border-danger shadow-sm mb-3",
          card_header(class = "bg-danger-subtle text-danger d-flex align-items-center",
                      icon("circle-xmark", class="me-2 fs-4"), 
                      span("Hypothesis Fails!", class="fw-bold fs-5")),
          card_body(
            p(class="lead text-danger", "This implication rule does not hold in the current system."),
            p("There are cases where the premise holds but the conclusion does not."),
            hr(),
            h6("Failing Attributes", class="fw-bold text-muted small uppercase mb-2"),
            p("The following attributes from the conclusion are not implied by the premise closure:"),
            div(class="p-2 bg-light border rounded mb-3",
                lapply(missing_atts, function(a) span(a, class="badge bg-danger me-1 fs-6"))
            ),
            h6("Counterexamples in Data", class="fw-bold text-muted small uppercase mb-2"),
            p("We scanned your formal context and found these object counterexamples:"),
            cex_table
          )
        )
      }
    })
  })

  # ===========================================================================
  # 7. RULE VISUALIZATION (fcaR implications → arules)
  # ===========================================================================
  arules_data <- reactive({
    vals$trigger
    req(vals$fc)
    if(is.null(vals$fc$implications)) return(NULL)
    n <- tryCatch(vals$fc$implications$cardinality(), error=function(e) -1)
    if(n<=0) return(NULL)
    tryCatch({ vals$fc$implications$to_arules() }, error=function(e) NULL)
  })
  arules_filtered <- reactive({
    req(arules_data())
    rules <- arules_data()
    if (!is.null(input$selectLHS)) rules <- subset(rules, subset = lhs %ain% input$selectLHS)
    if (!is.null(input$selectNotLHS)) rules <- subset(rules, subset = !(lhs %in% input$selectNotLHS))
    if (!is.null(input$selectRHS)) rules <- subset(rules, subset = rhs %ain% input$selectRHS)
    if (!is.null(input$selectNotRHS)) rules <- subset(rules, subset = !(rhs %in% input$selectNotRHS))
    if (!is.null(input$minLift) && input$minLift > 0) rules <- subset(rules, subset = lift >= input$minLift)
    if (!is.null(input$support) && input$support > 0) rules <- subset(rules, subset = support >= input$support)
    return(rules)
  })
  output$arulesPlotContainer <- renderUI({
    rules <- tryCatch(arules_filtered(), error=function(e) NULL)
    if(is.null(rules)) return(div(class="alert alert-info", "Compute implications first."))
    if(length(rules) == 0) return(div(class="alert alert-warning", "No rules satisfy filters."))
    
    if (input$arulesPlotType == "graph") {
      visNetworkOutput("arulesVisNet", height = "600px")
    } else if (input$arulesPlotType == "scatterplot") {
      plotly::plotlyOutput("arulesScatterplot", height = "600px")
    } else {
      plotly::plotlyOutput("arulesPlotly", height = "600px")
    }
  })
  output$arulesVisNet <- renderVisNetwork({
    req(arules_filtered())
    req(input$arulesPlotType == "graph")
    req(input$netColorMetric, input$netSizeMetric)
    rules <- arules_filtered()
    if(length(rules)>100) rules <- head(rules, n=100, by="lift")
    tryCatch({
      vn <- plot(rules, method="graph", engine="htmlwidget")
      nodes <- vn$x$nodes
      is_rule <- nodes$group == 2
      if (any(is_rule)) {
        rule_indices <- as.numeric(gsub("rule ", "", nodes$label[is_rule]))
        q <- quality(rules)[rule_indices, ]
        
        # 1. Custom Size Metric
        size_metric <- if (input$netSizeMetric == "support") q$support else q$lift
        size_metric[is.na(size_metric) | is.nan(size_metric) | !is.finite(size_metric)] <- 0
        if (diff(range(size_metric)) == 0) {
          norm_size <- rep(25, length(size_metric))
        } else {
          norm_size <- 10 + 30 * (size_metric - min(size_metric)) / diff(range(size_metric))
        }
        nodes$value[is_rule] <- norm_size
        
        # 2. Custom Color Metric
        color_metric <- if (input$netColorMetric == "support") q$support else q$lift
        color_metric[is.na(color_metric) | is.nan(color_metric) | !is.finite(color_metric)] <- 0
        pal <- colorRampPalette(c("#FED976", "#FD8D3C", "#E31A1C"))(100)
        if (diff(range(color_metric)) == 0) {
          nodes$color[is_rule] <- pal[50]
        } else {
          color_idx <- round(1 + 99 * (color_metric - min(color_metric)) / diff(range(color_metric)))
          nodes$color[is_rule] <- pal[color_idx]
        }
      }
      vn$x$nodes <- nodes
      
      vn %>% visNetwork::visOptions(
        nodesIdSelection = list(enabled = TRUE, main = "Select attribute or implication number"),
        highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE, labelOnly = FALSE)
      )
    }, error = function(e) NULL)
  })
  output$netGraphLegendUI <- renderUI({
    req(arules_filtered(), input$netColorMetric, input$netSizeMetric)
    rules <- arules_filtered()
    if(length(rules)>100) rules <- head(rules, n=100, by="lift")
    q <- quality(rules)
    
    color_label <- if (input$netColorMetric == "support") "Support" else "Lift"
    size_label <- if (input$netSizeMetric == "support") "Support" else "Lift"
    
    color_val <- if (input$netColorMetric == "support") q$support else q$lift
    size_val <- if (input$netSizeMetric == "support") q$support else q$lift
    color_val[is.na(color_val) | is.nan(color_val) | !is.finite(color_val)] <- 0
    size_val[is.na(size_val) | is.nan(size_val) | !is.finite(size_val)] <- 0
    
    min_c <- min(color_val)
    max_c <- max(color_val)
    min_s <- min(size_val)
    max_s <- max(size_val)
    
    color_range_text <- if(min_c == max_c) paste(round(min_c, 3)) else paste0(round(min_c, 3), " - ", round(max_c, 3))
    size_range_text <- if(min_s == max_s) paste(round(min_s, 3)) else paste0(round(min_s, 3), " - ", round(max_s, 3))
    
    tagList(
      hr(),
      h6("Legend", class="text-muted small mb-2"),
      div(class="d-flex align-items-center mb-2",
          span(style="width:20px; height:20px; background-color:#CBD2FC; border:1px solid #c0c0c0; display:inline-block; margin-right:8px; border-radius:3px;"),
          span("Attributes / Items", class="small text-muted")
      ),
      div(class="d-flex align-items-center mb-2",
          span(style="width:20px; height:20px; background-color:#FD8D3C; border-radius:50%; display:inline-block; margin-right:8px;"),
          span(paste("Rules (Color:", color_label, ")"), class="small text-muted")
      ),
      div(class="ps-4 small text-muted",
          div(style="height:8px; background:linear-gradient(90deg, #FED976 0%, #FD8D3C 50%, #E31A1C 100%); border-radius:3px; margin-bottom:4px;"),
          div(class="d-flex justify-content-between",
              span(round(min_c, 3)),
              span(round(max_c, 3))
          )
      ),
      div(class="d-flex align-items-center mt-2",
          span(class="small me-2 text-muted", paste("Rule Size:", size_label)),
          span(class="badge bg-secondary", size_range_text)
      )
    )
  })
  output$arulesScatterplot <- plotly::renderPlotly({
    req(arules_filtered())
    req(input$arulesPlotType == "scatterplot")
    req(input$scatterXMetric, input$scatterYMetric, input$scatterColorMetric, input$scatterSizeMetric)
    rules <- arules_filtered()
    
    # Extract quality metrics
    q <- quality(rules)
    lhs_size <- size(lhs(rules))
    rhs_size <- size(rhs(rules))
    
    # Helper to map inputs to metrics
    get_metric_values <- function(metric_name) {
      val <- if (metric_name == "support") {
        q$support
      } else if (metric_name == "lift") {
        q$lift
      } else if (metric_name == "lhs_size") {
        lhs_size
      } else if (metric_name == "rhs_size") {
        rhs_size
      } else {
        q$support
      }
      val[is.na(val) | is.nan(val) | !is.finite(val)] <- 0
      return(val)
    }
    
    x_val <- get_metric_values(input$scatterXMetric)
    y_val <- get_metric_values(input$scatterYMetric)
    color_val <- get_metric_values(input$scatterColorMetric)
    size_val <- get_metric_values(input$scatterSizeMetric)
    
    # Scale marker sizes nicely between 8 and 28
    scaled_size <- 8 + 20 * (size_val - min(size_val)) / max(c(diff(range(size_val)), 0.0001))
    
    # Generate clean labels for tooltip
    df_rules <- as(rules, "data.frame")
    rule_labels <- gsub("\\{", "", df_rules$rules)
    rule_labels <- gsub("\\}", "", rule_labels)
    rule_labels <- gsub("=>", " \u2192 ", rule_labels)
    
    # Build data frame for Plotly
    plot_df <- data.frame(
      Rule = rule_labels,
      X_Val = x_val,
      Y_Val = y_val,
      Color_Val = color_val,
      Scaled_Size = scaled_size,
      Support = q$support,
      Lift = q$lift,
      LHS_Size = lhs_size,
      RHS_Size = rhs_size,
      stringsAsFactors = FALSE
    )
    
    # Proper labels for metrics in colorbar/axes
    metric_label <- function(m) {
      if (m == "support") return("Support")
      if (m == "lift") return("Lift")
      if (m == "lhs_size") return("LHS Size")
      if (m == "rhs_size") return("RHS Size")
      return(m)
    }
    
    # Generate scatterplot using plotly
    p <- plotly::plot_ly(
      data = plot_df,
      x = ~X_Val,
      y = ~Y_Val,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = ~Scaled_Size,
        color = ~Color_Val,
        colorscale = "Viridis",
        colorbar = list(title = metric_label(input$scatterColorMetric)),
        opacity = 0.8,
        line = list(color = "#ffffff", width = 1)
      ),
      text = ~paste0(
        "<b>Rule:</b> ", Rule, "<br>",
        "<b>Support:</b> ", round(Support, 4), "<br>",
        "<b>Lift:</b> ", round(Lift, 2), "<br>",
        "<b>LHS Size:</b> ", LHS_Size, "<br>",
        "<b>RHS Size:</b> ", RHS_Size
      ),
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        title = paste("Rules Explorer (", metric_label(input$scatterXMetric), "vs", metric_label(input$scatterYMetric), ")"),
        xaxis = list(title = metric_label(input$scatterXMetric), gridcolor = "#f3f3f3"),
        yaxis = list(title = metric_label(input$scatterYMetric), gridcolor = "#f3f3f3"),
        hoverlabel = list(bgcolor = "#2c3e50", font = list(color = "#ffffff")),
        margin = list(t = 50, b = 50, l = 50, r = 50)
      )
    
    return(p)
  })
  output$arulesPlotly <- plotly::renderPlotly({
    req(arules_filtered())
    req(input$arulesPlotType == "grouped")
    rules <- arules_filtered()
    
    # 1. Group rules by their closure
    arules_labels <- rules@lhs@itemInfo$labels
    fcar_labels <- vals$fc$attributes
    lhs_data <- as(rules@lhs@data, "ngCMatrix")
    
    rule_closures <- list()
    for (i in 1:length(rules)) {
      active_indices <- which(lhs_data[, i])
      active_labels <- arules_labels[active_indices]
      valid_labels <- intersect(active_labels, fcar_labels)
      
      S <- Set$new(fcar_labels)
      if (length(valid_labels) > 0) {
        S$assign(attributes = valid_labels, values = rep(1, length(valid_labels)))
      }
      S_clo <- vals$fc$implications$closure(S)
      vec_clo <- as.numeric(S_clo$closure$get_vector())
      rule_closures[[i]] <- fcar_labels[vec_clo > 0]
    }
    
    closure_keys <- sapply(rule_closures, function(x) paste(sort(x), collapse = ", "))
    unique_closures <- unique(closure_keys)
    
    # Build data points by iterating over unique closures
    plot_data_list <- list()
    for (idx in 1:length(unique_closures)) {
      key <- unique_closures[idx]
      matching_rules_idx <- which(closure_keys == key)
      
      # For this group, collect all (attribute, support, lift, rule_str)
      group_rules <- rules[matching_rules_idx]
      q <- quality(group_rules)
      
      # Clean rule representations
      df_rules <- as(group_rules, "data.frame")
      rule_formulas <- gsub("\\{", "", df_rules$rules)
      rule_formulas <- gsub("\\}", "", rule_formulas)
      rule_formulas <- gsub("=>", " \u2192 ", rule_formulas)
      
      # Group LHS description for Y-axis (to show which premises lead to this closure)
      lhs_strings <- sapply(1:length(group_rules), function(r_idx) {
        lhs_atts <- arules_labels[which(as(group_rules@lhs@data, "ngCMatrix")[, r_idx])]
        paste0("{", paste(lhs_atts, collapse = ", "), "}")
      })
      unique_lhs_desc <- paste(unique(lhs_strings), collapse = " / ")
      
      # Y-axis label: Group name + its rule count (to prevent cutoffs)
      y_label <- paste0("Group ", idx, " (", length(matching_rules_idx), " rules)")
      
      # For each attribute in the closure, check if it is implied by any rule in this group
      for (att in rule_closures[[matching_rules_idx[1]]]) {
        rules_implying <- c()
        supps <- c()
        lifts <- c()
        
        for (r_idx in 1:length(group_rules)) {
          rhs_atts <- arules_labels[which(as(group_rules@rhs@data, "ngCMatrix")[, r_idx])]
          if (att %in% rhs_atts) {
            rules_implying <- c(rules_implying, rule_formulas[r_idx])
            supps <- c(supps, q$support[r_idx])
            lifts <- c(lifts, q$lift[r_idx])
          }
        }
        
        # If at least one rule implies this attribute, create a bubble point
        if (length(rules_implying) > 0) {
          plot_data_list[[length(plot_data_list) + 1]] <- data.frame(
            Attribute = att,
            Group = y_label,
            Closure = paste0("{", key, "}"),
            LHS = unique_lhs_desc,
            Support = mean(supps),
            Lift = mean(lifts),
            Rules = paste(rules_implying, collapse = "<br>"),
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if (length(plot_data_list) == 0) {
      return(plotly::plot_ly() %>% plotly::layout(title = "No implications to display"))
    }
    
    plot_df <- do.call(rbind, plot_data_list)
    
    # Scale marker sizes nicely between 10 and 35 based on support
    scaled_size <- 10 + 25 * (plot_df$Support - min(plot_df$Support)) / max(c(diff(range(plot_df$Support)), 0.0001))
    plot_df$ScaledSize <- scaled_size
    
    # Plot using Plotly (Custom Grouped Matrix Bubble Chart)
    p <- plotly::plot_ly(
      data = plot_df,
      x = ~Attribute,
      y = ~Group,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = ~ScaledSize,
        color = ~Lift,
        colorscale = "Viridis",
        colorbar = list(title = "Mean Lift"),
        opacity = 0.85,
        line = list(color = "#ffffff", width = 1.5)
      ),
      text = ~paste0(
        "<b>Rule Group:</b> ", Group, "<br>",
        "<b>Closure:</b> ", Closure, "<br>",
        "<b>LHS Premises:</b> ", LHS, "<br>",
        "<b>Implied Attribute:</b> ", Attribute, "<br>",
        "<b>Mean Support:</b> ", round(Support, 4), "<br>",
        "<b>Mean Lift:</b> ", round(Lift, 2), "<br><br>",
        "<b>Unified Rules:</b><br>", Rules
      ),
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        title = "Semantic Grouped Matrix (Unified by Closures)",
        xaxis = list(title = "Implied Attributes (RHS)", gridcolor = "#f3f3f3"),
        yaxis = list(title = "Unified Rule Groups (by Closure)", gridcolor = "#f3f3f3", automargin = TRUE),
        hoverlabel = list(bgcolor = "#2c3e50", font = list(color = "#ffffff")),
        margin = list(t = 65, b = 65, l = 150, r = 50)
      )
    
    return(p)
  })
  output$arulesTable <- renderDT({
    req(arules_filtered())
    df <- as(arules_filtered(), "data.frame")
    df$rules <- gsub("\\{", "", df$rules)
    df$rules <- gsub("\\}", "", df$rules)
    df$rules <- gsub("=>", " → ", df$rules)
    
    # 1. Filter out columns that are completely empty (all NA, NaN, NULL, or "")
    empty_cols <- sapply(df, function(col) {
      all(is.na(col) | is.nan(col) | is.null(col) | col == "")
    })
    df <- df[, !empty_cols, drop = FALSE]
    
    # 2. Capitalize remaining column names for neatness
    names(df) <- toupper(names(df))
    
    # 3. Identify numeric columns programmatically
    num_cols <- names(df)[sapply(df, is.numeric)]
    # Exclude count or rule index from rounding decimals
    num_cols <- num_cols[!tolower(num_cols) %in% c("count", "rule")]
    
    datatable(df, rownames = FALSE, options = list(scrollX = TRUE)) %>% 
      formatRound(columns = num_cols, digits = 3)
  })

  convert_rules_to_fcaR_safe <- function(rules_obj, formal_context) {
    # Safety check: do not attempt conversion if there are no rules
    if (length(rules_obj) == 0) {
      stop("No rules to convert (0 rules mined).")
    }
    
    # Build transactions the same way as when mining, so labels always match
    fc_mat <- t(as.matrix(formal_context$I))
    # The item labels arules assigns are the column names of the transposed matrix,
    # which are the attribute names of the formal context
    fcar_labels <- formal_context$attributes
    arules_labels <- rules_obj@lhs@itemInfo$labels
    
    # Compute the permutation: map each fcaR attribute to its position in arules items
    p <- match(fcar_labels, arules_labels)
    if (any(is.na(p))) {
      stop(paste0(
        "Mismatch between arules items and fcaR attributes.\n",
        "arules labels: ", paste(arules_labels, collapse = ", "), "\n",
        "fcaR attributes: ", paste(fcar_labels, collapse = ", ")
      ))
    }
    
    lhs_mat <- as(rules_obj@lhs@data, "ngCMatrix")
    rhs_mat <- as(rules_obj@rhs@data, "ngCMatrix")
    
    lhs_aligned <- lhs_mat[p, , drop = FALSE]
    rhs_aligned <- rhs_mat[p, , drop = FALSE]
    lhs_aligned <- as(lhs_aligned, "dgCMatrix")
    rhs_aligned <- as(rhs_aligned, "dgCMatrix")
    
    new_imps <- ImplicationSet$new(attributes = formal_context$attributes, I = formal_context$I)
    new_imps$add(lhs_aligned, rhs_aligned)
    return(new_imps)
  }

  # ===========================================================================
  # 8. INTERACTIVE ATTRIBUTE EXPLORATION (CONEXP INSPIRATION)
  # ===========================================================================
  
  # Helper to parse string vectors safely
  clean_vector <- function(s) {
    if (is.null(s) || is.na(s) || s == "" || s == "{}") return(character(0))
    parts <- strsplit(s, ", ")[[1]]
    parts <- parts[parts != "" & parts != "{}"]
    return(parts)
  }
  
  # --- UI Conditionals ---
  output$isExplorationActive <- reactive({
    isTRUE(vals$exploration_active)
  })
  outputOptions(output, "isExplorationActive", suspendWhenHidden = FALSE)
  
  output$isExplorationComplete <- reactive({
    isTRUE(vals$exploration_complete)
  })
  outputOptions(output, "isExplorationComplete", suspendWhenHidden = FALSE)
  
  output$isRefutingActive <- reactive({
    isTRUE(vals$is_refuting)
  })
  outputOptions(output, "isRefutingActive", suspendWhenHidden = FALSE)

  output$refutingRuleKeyText <- renderUI({
    req(vals$current_question)
    code(vals$current_question$key)
  })

  output$cexAttributesSelectorUI <- renderUI({
    req(vals$fc, vals$current_question)
    q <- vals$current_question
    selected_atts <- clean_vector(q$lhs)
    checkboxGroupInput("cex_attributes", "Select the attributes that this object possesses:",
                       choices = vals$fc$attributes,
                       selected = selected_atts,
                       inline = TRUE)
  })
  
  # --- Badge and Counts ---
  output$explorationStatusBadge <- renderUI({
    if (isTRUE(vals$exploration_complete)) {
      span("Complete", class = "badge bg-success fs-6 px-3 py-2")
    } else if (!isTRUE(vals$exploration_active)) {
      span("Inactive", class = "badge bg-secondary fs-6 px-3 py-2")
    } else {
      span("Active", class = "badge bg-primary fs-6 px-3 py-2 blink")
    }
  })
  
  output$confirmedRulesCount <- renderUI({
    n <- length(vals$confirmed_rules)
    div(class="d-flex justify-content-between align-items-center mt-2",
        span("Confirmed Rules:", class="text-muted small"),
        span(n, class="badge bg-success"))
  })
  
  output$refutedRulesCount <- renderUI({
    n <- length(vals$refuted_rules)
    div(class="d-flex justify-content-between align-items-center mt-2",
        span("Refutations:", class="text-muted small"),
        span(n, class="badge bg-danger"))
  })
  
  # --- Question and Confirmed Rules List ---
  output$questionText <- renderUI({
    req(vals$current_question)
    q <- vals$current_question
    
    # Beautifully display LHS and RHS attributes
    lhs_clean <- clean_vector(q$lhs)
    rhs_clean <- clean_vector(q$rhs)
    
    lhs_html <- if (length(lhs_clean) == 0) {
      as.character(span("Ø", class="text-muted fw-bold"))
    } else {
      paste(sapply(lhs_clean, function(a) {
        as.character(span(a, class="badge bg-dark me-1 fs-6"))
      }), collapse = " ")
    }
    
    rhs_html <- paste(sapply(rhs_clean, function(a) {
      as.character(span(a, class="badge bg-primary me-1 fs-6"))
    }), collapse = " ")
    
    div(class="d-flex align-items-center justify-content-center flex-wrap py-3",
        div(class="text-center px-3", HTML(lhs_html)),
        div(class="px-2 fs-3 exploration-arrow", icon("arrow-right")),
        div(class="text-center px-3", HTML(rhs_html))
    )
  })
  
  output$listConfirmedRules <- renderUI({
    if (length(vals$confirmed_rules) == 0) {
      return(p(class="text-muted italic", "No rules confirmed yet."))
    }
    tags$ul(class="list-group list-group-flush",
            lapply(vals$confirmed_rules, function(r) {
              tags$li(class="list-group-item bg-transparent text-success fw-bold py-1",
                      icon("check-circle", class="me-2"), r)
            }))
  })

  # --- Start and Reset Observers ---
  observeEvent(input$btnStartExploration, {
    req(vals$fc)
    vals$exploration_active <- TRUE
    vals$exploration_complete <- FALSE
    vals$is_refuting <- FALSE
    vals$confirmed_rules <- character(0)
    vals$refuted_rules <- character(0)
    
    # Find implications of the current context to start
    withProgress(message = "Initializing Attribute Exploration...", value = 0.5, {
      safe_find_implications(vals$fc)
    })
    
    implications_df(NULL)
    vals$trigger <- vals$trigger + 1
    
    shinyalert("Exploration Started", "Systematic knowledge acquisition is now active. Answer the questions on the right panel.", type = "info")
  })
  
  observeEvent(input$btnResetExploration, {
    vals$exploration_active <- FALSE
    vals$exploration_complete <- FALSE
    vals$is_refuting <- FALSE
    vals$confirmed_rules <- character(0)
    vals$refuted_rules <- character(0)
    vals$current_question <- NULL
    
    shinyalert("Exploration Reset", "The interactive exploration state has been cleared.", type = "warning")
  })

  # --- Interactive exploration loop logic ---
  observe({
    req(vals$exploration_active, vals$fc)
    
    # If implications are not computed, find them
    if (is.null(vals$fc$implications)) {
      withProgress(message = "Computing canonical basis...", value = 0.5, {
        safe_find_implications(vals$fc)
      })
    }
    
    # Get current implications
    imp_set <- vals$fc$implications
    if (is.null(imp_set) || imp_set$cardinality() == 0) {
      vals$exploration_complete <- TRUE
      vals$current_question <- NULL
      implications_df(NULL)
      vals$trigger <- vals$trigger + 1
      return()
    }
    
    imps_df <- get_implications_dataframe(imp_set)
    
    # Generate keys for all current implications
    keys <- sapply(1:nrow(imps_df), function(i) {
      paste0(imps_df$`if`[i], " => ", imps_df$then[i])
    })
    
    # Find the first one not in confirmed_rules
    unconfirmed_idx <- which(!(keys %in% vals$confirmed_rules))
    
    if (length(unconfirmed_idx) == 0) {
      vals$exploration_complete <- TRUE
      vals$exploration_active <- FALSE  # Deactivate exploration to prevent infinite reactive loops
      vals$current_question <- NULL
      
      # Clear any stale filter selections that may crash arules after context change
      updateSelectInput(session, "selectLHS", selected = character(0))
      updateSelectInput(session, "selectNotLHS", selected = character(0))
      updateSelectInput(session, "selectRHS", selected = character(0))
      updateSelectInput(session, "selectNotRHS", selected = character(0))
      updateSliderInput(session, "support", value = 0)
      updateNumericInput(session, "minLift", value = 1.0)
      vals$filtered_imps <- NULL
      
      implications_df(get_implications_dataframe(vals$fc$implications))
      vals$trigger <- vals$trigger + 1
    } else {
      idx <- unconfirmed_idx[1]
      vals$exploration_complete <- FALSE
      vals$current_question <- list(
        idx = idx,
        lhs = imps_df$`if`[idx],
        rhs = imps_df$then[idx],
        key = keys[idx],
        lhs_vector = clean_vector(imps_df$`if`[idx]),
        rhs_vector = clean_vector(imps_df$then[idx])
      )
    }
  })

  # --- Confirm Rule ---
  observeEvent(input$btnConfirmRule, {
    req(vals$exploration_active, vals$current_question)
    q <- vals$current_question
    vals$confirmed_rules <- c(vals$confirmed_rules, q$key)
    showNotification("Implication confirmed as domain rule!", type = "message")
  })

  # --- Refute Rule with Counterexample ---
  observeEvent(input$btnRefuteRule, {
    req(vals$exploration_active, vals$current_question, vals$fc)
    q <- vals$current_question
    updateTextInput(session, "cex_name", value = paste0("Cex_", length(vals$refuted_rules) + 1))
    vals$is_refuting <- TRUE
  })

  # --- Cancel Refute Action ---
  observeEvent(input$btnCancelRefute, {
    vals$is_refuting <- FALSE
  })

  # --- Save Counterexample ---
  observeEvent(input$btnSaveCex, {
    req(vals$exploration_active, vals$current_question, vals$fc, input$cex_name)
    q <- vals$current_question
    
    cex_name <- trimws(input$cex_name)
    if (cex_name == "") {
      showNotification("Please provide a name for the counterexample.", type = "error")
      return()
    }
    
    # Check if object name already exists
    if (cex_name %in% vals$fc$objects) {
      showNotification("An object with this name already exists. Please choose a unique name.", type = "error")
      return()
    }
    
    selected_atts <- input$cex_attributes
    lhs_atts <- clean_vector(q$lhs)
    rhs_atts <- clean_vector(q$rhs)
    
    # Validation 1: Must possess all LHS attributes
    if (length(lhs_atts) > 0 && !all(lhs_atts %in% selected_atts)) {
      showNotification("Invalid Counterexample: It must possess ALL attributes in the LHS (premise).", type = "error")
      return()
    }
    
    # Validation 2: Must lack at least one RHS attribute
    if (length(rhs_atts) > 0 && all(rhs_atts %in% selected_atts)) {
      showNotification("Invalid Counterexample: It must LACK at least one attribute in the RHS (conclusion).", type = "error")
      return()
    }
    
    # All good! Update the context
    mat <- t(as.matrix(vals$fc$I))
    new_row <- matrix(0, nrow = 1, ncol = ncol(mat))
    colnames(new_row) <- colnames(mat)
    rownames(new_row) <- cex_name
    
    if (length(selected_atts) > 0) {
      new_row[1, selected_atts] <- 1
    }
    
    new_mat <- rbind(mat, new_row)
    
    # Re-instantiate formal context!
    vals$fc <- FormalContext$new(new_mat)
    
    # Clear computed implications and recalculate
    withProgress(message = "Re-evaluating implications with counterexample...", value = 0.5, {
      safe_find_implications(vals$fc)
      vals$fc$find_concepts()
    })
    
    # Save statistics
    vals$refuted_rules <- c(vals$refuted_rules, q$key)
    vals$is_refuting <- FALSE
    vals$trigger <- vals$trigger + 1
    implications_df(NULL)
    vals$stats_orig <- NULL
    
    shinyalert("Counterexample Added", paste0("Object '", cex_name, "' has been added to your formal context."), type = "success")
  })

  # ===========================================================================
  # 9. DUAL INTERACTIVE OBJECT EXPLORATION (GANTER ON TRANSPOSED CONTEXT)
  # ===========================================================================
  
  # --- UI Conditionals ---
  output$isObjExplorationActive <- reactive({
    isTRUE(vals$obj_exploration_active)
  })
  outputOptions(output, "isObjExplorationActive", suspendWhenHidden = FALSE)
  
  output$isObjExplorationComplete <- reactive({
    isTRUE(vals$obj_exploration_complete)
  })
  outputOptions(output, "isObjExplorationComplete", suspendWhenHidden = FALSE)
  
  # --- Badge and Counts ---
  output$objExplorationStatusBadge <- renderUI({
    if (!isTRUE(vals$obj_exploration_active)) {
      span("Inactive", class = "badge bg-secondary fs-6 px-3 py-2")
    } else if (isTRUE(vals$obj_exploration_complete)) {
      span("Complete", class = "badge bg-success fs-6 px-3 py-2")
    } else {
      span("Active", class = "badge bg-warning text-dark fs-6 px-3 py-2 blink")
    }
  })
  
  output$objConfirmedRulesCount <- renderUI({
    n <- length(vals$obj_confirmed_rules)
    div(class="d-flex justify-content-between align-items-center mt-2",
        span("Confirmed Object Rules:", class="text-muted small"),
        span(n, class="badge bg-success"))
  })
  
  output$objRefutedRulesCount <- renderUI({
    n <- length(vals$obj_refuted_rules)
    div(class="d-flex justify-content-between align-items-center mt-2",
        span("Refutations:", class="text-muted small"),
        span(n, class="badge bg-danger"))
  })
  
  # --- Question and Confirmed Rules List ---
  output$objQuestionText <- renderUI({
    req(vals$obj_current_question)
    q <- vals$obj_current_question
    
    lhs_clean <- clean_vector(q$lhs)
    rhs_clean <- clean_vector(q$rhs)
    
    lhs_html <- if (length(lhs_clean) == 0) {
      as.character(span("Ø", class="text-muted fw-bold"))
    } else {
      paste(sapply(lhs_clean, function(a) {
        as.character(span(a, class="badge bg-warning text-dark me-1 fs-6"))
      }), collapse = " ")
    }
    
    rhs_html <- paste(sapply(rhs_clean, function(a) {
      as.character(span(a, class="badge bg-primary me-1 fs-6"))
    }), collapse = " ")
    
    div(class="d-flex align-items-center justify-content-center flex-wrap py-3",
        div(class="text-center px-3", HTML(lhs_html)),
        div(class="px-2 fs-3 exploration-arrow", icon("arrow-right")),
        div(class="text-center px-3", HTML(rhs_html))
    )
  })
  
  output$listObjConfirmedRules <- renderUI({
    if (length(vals$obj_confirmed_rules) == 0) {
      return(p(class="text-muted italic", "No object rules confirmed yet."))
    }
    tags$ul(class="list-group list-group-flush",
            lapply(vals$obj_confirmed_rules, function(r) {
              tags$li(class="list-group-item bg-transparent text-success fw-bold py-1",
                      icon("check-circle", class="me-2"), r)
            }))
  })

  # --- Start and Reset Observers ---
  observeEvent(input$btnStartObjExploration, {
    req(vals$fc)
    vals$obj_exploration_active <- TRUE
    vals$obj_exploration_complete <- FALSE
    vals$obj_confirmed_rules <- character(0)
    vals$obj_refuted_rules <- character(0)
    
    withProgress(message = "Initializing Object Exploration (Transposing Context)...", value = 0.5, {
      # Transpose matrix: rows become attributes (so they are our new objects), cols become objects
      transposed_mat <- as.matrix(vals$fc$I)
      vals$fc_transposed <- FormalContext$new(transposed_mat)
      safe_find_implications(vals$fc_transposed)
    })
    
    shinyalert("Object Exploration Started", "Systematic dual knowledge acquisition is active. Answer the questions on the right panel.", type = "info")
  })
  
  observeEvent(input$btnResetObjExploration, {
    vals$obj_exploration_active <- FALSE
    vals$obj_exploration_complete <- FALSE
    vals$obj_confirmed_rules <- character(0)
    vals$obj_refuted_rules <- character(0)
    vals$obj_current_question <- NULL
    vals$fc_transposed <- NULL
    
    shinyalert("Exploration Reset", "The dual object exploration state has been cleared.", type = "warning")
  })

  # --- Interactive exploration loop logic ---
  observe({
    req(vals$obj_exploration_active, vals$fc_transposed)
    
    if (is.null(vals$fc_transposed$implications)) {
      withProgress(message = "Computing canonical object basis...", value = 0.5, {
        safe_find_implications(vals$fc_transposed)
      })
    }
    
    imp_set <- vals$fc_transposed$implications
    if (is.null(imp_set) || imp_set$cardinality() == 0) {
      vals$obj_exploration_complete <- TRUE
      vals$obj_current_question <- NULL
      return()
    }
    
    imps_df <- get_implications_dataframe(imp_set)
    
    keys <- sapply(1:nrow(imps_df), function(i) {
      paste0(imps_df$`if`[i], " => ", imps_df$then[i])
    })
    
    unconfirmed_idx <- which(!(keys %in% vals$obj_confirmed_rules))
    
    if (length(unconfirmed_idx) == 0) {
      vals$obj_exploration_complete <- TRUE
      vals$obj_current_question <- NULL
    } else {
      idx <- unconfirmed_idx[1]
      vals$obj_exploration_complete <- FALSE
      vals$obj_current_question <- list(
        idx = idx,
        lhs = imps_df$`if`[idx],
        rhs = imps_df$then[idx],
        key = keys[idx],
        lhs_vector = clean_vector(imps_df$`if`[idx]),
        rhs_vector = clean_vector(imps_df$then[idx])
      )
    }
  })

  # --- Confirm Rule ---
  observeEvent(input$btnConfirmObjRule, {
    req(vals$obj_exploration_active, vals$obj_current_question)
    q <- vals$obj_current_question
    vals$obj_confirmed_rules <- c(vals$obj_confirmed_rules, q$key)
    showNotification("Object implication confirmed as domain rule!", type = "message")
  })

  # --- Refute Rule with Counterexample ---
  observeEvent(input$btnRefuteObjRule, {
    req(vals$obj_exploration_active, vals$obj_current_question, vals$fc)
    q <- vals$obj_current_question
    
    selected_objs <- clean_vector(q$lhs)
    
    showModal(modalDialog(
      title = span(icon("times-circle", class="text-warning"), " Provide Counterexample Attribute"),
      size = "m",
      easyClose = FALSE,
      
      p(class="text-muted small", 
        "A valid counterexample attribute must be possessed by ALL objects in the Premise (LHS), but MUST NOT be possessed by at least one object in the Conclusion (RHS)."),
      
      div(class="p-3 bg-light border rounded mb-3",
        tags$strong("Object Implication being refuted:"), br(),
        code(q$key)
      ),
      
      textInput("cex_attr_name", "Name of the new Attribute", 
                value = paste0("NewAtt_", length(vals$obj_refuted_rules) + 1), 
                width = "100%"),
      
      checkboxGroupInput("cex_objects", "Select the objects that possess this new attribute:",
                         choices = vals$fc$objects,
                         selected = selected_objs,
                         inline = TRUE),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btnSaveObjCex", "Save Counterexample", class = "btn-warning")
      )
    ))
  })

  # --- Save Counterexample ---
  observeEvent(input$btnSaveObjCex, {
    req(vals$obj_exploration_active, vals$obj_current_question, vals$fc, input$cex_attr_name)
    q <- vals$obj_current_question
    
    cex_attr_name <- trimws(input$cex_attr_name)
    if (cex_attr_name == "") {
      showNotification("Please provide a name for the counterexample attribute.", type = "error")
      return()
    }
    
    if (cex_attr_name %in% vals$fc$attributes) {
      showNotification("An attribute with this name already exists. Please choose a unique name.", type = "error")
      return()
    }
    
    selected_objs <- input$cex_objects
    lhs_objs <- clean_vector(q$lhs)
    rhs_objs <- clean_vector(q$rhs)
    
    # Validation 1: Must be possessed by all LHS objects
    if (length(lhs_objs) > 0 && !all(lhs_objs %in% selected_objs)) {
      showNotification("Invalid Counterexample: The new attribute must be possessed by ALL objects in the LHS (premise).", type = "error")
      return()
    }
    
    # Validation 2: Must LACK at least one RHS object
    if (length(rhs_objs) > 0 && all(rhs_objs %in% selected_objs)) {
      showNotification("Invalid Counterexample: The new attribute must LACK at least one object in the RHS (conclusion).", type = "error")
      return()
    }
    
    # All good! Update the main context: we are adding a NEW COLUMN (attribute) to vals$fc!
    mat <- t(as.matrix(vals$fc$I)) # rows = objects, columns = attributes
    new_col <- matrix(0, nrow = nrow(mat), ncol = 1)
    colnames(new_col) <- cex_attr_name
    rownames(new_col) <- rownames(mat)
    
    if (length(selected_objs) > 0) {
      new_col[selected_objs, 1] <- 1
    }
    
    new_mat <- cbind(mat, new_col)
    
    # Re-instantiate formal contexts!
    vals$fc <- FormalContext$new(new_mat)
    
    transposed_mat <- as.matrix(vals$fc$I)
    vals$fc_transposed <- FormalContext$new(transposed_mat)
    
    # Clear computed implications and recalculate
    withProgress(message = "Re-evaluating object implications with counterexample...", value = 0.5, {
      safe_find_implications(vals$fc_transposed)
      vals$fc$find_concepts()
    })
    
    # Save statistics
    vals$obj_refuted_rules <- c(vals$obj_refuted_rules, q$key)
    vals$trigger <- vals$trigger + 1
    implications_df(NULL)
    vals$stats_orig <- NULL
    
    # Close modal
    removeModal()
    shinyalert("Counterexample Added", paste0("Attribute '", cex_attr_name, "' has been added to your formal context."), type = "success")
  })
}
