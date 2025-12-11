get_metadata <- function(ID) {
  meta <- yaml::read_yaml("https://fcarepository.org/contexts.yaml")

  if (ID %in% names(meta)) {
    return(meta[[ID]])
  }

  return(NULL)
}

#' Get Metadata from the FCA Repository
#'
#' @description
#' Connects to the official FCA Repository (https://fcarepository.org) and downloads
#' the metadata for all available datasets.
#'
#' @return A list containing the metadata for each context (title, dimensions, description, source).
#' @importFrom yaml read_yaml
#' @export
#'
#' @examples
#' \dontrun{
#' meta <- get_fcarepository_contexts()
#' }
get_fcarepository_contexts <- function() {
  meta <- yaml::read_yaml("https://fcarepository.org/contexts.yaml")
}

#' Print Details of Repository Contexts
#'
#' @description
#' Prints a formatted summary of the contexts available in the FCA Repository
#' to the console. It displays the filename, title, dimensions (objects x attributes),
#' and description for each entry.
#'
#' @param meta A list of metadata objects, typically obtained via \code{\link{get_fcarepository_contexts}}.
#'
#' @return Prints the summary to the console. Returns \code{NULL} invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' meta <- get_fcarepository_contexts()
#' print_repo_details(meta)
#' }
print_repo_details <- function(meta) {
  # Header summary
  cat(sprintf("Found %d formal contexts in the repository:\n", length(meta)))
  cat(paste(rep("=", 40), collapse = ""), "\n\n")

  for (filename in names(meta)) {
    item <- meta[[filename]]

    # --- 1. Normalización de Dimensiones (Robustez) ---
    if (!is.null(item$size)) {
      # Caso anidado
      n_obj <- item$size[[1]]$objects
      n_attr <- item$size[[2]]$attributes
    } else {
      # Caso plano
      n_obj <- if (!is.null(item$objects)) item$objects else 0
      n_attr <- if (!is.null(item$attributes)) item$attributes else 0
    }

    # --- 2. Preparación de textos ---
    title <- if (!is.null(item$title)) item$title else "No title"
    desc <- if (!is.null(item$description)) item$description else ""

    # Formateamos la descripción para que no desborde si la consola es estrecha
    # 'exdent = 15' alinea las líneas siguientes con la indentación visual
    desc_wrapped <- paste(strwrap(desc, width = 60, exdent = 15), collapse = "\n")

    # --- 3. Impresión del Bloque ---
    # Usamos un marcador visual (bullet) para separar elementos
    cat(sprintf("* %s\n", filename))
    cat(sprintf("    Title:        %s\n", title))
    cat(sprintf("    Dimensions:   %d objects x %d attributes\n", n_obj, n_attr))

    # Solo imprimimos descripción si existe
    if (nchar(desc) > 0) {
      cat(sprintf("    Description:  %s\n", desc_wrapped))
    }

    # Línea vacía entre contextos para legibilidad
    cat("\n")
  }
}


#' GUI to select and download a context from the repository
#'
#' @param meta A list of metadata objects (obtained via \code{get_fcarepository_contexts}).
#'
#' @export
select_repository_context <- function(meta) {
  check_needed_pkg("shiny", "the Context Fetcher Addin")
  check_needed_pkg("miniUI", "the Context Fetcher Addin")
  check_needed_pkg("DT", "displaying tables in the Addin")

  # 1. Preprocesamiento: Convertir la lista 'meta' compleja a data.frame plano
  # Esto es necesario para que DT pueda mostrarlo bien
  df_meta <- do.call(rbind, lapply(names(meta), function(fname) {
    item <- meta[[fname]]

    # Lógica de normalización de tamaño (la misma que discutimos antes)
    if (!is.null(item$size)) {
      obj <- item$size[[1]]$objects
      att <- item$size[[2]]$attributes
    } else {
      obj <- if (!is.null(item$objects)) item$objects else 0
      att <- if (!is.null(item$attributes)) item$attributes else 0
    }

    data.frame(
      Filename = fname,
      Title = if (!is.null(item$title)) item$title else "",
      Objects = obj,
      Attributes = att,
      Description = if (!is.null(item$description)) item$description else "",
      stringsAsFactors = FALSE
    )
  }))

  # 2. Definición de la Interfaz (UI) usando miniUI
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("FCA Repository Browser"),
    miniUI::miniContentPanel(
      DT::DTOutput("context_table", height = "100%")
    )
  )

  # 3. Lógica del Servidor
  server <- function(input, output, session) {
    # Renderizar la tabla interactiva
    output$context_table <- DT::renderDT({
      DT::datatable(
        df_meta,
        selection = "single", # Solo permitir seleccionar uno a la vez
        options = list(
          pageLength = 10,
          dom = "ftp", # f=filter(search), t=table, p=pagination (diseño limpio)
          autoWidth = TRUE
        ),
        rownames = FALSE
      )
    })

    # Manejar el botón "Done" (que será nuestro botón de "Download/Select")
    # En la parte del server del Addin:
    shiny::observeEvent(input$done, {
      selected_idx <- input$context_table_rows_selected
      if (!is.null(selected_idx)) {
        file_id <- df_meta$Filename[selected_idx]

        # Opción 1: Devolver el código para que el usuario lo ejecute
        code <- sprintf("fc <- fcaR::fetch_context('%s')", file_id)
        rstudioapi::sendToConsole(code)

        shiny::stopApp()
      }
    })

    # Manejar el botón "Cancel"
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
  }

  # 4. Ejecutar el Gadget
  # 'dialog' abre una ventana flotante, 'viewer' lo abre en el panel lateral
  viewer <- shiny::dialogViewer("Select Context", width = 900, height = 600)
  shiny::runGadget(ui, server, viewer = viewer)
}

#' Fetch a Formal Context from the FCA Repository
#'
#' Downloads a context file from the fcarepository.org (via GitHub mirror),
#' parses it, and returns a FormalContext object.
#'
#' @param filename Character string. The ID/filename of the context (e.g., "animals_en.cxt").
#' @param verbose Logical. If TRUE, prints metadata and progress messages using cli/glue.
#'
#' @importFrom utils download.file
#' @importFrom cli cli_alert_info
#' @return A FormalContext object.
#' @export
fetch_context <- function(filename, verbose = TRUE) {
  # 1. Validación básica de entrada
  if (!is.character(filename) || length(filename) != 1) {
    stop("The 'filename' argument must be a single character string.")
  }

  # Asegurar que tiene la extensión .cxt (por si el usuario se le olvida)
  if (!grepl("\\.cxt$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".cxt")
  }

  # 2. Construcción de URL y Archivo Temporal
  # Usamos la URL raw de GitHub que ya tenías identificada
  base_url <- "https://github.com/fcatools/contexts/raw/main/contexts/"
  url <- glue::glue("{base_url}{filename}")
  dest_file <- tempfile(fileext = ".cxt")

  # Aseguramos limpieza del archivo temporal al salir de la función (éxito o error)
  on.exit(unlink(dest_file), add = TRUE)

  # 3. Descarga con Manejo de Errores
  if (verbose) {
    cli::cli_alert_info("Attempting to fetch {.val {filename}} from repository...")
  }

  download_status <- tryCatch(
    {
      utils::download.file(url, destfile = dest_file, quiet = TRUE, mode = "wb")
    },
    error = function(e) {
      return(e)
    },
    warning = function(w) {
      # download.file a veces emite warnings en 404, los capturamos como error
      return(structure(list(message = w$message), class = c("error", "condition")))
    }
  )

  # Verificar si hubo error en la descarga
  if (inherits(download_status, "error")) {
    msg <- download_status$message
    # Intentamos dar un mensaje más amigable si es un 404
    if (grepl("404", msg) || grepl("cannot open URL", msg)) {
      stop(glue::glue("Could not find context '{filename}' in the repository. Please check the ID."), call. = FALSE)
    } else {
      stop(glue::glue("Network error while downloading: {msg}"), call. = FALSE)
    }
  }

  # 4. Carga del Contexto (Parsing)
  # Intentamos crear el objeto FormalContext
  fc <- tryCatch(
    {
      # Asumimos que FormalContext$new() puede leer desde un path
      # Si tu constructor requiere leer el archivo explícitamente antes, ajusta esta línea.
      FormalContext$new(dest_file)
    },
    error = function(e) {
      stop(glue::glue("The file was downloaded but could not be parsed as a Formal Context.\nReason: {e$message}"), call. = FALSE)
    }
  )

  # 5. Mostrar Metadatos (Estilo fcaR)
  if (verbose) {
    cli::cli_alert_success("Context loaded successfully.")

    # Asumimos que get_metadata es una función interna o helper disponible
    # Si no existe, podemos intentar extraerla del objeto 'meta' global si está disponible
    # Ojo: Aquí reutilizo tu lógica visual exacta

    # Recuperamos metadatos (esto dependerá de cómo tengas implementado get_metadata)
    # Opción A: Si get_metadata hace una query nueva:
    meta_info <- try(get_metadata(filename), silent = TRUE)

    # Opción B: Si el objeto 'fc' ya guarda metadatos internos al cargar, úsalos.

    if (!inherits(meta_info, "try-error") && !is.null(meta_info)) {
      # Tu bloque de formateo con glue y cli
      info_text <- glue::glue(
        "\n",
        "- {cli::style_underline('Title')}: {meta_info$title}\n",
        "- {cli::style_underline('Description')}: {stringr::str_to_sentence(meta_info$description)}\n",
        "- {cli::style_underline('Source')}: {meta_info$source}\n",
        .trim = FALSE
      )
      cat(info_text)

      fc$description <- stringr::str_to_sentence(meta_info$description)

    }
  }

  return(fc)
}

select_repository_context_addin <- function() {
  # Aquí tendrías que llamar a la función que descarga el 'meta'
  meta <- get_fcarepository_contexts()

  file <- select_repository_context(meta)

  if (!is.null(file)) {
    # Truco Pro: Insertar el código para descargar ese fichero en la consola del usuario
    code_to_run <- sprintf("fc <- fcaR::fetch_context('%s')", file)
    rstudioapi::sendToConsole(code_to_run)
  }
}
