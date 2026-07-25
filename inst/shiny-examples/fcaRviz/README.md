# Guía de Integración de fcaRviz en el Paquete R `fcaR`

Esta carpeta contiene todos los archivos necesarios (`ui.R`, `server.R`, `global.R`, submódulos de la UI y recursos estáticos en `www/`) para lanzar **fcaRviz** directamente desde la librería R `fcaR`.

---

## Pasos para la Integración

### 1. Copiar los archivos al paquete fuente
En la estructura de directorios del código fuente del paquete `fcaR`, crea una estructura bajo la carpeta de instalación estándar `inst/`:

```text
fcaR/
├── inst/
│   └── shiny-examples/
│       └── fcaRviz/
│           ├── ui.R
│           ├── server.R
│           ├── global.R
│           ├── scaling_helpers.R
│           ├── uiHome.R
│           ├── uiUploadData.R
│           ├── uiBasicOperations.R
│           ├── uiImplications.R
│           ├── uiConcepts.R
│           └── www/
│               ├── style.css
│               ├── logo.png
│               ├── logo2.png
│               ├── oops.png
│               ├── success.gif
│               └── success2.gif
```

*(La carpeta `inst/` se copia automáticamente a la raíz de la instalación del paquete R al compilarlo).*

---

### 2. Crear la función lanzadora en R
Crea un archivo de R en la ruta del paquete `fcaR/R/fcaRviz.R` con la función exportable para iniciar la aplicación:

```R
#' Launch the fcaRviz Interactive Viewer
#'
#' This function opens the interactive Shiny application fcaRviz for visual exploration.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @export
#' @importFrom shiny runApp
#' @examples
#' \dontrun{
#' run_fcaRviz()
#' }
run_fcaRviz <- function(...) {
  # Localizar la ruta de la aplicación instalada en el sistema
  app_dir <- system.file("shiny-examples", "fcaRviz", package = "fcaR")
  
  if (app_dir == "") {
    stop("Could not find the shiny-examples/fcaRviz directory in the fcaR package. Try re-installing `fcaR`.", call. = FALSE)
  }
  
  # Ejecutar la aplicación Shiny
  shiny::runApp(app_dir, ...)
}
```

---

### 3. Declarar dependencias en el archivo DESCRIPTION
Asegúrate de registrar en el archivo `DESCRIPTION` del paquete `fcaR` bajo `Imports` (o `Suggests` si consideras opcional la interfaz gráfica) los paquetes que utiliza la aplicación:

```dcf
Imports:
    shiny,
    bslib,
    DT,
    ggplot2,
    plotly,
    shinyalert,
    shinyjs,
    shinyWidgets,
    yaml,
    visNetwork,
    Matrix,
    arules,
    igraph,
    dplyr,
    future,
    promises,
    rhandsontable
```

*(Recuerda documentar el paquete usando `devtools::document()` para generar el archivo NAMESPACE actualizado con `export(run_fcaRviz)`).*

---

### 4. Configurar el Addin de RStudio
Para que la aplicación aparezca directamente en el menú de **Addins** de RStudio de los usuarios:

* **Si ya tienes un archivo `addins.dcf`:** Simplemente abre tu archivo `inst/rstudio/addins.dcf` existente y añade el nuevo bloque **al final, dejando una línea en blanco de separación** con el addin anterior.
* **Si no tenías ninguno:** Copia el archivo `addins.dcf` que he preparado en `for_fcaR/rstudio/addins.dcf` dentro de la carpeta `inst/rstudio/` de tu paquete.

El bloque a añadir o contener es:

```dcf
Name: Launch fcaRviz
Description: Launches the interactive FCA viewer fcaRviz.
Binding: run_fcaRviz
Interactive: true
```

Una vez instalado o recargado el paquete `fcaR` en RStudio, la interfaz **fcaRviz** estará disponible a un solo clic en el menú superior de Addins junto con tus addins anteriores.


