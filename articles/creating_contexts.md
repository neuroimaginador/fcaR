# Creating Formal Contexts in fcaR

``` r
library(fcaR)
```

The starting point of any Formal Concept Analysis (FCA) workflow is the
**Formal Context**. A formal context is a triple $K = (G,M,I)$, where
$G$ is a set of objects, $M$ is a set of attributes, and
$I \subseteq G \times M$ is a binary relation between them.

In `fcaR`, formal contexts are managed by the `FormalContext` R6 class.
This vignette demonstrates the multiple ways to create or import a
`FormalContext` object.

## 1. Creating from R Data Structures

The most direct way to create a formal context is by passing a binary
matrix (0/1 or TRUE/FALSE) or a data frame to the constructor.

### From a matrix

You can manually define a matrix where rows represent objects and
columns represent attributes.

``` r
# Create a binary matrix
M <- matrix(c(1, 0, 1,
              1, 1, 0,
              0, 1, 1), 
            nrow = 3, 
            byrow = TRUE)

# Assign row and column names (Objects and Attributes)
rownames(M) <- c("Object1", "Object2", "Object3")
colnames(M) <- c("Attribute1", "Attribute2", "Attribute3")

# Create the FormalContext object
fc <- FormalContext$new(M)

# Print the context
fc
#> FormalContext with 3 objects and 3 attributes.
#>          Attribute1  Attribute2  Attribute3  
#>  Object1      X                       X      
#>  Object2      X           X                  
#>  Object3                  X           X
```

### From a data frame

You can also use a `data.frame`. `fcaR` will attempt to coerce it into a
binary matrix. This is useful if your data is already loaded in R from
another source.

``` r
df <- data.frame(
  has_wings = c(TRUE, TRUE, FALSE),
  can_fly   = c(TRUE, FALSE, FALSE),
  has_legs  = c(TRUE, TRUE, TRUE),
  row.names = c("Eagle", "Penguin", "Dog")
)

fc_df <- FormalContext$new(df)
fc_df
#> FormalContext with 3 objects and 3 attributes.
#>          has_wings  can_fly  has_legs  
#>    Eagle     X         X         X     
#>  Penguin     X                   X     
#>      Dog                         X
```

## 2. Importing from Local Files

`fcaR` supports several standard file formats used in the FCA community.
The `FormalContext$new()` constructor automatically detects the format
based on the file extension.

### CXT Format (`.cxt`)

The Burmeister format (`.cxt`) is the standard for many classic FCA
tools like ConExp.

``` r
# We use an example file included in the package
cxt_file <- system.file("contexts", "lives_in_water.cxt", package = "fcaR")

# Load the context
fc_cxt <- FormalContext$new(cxt_file)
fc_cxt
#> FormalContext with 8 objects and 9 attributes.
#>              needs water to live  lives in water  lives on land  needs chlorophyll  
#>   fish leech          X                  X                                          
#>        bream          X                  X                                          
#>         frog          X                  X              X                           
#>          dog          X                                 X                           
#>  water weeds          X                  X                               X          
#>         reed          X                  X              X                X          
#>         bean          X                                 X                X          
#>         corn          X                                 X                X          
#> Other attributes are: dicotyledon, monocotyledon, can move, has limbs, breast
#> feeds
```

### CSV Files (`.csv`)

You can load a context from a Comma-Separated Values file. It assumes
the first column contains object names and the header contains attribute
names.

``` r
# We use an example file included in the package
csv_file <- system.file("contexts", "airlines.csv", package = "fcaR")

# Load from CSV
# Note: Ensure your CSV contains binary data
fc_csv <- FormalContext$new(csv_file)

# Inspect dimensions
dim(fc_csv)
#> NULL
```

## 3. Fetching from the FCA Repository

`fcaR` includes a powerful feature to download and load curated datasets
directly from the online **FCA Repository** (fcarepository.org).

### Browsing the Repository

You don’t need to know the filenames by heart. You can browse the
available contexts and see their metadata (dimensions, description,
source) directly from the console.

``` r
# Get the list of available contexts
meta <- get_fcarepository_contexts()

# Print a detailed summary to the console
# (Shows Title, Dimensions, and Description for each entry)
print_repo_details(meta)
#> Found 20 formal contexts in the repository:
#> ======================================== 
#> 
#> * animals_en.cxt
#>     Title:        Animals
#>     Dimensions:   35 objects x 11 attributes
#>     Description:  animals and their characteristics
#> 
#> * binary_relations_en.cxt
#>     Title:        Properties of binary relations
#>     Dimensions:   14 objects x 9 attributes
#>     Description:  Properties of binary relations
#> 
#> * bodiesofwater_de.cxt
#>     Title:        Gewässer
#>     Dimensions:   8 objects x 6 attributes
#>     Description:  bodies of water and their properties
#> 
#> * bodiesofwater_en.cxt
#>     Title:        Bodies of water
#>     Dimensions:   17 objects x 5 attributes
#>     Description:  bodies of water and their properties
#> 
#> * driveconcepts_de.cxt
#>     Title:        Drive Concepts
#>     Dimensions:   5 objects x 25 attributes
#>     Description:  drive concepts for motorcars
#> 
#> * driveconcepts_en.cxt
#>     Title:        Drive concepts
#>     Dimensions:   5 objects x 25 attributes
#>     Description:  drive concepts for motorcars
#> 
#> * famous_animals_en.cxt
#>     Title:        Famous Animals
#>     Dimensions:   5 objects x 6 attributes
#>     Description:  famous animals and their characteristics
#> 
#> * forum_romanum_en.cxt
#>     Title:        Forum Romanum
#>     Dimensions:   14 objects x 7 attributes
#>     Description:  Ratings of monuments on the Forum Romanum in different
#>                travel guides.
#> 
#> * livingbeings_de.cxt
#>     Title:        Lebewesen und Wasser
#>     Dimensions:   8 objects x 9 attributes
#>     Description:  conditions different living beings need
#> 
#> * livingbeings_en.cxt
#>     Title:        Living beings and water
#>     Dimensions:   8 objects x 9 attributes
#>     Description:  conditions different living beings need
#> 
#> * missmarple_de.cxt
#>     Title:        Mordmethoden in Miss-Marple-Romanen
#>     Dimensions:   12 objects x 6 attributes
#>     Description:  methods of murder in Miss Marple novels (not short
#>                stories!)
#> 
#> * missmarple_en.cxt
#>     Title:        Methods of murder in Miss Marple novels
#>     Dimensions:   12 objects x 6 attributes
#>     Description:  methods of murder in Miss Marple novels (not short
#>                stories!)
#> 
#> * music_en.cxt
#>     Title:        Music
#>     Dimensions:   31 objects x 11 attributes
#>     Description:  music and their characteristics
#> 
#> * newzealand_en.cxt
#>     Title:        New Zealand Leasure
#>     Dimensions:   13 objects x 8 attributes
#>     Description:  Places in New Zealand and offered activities
#> 
#> * officesupplies_de.cxt
#>     Title:        Bürobedarf
#>     Dimensions:   8 objects x 5 attributes
#>     Description:  service offers of an office supplies business
#> 
#> * officesupplies_en.cxt
#>     Title:        Office supply services
#>     Dimensions:   8 objects x 5 attributes
#>     Description:  Office supply services
#> 
#> * planets_en.cxt
#>     Title:        Planets
#>     Dimensions:   9 objects x 7 attributes
#>     Description:  size and distance of planets
#> 
#> * seasoningplanner_de.cxt
#>     Title:        Gewürzplaner
#>     Dimensions:   56 objects x 37 attributes
#>     Description:  spices and herbs together and the meals they match
#> 
#> * tealady.cxt
#>     Title:        Tea Ladies
#>     Dimensions:   18 objects x 14 attributes
#>     Description:  participation of social events by some ladies in Old City
#> 
#> * triangles_en.cxt
#>     Title:        Triangles
#>     Dimensions:   7 objects x 7 attributes
#>     Description:  Properties of triangles.
```

### Downloading a Specific Context

Once you have identified a context of interest (e.g., `"planets_en.cxt"`
or `"animals_en.cxt"`), you can download it using
[`fetch_context()`](https://neuroimaginador.github.io/fcaR/reference/fetch_context.md).
This function handles the download, parsing, and error checking for you.

``` r
# Download and load the 'Planets' context
fc_planets <- fetch_context("planets_en.cxt")
#> ℹ Attempting to fetch "planets_en.cxt" from repository...
#> ✔ Context loaded successfully.
#> 
#> - Title: Planets
#> - Description: Size and distance of planets
#> - Source: Anggraini, D. (2011). Analisis Perubahan Kelompok Berdasarkan Perubahan Nilai Jual Pada Bloomberg Market Data dengan Menggunakan Formal Concept Analysis, p. 7

# The object is ready for analysis
fc_planets$find_concepts()
fc_planets$concepts$size()
#> [1] 12
```

## 4. Interactive Use: The RStudio Addin

For a more visual and user-friendly experience, `fcaR` includes an
**RStudio Addin**. This tool provides a Graphical User Interface (GUI)
to explore the repository without writing code initially.

### How to Launch It

You have two options:

1.  **Via menu:** In RStudio, go to the toolbar, click the **“Addins”**
    dropdown menu, and select **“Browse FCA Repository”** under the
    **fcaR** section.
2.  **Via console:** Run the following command:

``` r
select_repository_context_addin()
```

### What Does the Addin Do?

1.  It opens a floating window with an interactive table of all
    available contexts in the repository.
2.  You can **filter** and **search** contexts by filename, title, or
    description.
3.  When you select a row and click **“Done”**, the Addin automatically
    inserts the necessary code into your console to download that
    specific context (using `fetch_context`).

This is the recommended way to discover new datasets and start working
quickly.

## Summary of Methods

| Source                    | Function / Method                   | Description                             |
|:--------------------------|:------------------------------------|:----------------------------------------|
| **R Matrix / Data Frame** | `FormalContext$new(x)`              | Direct creation from in-memory objects. |
| **Local File (.cxt)**     | `FormalContext$new("file.cxt")`     | Loads Burmeister format files.          |
| **Local File (.csv)**     | `FormalContext$new("file.csv")`     | Loads CSV files (expects binary data).  |
| **FCA Repository (Code)** | `fetch_context("name.cxt")`         | Downloads and loads from the web.       |
| **FCA Repository (GUI)**  | `select_repository_context_addin()` | Graphical interface to search and load. |
