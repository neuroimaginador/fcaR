# R6 class for a formal context

This class implements the data structure and methods for formal
contexts.

## References

Guigues J, Duquenne V (1986). “Familles minimales d'implications
informatives résultant d'un tableau de données binaires.” *Mathématiques
et Sciences humaines*, *95*, 5-18.

Ganter B, Wille R (1999). *Formal concept analysis : mathematical
foundations*. Springer. ISBN 3540627715.

Belohlavek R (2002). “Algorithms for fuzzy concept lattices.” In *Proc.
Fourth Int. Conf. on Recent Advances in Soft Computing*. Nottingham,
United Kingdom, 200-205.

Hahsler M, Grun B, Hornik K (2005). “arules - a computational
environment for mining association rules and frequent item sets.” *J
Stat Softw*, *14*, 1-25.

## Public fields

- `I`:

  The table of the formal context as a matrix.

- `attributes`:

  The attributes of the formal context.

- `objects`:

  The objects of the formal context.

- `grades_set`:

  The set of degrees (in \[0, 1\]) the whole set of attributes can take.

- `expanded_grades_set`:

  The set of degrees (in \[0, 1\]) each attribute can take.

- `concepts`:

  The concept lattice associated to the formal context as a
  [`ConceptLattice`](https://neuroimaginador.github.io/fcaR/reference/ConceptLattice.md).

- `implications`:

  A set of implications on the formal context as an
  [`ImplicationSet`](https://neuroimaginador.github.io/fcaR/reference/ImplicationSet.md).

- `description`:

  An optional description of the dataset

## Methods

### Public methods

- [`FormalContext$new()`](#method-FormalContext-new)

- [`FormalContext$is_empty()`](#method-FormalContext-is_empty)

- [`FormalContext$scale()`](#method-FormalContext-scale)

- [`FormalContext$get_scales()`](#method-FormalContext-get_scales)

- [`FormalContext$background_knowledge()`](#method-FormalContext-background_knowledge)

- [`FormalContext$dual()`](#method-FormalContext-dual)

- [`FormalContext$intent()`](#method-FormalContext-intent)

- [`FormalContext$uparrow()`](#method-FormalContext-uparrow)

- [`FormalContext$extent()`](#method-FormalContext-extent)

- [`FormalContext$downarrow()`](#method-FormalContext-downarrow)

- [`FormalContext$closure()`](#method-FormalContext-closure)

- [`FormalContext$obj_concept()`](#method-FormalContext-obj_concept)

- [`FormalContext$att_concept()`](#method-FormalContext-att_concept)

- [`FormalContext$is_concept()`](#method-FormalContext-is_concept)

- [`FormalContext$is_closed()`](#method-FormalContext-is_closed)

- [`FormalContext$clarify()`](#method-FormalContext-clarify)

- [`FormalContext$reduce()`](#method-FormalContext-reduce)

- [`FormalContext$standardize()`](#method-FormalContext-standardize)

- [`FormalContext$find_concepts()`](#method-FormalContext-find_concepts)

- [`FormalContext$find_implications()`](#method-FormalContext-find_implications)

- [`FormalContext$find_causal_rules()`](#method-FormalContext-find_causal_rules)

- [`FormalContext$factorize()`](#method-FormalContext-factorize)

- [`FormalContext$to_transactions()`](#method-FormalContext-to_transactions)

- [`FormalContext$save()`](#method-FormalContext-save)

- [`FormalContext$load()`](#method-FormalContext-load)

- [`FormalContext$dim()`](#method-FormalContext-dim)

- [`FormalContext$to_json()`](#method-FormalContext-to_json)

- [`FormalContext$print()`](#method-FormalContext-print)

- [`FormalContext$to_latex()`](#method-FormalContext-to_latex)

- [`FormalContext$incidence()`](#method-FormalContext-incidence)

- [`FormalContext$subcontext()`](#method-FormalContext-subcontext)

- [`FormalContext$[()`](#method-FormalContext-%5B)

- [`FormalContext$plot()`](#method-FormalContext-plot)

- [`FormalContext$use_logic()`](#method-FormalContext-use_logic)

- [`FormalContext$get_logic()`](#method-FormalContext-get_logic)

- [`FormalContext$use_connection()`](#method-FormalContext-use_connection)

- [`FormalContext$get_connection()`](#method-FormalContext-get_connection)

- [`FormalContext$clone()`](#method-FormalContext-clone)

------------------------------------------------------------------------

### Method `new()`

Creator for the Formal Context class

#### Usage

    FormalContext$new(I, filename, remove_const = FALSE)

#### Arguments

- `I`:

  (numeric matrix) The table of the formal context.

- `filename`:

  (character) Path of a file to import.

- `remove_const`:

  (logical) If `TRUE`, remove constant columns. The default is `FALSE`.

#### Details

Columns of `I` should be named, since they are the names of the
attributes of the formal context.

If no `I` is used, the resulting `FormalContext` will be empty and not
usable unless for loading a previously saved one. In this case, one can
provide a `filename` to import. Only RDS, CSV and CXT files are
currently supported.

If the file is not present, the fcarepository.org is looked for
coincidences. If so, the corresponding context is loaded.

#### Returns

An object of the `FormalContext` class.

------------------------------------------------------------------------

### Method `is_empty()`

Check if the `FormalContext` is empty

#### Usage

    FormalContext$is_empty()

#### Returns

`TRUE` if the `FormalContext` is empty, that is, has not been provided
with a matrix, and `FALSE` otherwise.

------------------------------------------------------------------------

### Method [`scale()`](https://rdrr.io/r/base/scale.html)

Scale the context

#### Usage

    FormalContext$scale(attributes, type, ...)

#### Arguments

- `attributes`:

  The attributes to scale

- `type`:

  Type of scaling.

- `...`:

#### Details

The types of scaling are implemented in a registry, so that
`scalingRegistry$get_entries()` returns all types.

In the dots argument, the user can supply the value for `bg` (logical),
which, if set to `TRUE`, indicates to compute background knowledge as
implications on the scales; if `FALSE`, no implications will be computed
on the scales.

#### Returns

The scaled formal context

#### Examples

    filename <- system.file("contexts", "aromatic.csv", package = "fcaR")
    fc <- FormalContext$new(filename)
    fc$scale("nitro", "ordinal", comparison = `>=`, values = 1:3)
    fc$scale("OS", "nominal", c("O", "S"))
    fc$scale(attributes = "ring", type = "nominal")

------------------------------------------------------------------------

### Method `get_scales()`

Scales applied to the formal context

#### Usage

    FormalContext$get_scales(attributes = names(private$scales))

#### Arguments

- `attributes`:

  (character) Name of the attributes for which scales (if applied) are
  returned.

#### Returns

The scales that have been applied to the specified attributes of the
formal context. If no `attributes` are passed, then all applied scales
are returned.

#### Examples

    filename <- system.file("contexts", "aromatic.csv", package = "fcaR")
    fc <- FormalContext$new(filename)
    fc$scale("nitro", "ordinal", comparison = `>=`, values = 1:3)
    fc$scale("OS", "nominal", c("O", "S"))
    fc$scale(attributes = "ring", type = "nominal")
    fc$get_scales()

------------------------------------------------------------------------

### Method `background_knowledge()`

Background knowledge of a scaled formal context

#### Usage

    FormalContext$background_knowledge()

#### Returns

An `ImplicationSet` with the implications extracted from the application
of scales.

#### Examples

    filename <- system.file("contexts", "aromatic.csv", package = "fcaR")
    fc <- FormalContext$new(filename)
    fc$scale("nitro", "ordinal", comparison = `>=`, values = 1:3)
    fc$scale("OS", "nominal", c("O", "S"))
    fc$scale(attributes = "ring", type = "nominal")
    fc$background_knowledge()

------------------------------------------------------------------------

### Method `dual()`

Get the dual formal context

#### Usage

    FormalContext$dual()

#### Returns

A `FormalContext` where objects and attributes have interchanged their
roles.

------------------------------------------------------------------------

### Method `intent()`

Get the intent of a fuzzy set of objects

#### Usage

    FormalContext$intent(S)

#### Arguments

- `S`:

  (`Set`) The set of objects to compute the intent for.

#### Returns

A `Set` with the intent.

------------------------------------------------------------------------

### Method `uparrow()`

Get the intent of a fuzzy set of objects

#### Usage

    FormalContext$uparrow(S)

#### Arguments

- `S`:

  (`Set`) The set of objects to compute the intent for.

#### Returns

A `Set` with the intent.

------------------------------------------------------------------------

### Method `extent()`

Get the extent of a fuzzy set of attributes

#### Usage

    FormalContext$extent(S)

#### Arguments

- `S`:

  (`Set`) The set of attributes to compute the extent for.

#### Returns

A `Set` with the intent.

------------------------------------------------------------------------

### Method `downarrow()`

Get the extent of a fuzzy set of attributes

#### Usage

    FormalContext$downarrow(S)

#### Arguments

- `S`:

  (`Set`) The set of attributes to compute the extent for.

#### Returns

A `Set` with the intent.

------------------------------------------------------------------------

### Method `closure()`

Get the closure of a fuzzy set of attributes

#### Usage

    FormalContext$closure(S)

#### Arguments

- `S`:

  (`Set`) The set of attributes to compute the closure for.

#### Returns

A `Set` with the closure.

------------------------------------------------------------------------

### Method `obj_concept()`

Object Concept

#### Usage

    FormalContext$obj_concept(object)

#### Arguments

- `object`:

  (character) Name of the object to compute its associated concept

#### Returns

The object concept associated to the object given.

------------------------------------------------------------------------

### Method `att_concept()`

Attribute Concept

#### Usage

    FormalContext$att_concept(attribute)

#### Arguments

- `attribute`:

  (character) Name of the attribute to compute its associated concept

#### Returns

The attribute concept associated to the attribute given.

------------------------------------------------------------------------

### Method `is_concept()`

Is a Concept?

#### Usage

    FormalContext$is_concept(C)

#### Arguments

- `C`:

  A `Concept` object

#### Returns

`TRUE` if `C` is a concept.

------------------------------------------------------------------------

### Method `is_closed()`

Testing closure of attribute sets

#### Usage

    FormalContext$is_closed(S)

#### Arguments

- `S`:

  A `Set` of attributes

#### Returns

`TRUE` if the set `S` is closed in this formal context.

------------------------------------------------------------------------

### Method `clarify()`

Clarify a formal context

#### Usage

    FormalContext$clarify(copy = FALSE)

#### Arguments

- `copy`:

  (logical) If `TRUE`, a new `FormalContext` object is created with the
  clarified context, otherwise the current one is overwritten.

#### Returns

The clarified `FormalContext`.

------------------------------------------------------------------------

### Method `reduce()`

Reduce a formal context

#### Usage

    FormalContext$reduce(copy = FALSE)

#### Arguments

- `copy`:

  (logical) If `TRUE`, a new `FormalContext` object is created with the
  clarified and reduced context, otherwise the current one is
  overwritten.

#### Returns

The clarified and reduced `FormalContext`.

------------------------------------------------------------------------

### Method `standardize()`

Build the Standard Context

#### Usage

    FormalContext$standardize()

#### Details

All concepts must be previously computed.

#### Returns

The standard context using the join- and meet- irreducible elements.

------------------------------------------------------------------------

### Method `find_concepts()`

Use Ganter Algorithm to compute concepts

#### Usage

    FormalContext$find_concepts(method = "InClose", verbose = FALSE)

#### Arguments

- `method`:

  (string) The name of a method for the computation of concepts.
  Available options can be listed with `conceptRegistry$get_entries()`.

- `verbose`:

  (logical) TRUE will provide a verbose output.

#### Returns

A list with all the concepts in the formal context.

------------------------------------------------------------------------

### Method `find_implications()`

Use modified Ganter algorithm to compute both concepts and implications

#### Usage

    FormalContext$find_implications(save_concepts = TRUE, verbose = FALSE)

#### Arguments

- `save_concepts`:

  (logical) `TRUE` will also compute and save the concept lattice.
  `FALSE` is usually faster, since it only computes implications.

- `verbose`:

  (logical) `TRUE` will provide a verbose output.

#### Returns

Nothing, just updates the internal fields `concepts` and `implications`.

------------------------------------------------------------------------

### Method [`find_causal_rules()`](https://neuroimaginador.github.io/fcaR/reference/find_causal_rules.md)

Find causal rules

#### Usage

    FormalContext$find_causal_rules(
      response_var,
      min_support = 0.1,
      confidence_level = 0.95,
      max_length = 3,
      verbose = FALSE
    )

#### Arguments

- `response_var`:

  (character) The name of the response variable.

- `min_support`:

  (numeric) Minimum support for the premise attributes.

- `confidence_level`:

  (numeric) Confidence level for the causality test.

- `max_length`:

  (integer) Maximum length of the premise.

- `verbose`:

  (logical) Show verbose output.

#### Returns

A `RuleSet` object containing the discovered causal rules and their
quality metrics.

------------------------------------------------------------------------

### Method `factorize()`

Factorize the formal context using Boolean/Fuzzy Matrix Factorization
algorithms.

#### Usage

    FormalContext$factorize(method = "GreConD", ...)

#### Arguments

- `method`:

  (character) The algorithm to use. Currently supported: "GreConD",
  "ASSO".

- `...`:

  Additional arguments:

  - For `GreConD`: `w` (weight, default 1.0), `stop_threshold_ratio`
    (error tolerance, default 0.0).

  - For `ASSO`: `threshold` (confidence threshold, default 0.7), `w_pos`
    (reward), `w_neg` (penalty).

#### Returns

A list with two `FormalContext` objects:

- `object_factor`: The context mapping Objects to Factors (Matrix A).

- `factor_attribute`: The context mapping Factors to Attributes (Matrix
  B).

------------------------------------------------------------------------

### Method `to_transactions()`

Convert the formal context to object of class `transactions` from the
`arules` package

#### Usage

    FormalContext$to_transactions()

#### Returns

A `transactions` object.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save a `FormalContext` to RDS or CXT format

#### Usage

    FormalContext$save(filename = tempfile(fileext = ".rds"))

#### Arguments

- `filename`:

  (character) Path of the file where to store the `FormalContext`.

#### Details

The format is inferred from the extension of the filename.

#### Returns

Invisibly the current `FormalContext`.

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

Load a `FormalContext` from a file

#### Usage

    FormalContext$load(filename)

#### Arguments

- `filename`:

  (character) Path of the file to load the `FormalContext` from.

#### Details

Currently, only RDS, CSV and CXT files are supported.

#### Returns

The loaded `FormalContext`.

------------------------------------------------------------------------

### Method [`dim()`](https://rdrr.io/r/base/dim.html)

Dimensions of the formal context

#### Usage

    FormalContext$dim()

#### Returns

A vector with (number of objects, number of attributes).

------------------------------------------------------------------------

### Method `to_json()`

Export the formal context to JSON

#### Usage

    FormalContext$to_json(file = NULL)

#### Arguments

- `file`:

  (character) The path of the file to save the JSON to.

#### Returns

A JSON string representing the formal context.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints the formal context

#### Usage

    FormalContext$print()

#### Returns

Prints information regarding the formal context.

------------------------------------------------------------------------

### Method `to_latex()`

Write the context in LaTeX format

#### Usage

    FormalContext$to_latex(table = TRUE, label = "", caption = "")

#### Arguments

- `table`:

  (logical) If `TRUE`, surrounds everything between `\begin{table}` and
  `\end{table}`.

- `label`:

  (character) The label for the table environment.

- `caption`:

  (character) The caption of the table.

- `fraction`:

  (character) If `none`, no fractions are produced. Otherwise, if it is
  `frac`, `dfrac` or `sfrac`, decimal numbers are represented as
  fractions with the corresponding LaTeX typesetting.

#### Returns

A table environment in LaTeX.

------------------------------------------------------------------------

### Method `incidence()`

Incidence matrix of the formal context

#### Usage

    FormalContext$incidence()

#### Returns

The incidence matrix of the formal context

#### Examples

    fc <- FormalContext$new(planets)
    fc$incidence()

------------------------------------------------------------------------

### Method `subcontext()`

Generates a new FormalContext restricted to a subset of objects and/or
attributes.

#### Usage

    FormalContext$subcontext(objects, attributes)

#### Arguments

- `objects`:

  (character or integer vector) The names or indices of the objects to
  keep. If NULL, keeps all.

- `attributes`:

  (character or integer vector) The names or indices of the attributes
  to keep. If NULL, keeps all.

#### Returns

A new `FormalContext` object representing the subcontext.

#### Examples

    fc <- FormalContext$new(planets)
    fc$subcontext(attributes = c("moon", "no_moon"))

------------------------------------------------------------------------

### Method `[()`

Subcontext of the formal context

#### Usage

    FormalContext$[(objects, attributes)

#### Arguments

- `objects`:

  (character array) Name of the objects to keep.

- `attributes`:

  (character array) Names of the attributes to keep.

#### Details

A warning will be issued if any of the names is not present in the list
of objects or attributes of the formal context.

If `objects` or `attributes` is empty, then it is assumed to represent
the whole set of objects or attributes of the original formal context.

#### Returns

Another `FormalContext` that is a subcontext of the original one, with
only the objects and attributes selected.

#### Examples

    fc <- FormalContext$new(planets)
    fc[, c("moon", "no_moon")]

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot the formal context table

#### Usage

    FormalContext$plot(to_latex = FALSE, ...)

#### Arguments

- `to_latex`:

  (logical) If `TRUE`, export the plot as a `tikzpicture` environment
  that can be included in a `LaTeX` file.

- `...`:

  Other parameters to be passed to the `tikzDevice` that renders the
  lattice in `LaTeX`, or for the figure caption. See `Details`.

#### Details

Particular parameters that control the size of the `tikz` output are:
`width`, `height` (both in inches), and `pointsize` (in points), that
should be set to the font size used in the `documentclass` header in the
`LaTeX` file where the code is to be inserted.

If a `caption` is provided, the whole `tikz` picture will be wrapped by
a `figure` environment and the caption set.

#### Returns

If `to_latex` is `FALSE`, it returns nothing, just plots the graph of
the formal context. Otherwise, this function returns the `LaTeX` code to
reproduce the formal context plot.

------------------------------------------------------------------------

### Method `use_logic()`

Sets the logic to use

#### Usage

    FormalContext$use_logic(name = available_logics())

#### Arguments

- `name`:

  The name of the logic to use. To see the available names, run
  `available_logics()`.

------------------------------------------------------------------------

### Method `get_logic()`

Gets the logic used

#### Usage

    FormalContext$get_logic()

#### Returns

A string with the name of the logic.

------------------------------------------------------------------------

### Method `use_connection()`

Sets the name of the Galois connection to use

#### Usage

    FormalContext$use_connection(connection)

#### Arguments

- `connection`:

  The name of the Galois connection. Available connections are
  "standard" (antitone), "benevolent1" and "benevolent2" (isotone)

------------------------------------------------------------------------

### Method `get_connection()`

Gets the name of the Galois connection

#### Usage

    FormalContext$get_connection()

#### Returns

A string with the name of the Galois connection

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FormalContext$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Build and print the formal context
fc_planets <- FormalContext$new(planets)
print(fc_planets)
#> FormalContext with 9 objects and 7 attributes.
#>          small  medium  large  near  far  moon  no_moon  
#>  Mercury   X                     X                 X     
#>    Venus   X                     X                 X     
#>    Earth   X                     X          X            
#>     Mars   X                     X          X            
#>  Jupiter                  X           X     X            
#>   Saturn                  X           X     X            
#>   Uranus           X                  X     X            
#>  Neptune           X                  X     X            
#>    Pluto   X                          X     X            

# Define a set of attributes
S <- Set$new(attributes = fc_planets$attributes)
S$assign(moon = 1, large = 1)

# Compute the closure of S
Sc <- fc_planets$closure(S)
# Is Sc a closed set?
fc_planets$is_closed(Sc)
#> [1] TRUE

# Clarify and reduce the formal context
fc2 <- fc_planets$reduce(TRUE)

# Find implications
fc_planets$find_implications()

# Read a formal context from CSV
filename <- system.file("contexts", "airlines.csv", package = "fcaR")
fc <- FormalContext$new(filename)

# Read a formal context from a CXT file
filename <- system.file("contexts", "lives_in_water.cxt", package = "fcaR")
fc <- FormalContext$new(filename)


## ------------------------------------------------
## Method `FormalContext$scale`
## ------------------------------------------------

filename <- system.file("contexts", "aromatic.csv", package = "fcaR")
fc <- FormalContext$new(filename)
fc$scale("nitro", "ordinal", comparison = `>=`, values = 1:3)
fc$scale("OS", "nominal", c("O", "S"))
fc$scale(attributes = "ring", type = "nominal")

## ------------------------------------------------
## Method `FormalContext$get_scales`
## ------------------------------------------------

filename <- system.file("contexts", "aromatic.csv", package = "fcaR")
fc <- FormalContext$new(filename)
fc$scale("nitro", "ordinal", comparison = `>=`, values = 1:3)
fc$scale("OS", "nominal", c("O", "S"))
fc$scale(attributes = "ring", type = "nominal")
fc$get_scales()
#> $nitro
#> FormalContext with 3 objects and 3 attributes.
#>    nitro >= 1  nitro >= 2  nitro >= 3  
#>  1      X                              
#>  2      X           X                  
#>  3      X           X           X      
#> 
#> $OS
#> FormalContext with 3 objects and 2 attributes.
#>    OS = O  OS = S  
#>                    
#>  O    X            
#>  S            X    
#> 
#> $ring
#> FormalContext with 2 objects and 2 attributes.
#>        ring = hex  ring = penta  
#>    hex      X                    
#>  penta                   X       
#> 

## ------------------------------------------------
## Method `FormalContext$background_knowledge`
## ------------------------------------------------

filename <- system.file("contexts", "aromatic.csv", package = "fcaR")
fc <- FormalContext$new(filename)
fc$scale("nitro", "ordinal", comparison = `>=`, values = 1:3)
fc$scale("OS", "nominal", c("O", "S"))
fc$scale(attributes = "ring", type = "nominal")
fc$background_knowledge()
#> Implication set with 0 implications.

## ------------------------------------------------
## Method `FormalContext$incidence`
## ------------------------------------------------

fc <- FormalContext$new(planets)
fc$incidence()
#>         small medium large near far moon no_moon
#> Mercury     1      0     0    1   0    0       1
#> Venus       1      0     0    1   0    0       1
#> Earth       1      0     0    1   0    1       0
#> Mars        1      0     0    1   0    1       0
#> Jupiter     0      0     1    0   1    1       0
#> Saturn      0      0     1    0   1    1       0
#> Uranus      0      1     0    0   1    1       0
#> Neptune     0      1     0    0   1    1       0
#> Pluto       1      0     0    0   1    1       0

## ------------------------------------------------
## Method `FormalContext$subcontext`
## ------------------------------------------------

fc <- FormalContext$new(planets)
fc$subcontext(attributes = c("moon", "no_moon"))
#> FormalContext with 9 objects and 2 attributes.
#>          moon  no_moon  
#>  Mercury          X     
#>    Venus          X     
#>    Earth   X            
#>     Mars   X            
#>  Jupiter   X            
#>   Saturn   X            
#>   Uranus   X            
#>  Neptune   X            
#>    Pluto   X            

## ------------------------------------------------
## Method `FormalContext$[`
## ------------------------------------------------

fc <- FormalContext$new(planets)
fc[, c("moon", "no_moon")]
#> FormalContext with 9 objects and 2 attributes.
#>          moon  no_moon  
#>  Mercury          X     
#>    Venus          X     
#>    Earth   X            
#>     Mars   X            
#>  Jupiter   X            
#>   Saturn   X            
#>   Uranus   X            
#>  Neptune   X            
#>    Pluto   X            
```
