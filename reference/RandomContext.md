# Generate Random Formal Contexts

Functions to generate synthetic formal contexts using advanced
statistical distributions. These methods allow creating datasets that
mimic real-world properties (non-uniform density) or randomizing
existing contexts while preserving their structural properties.

## Usage

``` r
RandomContext(
  n_objects,
  n_attributes,
  density = 0.1,
  distribution = "uniform",
  alpha = 1,
  ...
)
```

## Arguments

- n_objects:

  (integer) Number of objects.

- n_attributes:

  (integer) Number of attributes.

- density:

  (numeric) Expected density of the context (proportion of 1s). Used for
  uniform distribution.

- distribution:

  (character) The distribution to use for generating the context.

  - `"uniform"`: Each cell is 1 with probability `density`.

  - `"dirichlet"`: The number of attributes per object follows a
    categorical distribution derived from a Dirichlet distribution. This
    creates "clumpy" or "sparse" rows typical of real data.

- alpha:

  (numeric) Concentration parameter for the Dirichlet distribution. Low
  values (e.g., 0.1) produce very skewed distributions (some objects
  have few attributes, others many). High values produce more uniform
  row sums. Default is 1.0.

- ...:

  Additional arguments passed to internal methods.

## Value

A `FormalContext` object.

## Examples

``` r
# 1. Uniform Random Context
fc_uni <- RandomContext(10, 5, density = 0.2)
print(fc_uni)
#> FormalContext with 10 objects and 5 attributes.
#>      A1  A2  A3  A4  A5  
#>   O1          X          
#>   O2  X                  
#>   O3      X              
#>   O4          X          
#>   O5  X   X              
#>   O6                     
#>   O7                  X  
#>   O8              X   X  
#>   O9  X                  
#>  O10                     

# 2. Dirichlet Random Context (Mimicking real data structure)
# Objects will have varying 'sizes' (number of attributes)
fc_dir <- RandomContext(10, 5, distribution = "dirichlet", alpha = 0.5)
print(fc_dir)
#> FormalContext with 10 objects and 5 attributes.
#>      A1  A2  A3  A4  A5  
#>   O1  X   X              
#>   O2  X           X      
#>   O3  X   X   X   X   X  
#>   O4      X   X          
#>   O5  X   X   X   X   X  
#>   O6  X       X          
#>   O7  X       X          
#>   O8  X   X   X   X   X  
#>   O9      X       X      
#>  O10  X   X              
```
