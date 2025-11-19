# Scaling Registry

Scaling Registry

## Usage

``` r
scalingRegistry
```

## Format

An object of class `scaling_registry` (inherits from `registry`) of
length 6.

## Details

This is a registry that stores the implemented scales that can be
applied using the [`scale()`](https://rdrr.io/r/base/scale.html) method
in an `FormalContext`.

One can obtain the list of available equivalence operators by:
`scalingRegistry$get_entry_names()`
