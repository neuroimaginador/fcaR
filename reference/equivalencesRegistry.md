# Equivalence Rules Registry

Equivalence Rules Registry

## Usage

``` r
equivalencesRegistry
```

## Format

An object of class `equivalence_registry` (inherits from `registry`) of
length 6.

## Details

This is a registry that stores the equivalence rules that can be applied
using the `apply_rules()` method in an `ImplicationSet`.

One can obtain the list of available equivalence operators by:
`equivalencesRegistry$get_entry_names()`
