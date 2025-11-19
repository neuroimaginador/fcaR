# Concept Miners Registry

Concept Miners Registry

## Usage

``` r
conceptRegistry
```

## Format

An object of class `concept_miner_registry` (inherits from `registry`)
of length 3.

## Details

This is a registry that stores the concept miners that can be applied
using the `find_concepts()` method in an `FormalConcept`.

One can obtain the list of available equivalence operators by:
`conceptRegistry$get_entry_names()`
