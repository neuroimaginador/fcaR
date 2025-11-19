# Parses several implications given as a string

Parses several implications given as a string

## Usage

``` r
parse_implications(input)
```

## Arguments

- input:

  (character) The string with the implications or a file containing the
  implications

## Value

An ImplicationSet

## Details

The format for the input file is:

- Every implication in its own line or separated by semicolon (;)

- Attributes are separated by commas (,)

- The LHS and RHS of each implication are separated by an arrow (-\>)

## Examples

``` r
input <- system.file("implications", "ex_implications", package = "fcaR")
imps <- parse_implications(input)
```
