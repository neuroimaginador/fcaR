# Conceptual Scaling

## Introduction and Motivation

Let us consider the following formal context about intern students:

|           | Grant | Intern |         Agreement          |   Attitude   | Score |
|:----------|------:|:------:|:--------------------------:|:------------:|:-----:|
| Student 1 |   yes |   7    |           agree            |   working    |  2.7  |
| Student 2 |   yes |   10   |       strongly agree       | hard working |  4.1  |
| Student 3 |    no |   5    | neither agree nor disagree |   working    |  3.6  |
| Student 4 |   yes |   8    |           agree            |   working    |  4.0  |
| Student 5 |    no |   4    |          disagree          |     lazy     |  3.6  |

The attributes are:

- *Grant*: a binary attribute that expresses whether the candidate is
  has received a grant.
- *Intern*: elapsed time as intern.
- *Agreement*: 1:strongly disagree, 2:disagree, 3:neither agree nor
  disagree, 4:agree and 5:strongly agree, with the work dynamics in the
  internship.
- *Attitude*: 1:hard working, 2:working, 3:lazy, 4:very lazy.
- *Score*: the score obtained in the internship procedure, with a
  maximum of 5.

This is a *many-valued context*, where we have mixed categorical and
numerical attributes.

In such formal context, we cannot derive concepts and implications
directly, we need to transform the formal context into one where each
attribute is in the interval $\lbrack 0,1\rbrack$.

## Types of Scaling

The *transformations* to the attributes of a many-valued formal context
are called *scaling*. Depending on the meaning of each attribute,
different types of scaling can be applied.

### Nominal scaling

**Nominal** scales are used for scaling attributes whose values exclude
each other.

For instance, in the example above, the attribute `Grant` is
categorical:

|           | Grant |
|:----------|------:|
| Student 1 |   yes |
| Student 2 |   yes |
| Student 3 |    no |
| Student 4 |   yes |
| Student 5 |    no |

and can be transformed into the following one:

|           | Grant = no | Grant = yes |
|:----------|:----------:|:-----------:|
| Student 1 |     0      |      1      |
| Student 2 |     0      |      1      |
| Student 3 |     1      |      0      |
| Student 4 |     0      |      1      |
| Student 5 |     1      |      0      |

The mapping from the previous attribute values to the derived context
is:

|     | Grant = no | Grant = yes |
|:----|:----------:|:-----------:|
| no  |     1      |      0      |
| yes |     0      |      1      |

### Ordinal scaling

**Ordinal** scales are used for attributes with ordered values, where
each value implies the smaller values (e.g. the number of children of an
individual).

In our example, the attribute `Intern` is ordinal:

|           | Intern |
|:----------|:------:|
| Student 1 |   7    |
| Student 2 |   10   |
| Student 3 |   5    |
| Student 4 |   8    |
| Student 5 |   4    |

and can be transformed into:

|           | Intern \<= 4 | Intern \<= 5 | Intern \<= 7 | Intern \<= 8 | Intern \<= 10 |
|:----------|:------------:|:------------:|:------------:|:------------:|:-------------:|
| Student 1 |      0       |      0       |      1       |      1       |       1       |
| Student 2 |      0       |      0       |      0       |      0       |       1       |
| Student 3 |      0       |      1       |      1       |      1       |       1       |
| Student 4 |      0       |      0       |      0       |      1       |       1       |
| Student 5 |      1       |      1       |      1       |      1       |       1       |

using the following scale:

|     | Intern \<= 4 | Intern \<= 5 | Intern \<= 7 | Intern \<= 8 | Intern \<= 10 |
|:----|:------------:|:------------:|:------------:|:------------:|:-------------:|
| 4   |      1       |      1       |      1       |      1       |       1       |
| 5   |      0       |      1       |      1       |      1       |       1       |
| 7   |      0       |      0       |      1       |      1       |       1       |
| 8   |      0       |      0       |      0       |      1       |       1       |
| 10  |      0       |      0       |      0       |      0       |       1       |

### Interordinal scaling

**Interordinal** scales are used in attributes that express different
degrees (for instance, the Likert scale: *strongly disagree*,
*disagree*, *neither agree nor disagree*, *agree* and *strongly agree*).

In our example, the attribute `Agreement` is interordinal:

|           |         Agreement          |
|:----------|:--------------------------:|
| Student 1 |           agree            |
| Student 2 |       strongly agree       |
| Student 3 | neither agree nor disagree |
| Student 4 |           agree            |
| Student 5 |          disagree          |

and can be transformed into:

|           | Agreement \<= strongly disagree | Agreement \<= disagree | Agreement \<= neither agree nor disagree | Agreement \<= agree | Agreement \<= strongly agree | Agreement \>= strongly disagree | Agreement \>= disagree | Agreement \>= neither agree nor disagree | Agreement \>= agree | Agreement \>= strongly agree |
|:----------|:-------------------------------:|:----------------------:|:----------------------------------------:|:-------------------:|:----------------------------:|:-------------------------------:|:----------------------:|:----------------------------------------:|:-------------------:|:----------------------------:|
| Student 1 |                0                |           0            |                    0                     |          1          |              1               |                1                |           1            |                    1                     |          1          |              0               |
| Student 2 |                0                |           0            |                    0                     |          0          |              1               |                1                |           1            |                    1                     |          1          |              1               |
| Student 3 |                0                |           0            |                    1                     |          1          |              1               |                1                |           1            |                    1                     |          0          |              0               |
| Student 4 |                0                |           0            |                    0                     |          1          |              1               |                1                |           1            |                    1                     |          1          |              0               |
| Student 5 |                0                |           1            |                    1                     |          1          |              1               |                1                |           1            |                    0                     |          0          |              0               |

using the following scale:

|                            | Agreement \<= strongly disagree | Agreement \<= disagree | Agreement \<= neither agree nor disagree | Agreement \<= agree | Agreement \<= strongly agree | Agreement \>= strongly disagree | Agreement \>= disagree | Agreement \>= neither agree nor disagree | Agreement \>= agree | Agreement \>= strongly agree |
|:---------------------------|:-------------------------------:|:----------------------:|:----------------------------------------:|:-------------------:|:----------------------------:|:-------------------------------:|:----------------------:|:----------------------------------------:|:-------------------:|:----------------------------:|
| strongly disagree          |                1                |           1            |                    1                     |          1          |              1               |                1                |           0            |                    0                     |          0          |              0               |
| disagree                   |                0                |           1            |                    1                     |          1          |              1               |                1                |           1            |                    0                     |          0          |              0               |
| neither agree nor disagree |                0                |           0            |                    1                     |          1          |              1               |                1                |           1            |                    1                     |          0          |              0               |
| agree                      |                0                |           0            |                    0                     |          1          |              1               |                1                |           1            |                    1                     |          1          |              0               |
| strongly agree             |                0                |           0            |                    0                     |          0          |              1               |                1                |           1            |                    1                     |          1          |              1               |

### Biordinal scaling

**Biordinal** scales are used when the attribute values express a degree
in one of two poles (for instance, *1:very silent*, *2:silent*,
*3:loud*, *4:very loud*, where *1:very silent* implies *2:silent*, and
*4:very loud* implies *3:loud*, but neither *silent* implies *loud* nor
*loud* implies *silent*).

In our example, the attribute `Attitude` is biordinal:

|           |   Attitude   |
|:----------|:------------:|
| Student 1 |   working    |
| Student 2 | hard working |
| Student 3 |   working    |
| Student 4 |   working    |
| Student 5 |     lazy     |

and can be transformed into:

|           | Attitude \<= hard working | Attitude \<= working | Attitude \>= lazy | Attitude \>= very lazy |
|:----------|:-------------------------:|:--------------------:|:-----------------:|:----------------------:|
| Student 1 |             0             |          1           |         0         |           0            |
| Student 2 |             1             |          1           |         0         |           0            |
| Student 3 |             0             |          1           |         0         |           0            |
| Student 4 |             0             |          1           |         0         |           0            |
| Student 5 |             0             |          0           |         1         |           0            |

using the following scale:

|              | Attitude \<= hard working | Attitude \<= working | Attitude \>= lazy | Attitude \>= very lazy |
|:-------------|:-------------------------:|:--------------------:|:-----------------:|:----------------------:|
| hard working |             1             |          1           |         0         |           0            |
| working      |             0             |          1           |         0         |           0            |
| lazy         |             0             |          0           |         1         |           0            |
| very lazy    |             0             |          0           |         1         |           1            |

### Interval scaling

**Interval** scales group continuous attributes in intervals or bins.

In our example, the attribute `Score` is continuous:

|           | Score |
|:----------|:-----:|
| Student 1 |  2.7  |
| Student 2 |  4.1  |
| Student 3 |  3.6  |
| Student 4 |  4.0  |
| Student 5 |  3.6  |

and can be categorized into intervals corresponding to the following
marks:

- *A*: score in the interval $(4,5\rbrack$.
- *B*: score in the interval $(3,4\rbrack$.
- *C*: score in the interval $(2,3\rbrack$.
  |           | Score is C | Score is B | Score is A |
  |:----------|:----------:|:----------:|:----------:|
  | Student 1 |     1      |     0      |     0      |
  | Student 2 |     0      |     0      |     1      |
  | Student 3 |     0      |     1      |     0      |
  | Student 4 |     0      |     1      |     0      |
  | Student 5 |     0      |     1      |     0      |

using the following scale:

|     | Score is C | Score is B | Score is A |
|:----|:----------:|:----------:|:----------:|
| 2.7 |     1      |     0      |     0      |
| 3.6 |     0      |     1      |     0      |
| 4   |     0      |     1      |     0      |
| 4.1 |     0      |     0      |     1      |

## Scaling in `fcaR`

Let us see how we can perform scaling with `fcaR`.

### Available scales

The available scales are stored in a `registry` object called
`scalingRegistry`, which keeps the functions to perform the different
types of scaling:

``` r
scalingRegistry$get_entry_names()
#> [1] "Nominal"      "Ordinal"      "Interordinal" "Biordinal"    "Interval"    
#> [6] "Implication"
```

### Applying scales

In order to scale an attribute of a `FormalContext`, we use the method
`scale`.

This method has two mandatory arguments: `attribute` (the attribute to
scale) and `type` (the type of scaling). Additional arguments can be
supplied for certain types of scaling, as we will se shortly.

For instance, if we call `fc` the formal context of the example at the
beginning of this document, we can replicate the above scales:

``` r
fc$scale("Grant", type = "nominal")
fc
#> FormalContext with 5 objects and 6 attributes.
#> # A tibble: 5 × 6
#>   `Grant = no` `Grant = yes` Intern Agreement                  Attitude    Score
#>          <dbl>         <dbl>  <dbl> <chr>                      <chr>       <dbl>
#> 1            0             1      7 agree                      working       2.7
#> 2            0             1     10 strongly agree             hard worki…   4.1
#> 3            1             0      5 neither agree nor disagree working       3.6
#> 4            0             1      8 agree                      working       4  
#> 5            1             0      4 disagree                   lazy          3.6
```

Note that the attribute `Grant` has been substituted by `Grant = yes`
and `Grant = no`.

In order to apply the *ordinal* scaling to the `Intern` attribute, we
do:

``` r
fc$scale("Intern", type = "ordinal")
fc
#> FormalContext with 5 objects and 10 attributes.
#> # A tibble: 5 × 10
#>   `Grant = no` `Grant = yes` `Intern <= 4` `Intern <= 5` `Intern <= 7`
#>          <dbl>         <dbl>         <dbl>         <dbl>         <dbl>
#> 1            0             1             0             0             1
#> 2            0             1             0             0             0
#> 3            1             0             0             1             1
#> 4            0             1             0             0             0
#> 5            1             0             1             1             1
#> # ℹ 5 more variables: `Intern <= 8` <dbl>, `Intern <= 10` <dbl>,
#> #   Agreement <chr>, Attitude <chr>, Score <dbl>
```

Now, the `Agreement` attribute can be scaled using and interordinal
scaling:

``` r
fc$scale("Agreement",
  type = "interordinal",
  values = c(
    "strongly disagree",
    "disagree",
    "neither agree nor disagree",
    "agree",
    "strongly agree"
  )
)
```

The resulting formal context has 19 columns, thus it is difficult to
print.

Note that the call to [`scale()`](https://rdrr.io/r/base/scale.html) has
an additional optional argument, `values`, that indicate, for character
attributes, the order between the different attribute values. In this
case, the order is “strongly disagree” \< “disagree” \< “neither agree
nor disagree” \< “agree” \< “strongly agree”.

The `Attitude` attribute can be scaled using a *biordinal* scale, since
it represents two poles: the first is given by values “hard working” and
“working” and the other is given by “very lazy” and “lazy”. Then, we can
do:

``` r
fc$scale("Attitude",
  type = "biordinal",
  values_le = c("hard working", "working"),
  values_ge = c("lazy", "very lazy")
)
```

The full order of attribute values is “hard working” \< “working” \<
“lazy” \< “very lazy”.

Note that we have listed the attribute values in two different
arguments, `values_le` (for values that are compared using `<=`) and
`values_ge` (values compared using `>=`). Also note that all attributes
must appear ordered in the arguments, that is, in the same order as in
the full order defined above.

The last attribute was the `Score`, that can be grouped in intervals
using:

``` r
fc$scale("Score",
  type = "interval",
  values = c(2, 3, 4, 5),
  interval_names = c("C", "B", "A")
)
```

The additional arguments are `values` (the endpoints of the intervals)
and `interval_names` (an optional name indicating how to call a given
interval).

Note that all of these scales can be applied to numerical and string
attributes.

### Scale contexts

In order to see the transformation (the *scale* context) used for any of
the attributes, we use the `get_scales()` method of a `FormalContext`:

``` r
fc$get_scales(c("Grant", "Score"))
#> $Grant
#> FormalContext with 2 objects and 2 attributes.
#>      Grant = no  Grant = yes  
#>   no      X                   
#>  yes                  X       
#> 
#> $Score
#> FormalContext with 4 objects and 3 attributes.
#>      Score is C  Score is B  Score is A  
#>  2.7      X                              
#>  3.6                  X                  
#>    4                  X                  
#>  4.1                              X
```

With no arguments, it prints all the scales that have been used in a
`FormalContext`.

### Background knowledge

Some scalings, particularly of the *ordinal* family, assume an implicit
knowledge (e.g., if attribute `Intern` is lower than 5, then it is also
lower than 7).

This *background knowledge* is computed after each scaling is performed,
and can be printed with:

``` r
fc$background_knowledge()
#> Implication set with 0 implications.
```

It is a simple `ImplicationSet` that stores all the implications that
can be derived from the scale contexts.

### Concepts and implications

As usual, for a given `FormalContext`, binary or fuzzy, one can compute
its concept lattice and its basis of implications. In the case of
*many-valued* contexts, one has first to scale all necessary attributes,
such that the resulting context is binary or fuzzy.

Once scaled, the same `find_concepts()` and `find_implications()`
methods can be used to compute the lattice and the basis.

In the case of the basis of implications, the NextClosure algorithm is
used, as in the binary/fuzzy case, but the resulting implications have
been post-processed to remove redundant information with respect to the
*background knowledge*.

In our example, the resulting implications are:

``` r
fc$find_implications()
fc$implications
#> Implication set with 26 implications.
#> Rule 1: {} -> {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree}
#> Rule 2: {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Score is A} -> {Grant = yes, Agreement >=
#>   neither agree nor disagree, Agreement >= agree, Agreement >= strongly agree,
#>   Attitude <= hard working, Attitude <= working}
#> Rule 3: {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Score is B} -> {Intern <= 8, Agreement <=
#>   agree}
#> Rule 4: {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Score is C} -> {Grant = yes, Intern <= 7,
#>   Intern <= 8, Agreement <= agree, Agreement >= neither agree nor disagree,
#>   Agreement >= agree, Attitude <= working}
#> Rule 5: {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Attitude >= very lazy} -> {Grant = no,
#>   Grant = yes, Intern <= 4, Intern <= 5, Intern <= 7, Intern <= 8, Agreement
#>   <= strongly disagree, Agreement <= disagree, Agreement <= neither agree
#>   nor disagree, Agreement <= agree, Agreement >= neither agree nor disagree,
#>   Agreement >= agree, Agreement >= strongly agree, Attitude <= hard working,
#>   Attitude <= working, Attitude >= lazy, Score is C, Score is B, Score is A}
#> Rule 6: {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Attitude >= lazy} -> {Grant = no, Intern <=
#>   4, Intern <= 5, Intern <= 7, Intern <= 8, Agreement <= disagree, Agreement <=
#>   neither agree nor disagree, Agreement <= agree, Score is B}
#> Rule 7: {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Attitude <= working} -> {Agreement >= neither
#>   agree nor disagree}
#> Rule 8: {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Attitude <= hard working} -> {Grant = yes,
#>   Agreement >= neither agree nor disagree, Agreement >= agree, Agreement >=
#>   strongly agree, Attitude <= working, Score is A}
#> Rule 9: {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Agreement >= strongly agree} -> {Grant = yes,
#>   Agreement >= neither agree nor disagree, Agreement >= agree, Attitude <= hard
#>   working, Attitude <= working, Score is A}
#> Rule 10: {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Agreement >= agree} -> {Grant = yes,
#>   Agreement >= neither agree nor disagree, Attitude <= working}
#> Rule 11: {Intern <= 10, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Agreement >= neither agree nor disagree} ->
#>   {Attitude <= working}
#> Rule 12: {Intern <= 10, Agreement <= agree, Agreement <= strongly agree,
#>   Agreement >= strongly disagree, Agreement >= disagree} -> {Intern <= 8}
#> Rule 13: {Intern <= 10, Agreement <= neither agree nor disagree, Agreement <=
#>   strongly agree, Agreement >= strongly disagree, Agreement >= disagree} ->
#>   {Grant = no, Intern <= 5, Intern <= 7, Intern <= 8, Agreement <= agree, Score
#>   is B}
#> Rule 14: {Intern <= 10, Agreement <= disagree, Agreement <= strongly agree,
#>   Agreement >= strongly disagree, Agreement >= disagree} -> {Grant = no, Intern
#>   <= 4, Intern <= 5, Intern <= 7, Intern <= 8, Agreement <= neither agree nor
#>   disagree, Agreement <= agree, Attitude >= lazy, Score is B}
#> Rule 15: {Intern <= 10, Agreement <= strongly disagree, Agreement <= strongly
#>   agree, Agreement >= strongly disagree, Agreement >= disagree} -> {Grant = no,
#>   Grant = yes, Intern <= 4, Intern <= 5, Intern <= 7, Intern <= 8, Agreement
#>   <= disagree, Agreement <= neither agree nor disagree, Agreement <= agree,
#>   Agreement >= neither agree nor disagree, Agreement >= agree, Agreement >=
#>   strongly agree, Attitude <= hard working, Attitude <= working, Attitude >=
#>   lazy, Attitude >= very lazy, Score is C, Score is B, Score is A}
#> Rule 16: {Intern <= 8, Intern <= 10, Agreement <= strongly agree, Agreement >=
#>   strongly disagree, Agreement >= disagree} -> {Agreement <= agree}
#> Rule 17: {Intern <= 7, Intern <= 10, Agreement <= strongly agree, Agreement >=
#>   strongly disagree, Agreement >= disagree} -> {Intern <= 8, Agreement <= agree}
#> Rule 18: {Intern <= 7, Intern <= 8, Intern <= 10, Agreement <= agree, Agreement
#>   <= strongly agree, Agreement >= strongly disagree, Agreement >= disagree,
#>   Score is B} -> {Grant = no, Intern <= 5, Agreement <= neither agree nor
#>   disagree}
#> Rule 19: {Intern <= 5, Intern <= 10, Agreement <= strongly agree, Agreement >=
#>   strongly disagree, Agreement >= disagree} -> {Grant = no, Intern <= 7, Intern
#>   <= 8, Agreement <= neither agree nor disagree, Agreement <= agree, Score is B}
#> Rule 20: {Intern <= 4, Intern <= 10, Agreement <= strongly agree, Agreement >=
#>   strongly disagree, Agreement >= disagree} -> {Grant = no, Intern <= 5, Intern
#>   <= 7, Intern <= 8, Agreement <= disagree, Agreement <= neither agree nor
#>   disagree, Agreement <= agree, Attitude >= lazy, Score is B}
#> Rule 21: {Grant = yes, Intern <= 10, Agreement <= strongly agree, Agreement >=
#>   strongly disagree, Agreement >= disagree} -> {Agreement >= neither agree nor
#>   disagree, Agreement >= agree, Attitude <= working}
#> Rule 22: {Grant = yes, Intern <= 8, Intern <= 10, Agreement <= agree, Agreement
#>   <= strongly agree, Agreement >= strongly disagree, Agreement >= disagree,
#>   Agreement >= neither agree nor disagree, Agreement >= agree, Agreement >=
#>   strongly agree, Attitude <= hard working, Attitude <= working, Score is A}
#>   -> {Grant = no, Intern <= 4, Intern <= 5, Intern <= 7, Agreement <= strongly
#>   disagree, Agreement <= disagree, Agreement <= neither agree nor disagree,
#>   Attitude >= lazy, Attitude >= very lazy, Score is C, Score is B}
#> Rule 23: {Grant = yes, Intern <= 7, Intern <= 8, Intern <= 10, Agreement <=
#>   agree, Agreement <= strongly agree, Agreement >= strongly disagree, Agreement
#>   >= disagree, Agreement >= neither agree nor disagree, Agreement >= agree,
#>   Attitude <= working} -> {Score is C}
#> Rule 24: {Grant = no, Intern <= 10, Agreement <= strongly agree, Agreement >=
#>   strongly disagree, Agreement >= disagree} -> {Intern <= 5, Intern <= 7, Intern
#>   <= 8, Agreement <= neither agree nor disagree, Agreement <= agree, Score is B}
#> Rule 25: {Grant = no, Intern <= 4, Intern <= 5, Intern <= 7, Intern <= 8,
#>   Intern <= 10, Agreement <= disagree, Agreement <= neither agree nor disagree,
#>   Agreement <= agree, Agreement <= strongly agree, Agreement >= strongly
#>   disagree, Agreement >= disagree, Agreement >= neither agree nor disagree,
#>   Attitude <= working, Attitude >= lazy, Score is B} -> {Grant = yes, Agreement
#>   <= strongly disagree, Agreement >= agree, Agreement >= strongly agree,
#>   Attitude <= hard working, Attitude >= very lazy, Score is C, Score is A}
#> Rule 26: {Grant = no, Grant = yes, Intern <= 5, Intern <= 7, Intern <= 8, Intern
#>   <= 10, Agreement <= neither agree nor disagree, Agreement <= agree, Agreement
#>   <= strongly agree, Agreement >= strongly disagree, Agreement >= disagree,
#>   Agreement >= neither agree nor disagree, Agreement >= agree, Attitude <=
#>   working, Score is C, Score is B} -> {Intern <= 4, Agreement <= strongly
#>   disagree, Agreement <= disagree, Agreement >= strongly agree, Attitude <= hard
#>   working, Attitude >= lazy, Attitude >= very lazy, Score is A}
```

Note that, in this case, this is not the basis of implications, since
the *background knowledge* has to be incorporated. That is, these
implications are valid, but to be complete they need the implications
derived from the scales.

## Another example

Let us consider the following context of attributes of aromatic
molecules:

``` r
filename <- system.file("contexts",
  "aromatic.csv",
  package = "fcaR"
)

fc <- FormalContext$new(filename)
```

``` r
fc$incidence() %>%
  knitr::kable(
    format = "html",
    align = "c"
  )
```

|                | ring  | OS  | nitro |
|:---------------|:-----:|:---:|:-----:|
| Benzene        |  hex  |     |   0   |
| Furan          | penta |  O  |   0   |
| Imidazole      | penta |     |   2   |
| 1-3-Oxazole    | penta |  O  |   1   |
| Pyrazine       |  hex  |     |   2   |
| Pyrazole       | penta |     |   2   |
| Pyridine       |  hex  |     |   1   |
| Pyrimidine     |  hex  |     |   2   |
| Pyrrole        | penta |     |   1   |
| Thiazole       | penta |  S  |   1   |
| Thiophene      | penta |  S  |   0   |
| 1-3-5-Triazine |  hex  |     |   3   |

The meaning of the attributes is as follows:

- `ring` represents the shape (pentagon or hexagon) of the molecule
  ring.
- `OS` means the presence of oxygen (O) or sulfur (S) atoms.
- `nitro` represents the number of nitrogen atoms.

We can transform this context into a binary one by means of the
following scalings:

``` r
fc$scale(
  attributes = "nitro",
  type = "ordinal",
  comparison = `>=`,
  values = 1:3
)
fc$scale(
  attributes = "OS",
  type = "nominal",
  c("O", "S")
)
fc$scale(
  attributes = "ring",
  type = "nominal"
)
```

The final formal context is:

|                | ring = hex | ring = penta | OS = O | OS = S | nitro \>= 1 | nitro \>= 2 | nitro \>= 3 |
|:---------------|:----------:|:------------:|:------:|:------:|:-----------:|:-----------:|:-----------:|
| Benzene        |     1      |      0       |   0    |   0    |      0      |      0      |      0      |
| Furan          |     0      |      1       |   1    |   0    |      0      |      0      |      0      |
| Imidazole      |     0      |      1       |   0    |   0    |      1      |      1      |      0      |
| 1-3-Oxazole    |     0      |      1       |   1    |   0    |      1      |      0      |      0      |
| Pyrazine       |     1      |      0       |   0    |   0    |      1      |      1      |      0      |
| Pyrazole       |     0      |      1       |   0    |   0    |      1      |      1      |      0      |
| Pyridine       |     1      |      0       |   0    |   0    |      1      |      0      |      0      |
| Pyrimidine     |     1      |      0       |   0    |   0    |      1      |      1      |      0      |
| Pyrrole        |     0      |      1       |   0    |   0    |      1      |      0      |      0      |
| Thiazole       |     0      |      1       |   0    |   1    |      1      |      0      |      0      |
| Thiophene      |     0      |      1       |   0    |   1    |      0      |      0      |      0      |
| 1-3-5-Triazine |     1      |      0       |   0    |   0    |      1      |      1      |      1      |

The implications derived from the scales are:

``` r
fc$background_knowledge()
#> Implication set with 0 implications.
```

Also, we can compute the implications that are not represented by the
*background knowledge*:

``` r
fc$find_implications()
fc$implications
#> Implication set with 8 implications.
#> Rule 1: {nitro >= 3} -> {ring = hex, nitro >= 1, nitro >= 2}
#> Rule 2: {nitro >= 2} -> {nitro >= 1}
#> Rule 3: {OS = S} -> {ring = penta}
#> Rule 4: {OS = O} -> {ring = penta}
#> Rule 5: {ring = penta, OS = S, nitro >= 1, nitro >= 2} -> {ring = hex, OS = O,
#>   nitro >= 3}
#> Rule 6: {ring = penta, OS = O, nitro >= 1, nitro >= 2} -> {ring = hex, OS = S,
#>   nitro >= 3}
#> Rule 7: {ring = penta, OS = O, OS = S} -> {ring = hex, nitro >= 1, nitro >= 2,
#>   nitro >= 3}
#> Rule 8: {ring = hex, ring = penta} -> {OS = O, OS = S, nitro >= 1, nitro >= 2,
#>   nitro >= 3}
```

and the concepts:

``` r
fc$concepts
#> A set of 15 concepts:
#> 1: ({Benzene, Furan, Imidazole, 1-3-Oxazole, Pyrazine, Pyrazole, Pyridine, Pyrimidine, Pyrrole, Thiazole, Thiophene, 1-3-5-Triazine}, {})
#> 2: ({Imidazole, 1-3-Oxazole, Pyrazine, Pyrazole, Pyridine, Pyrimidine, Pyrrole, Thiazole, 1-3-5-Triazine}, {nitro >= 1})
#> 3: ({Imidazole, Pyrazine, Pyrazole, Pyrimidine, 1-3-5-Triazine}, {nitro >= 1, nitro >= 2})
#> 4: ({Furan, Imidazole, 1-3-Oxazole, Pyrazole, Pyrrole, Thiazole, Thiophene}, {ring = penta})
#> 5: ({Imidazole, 1-3-Oxazole, Pyrazole, Pyrrole, Thiazole}, {ring = penta, nitro >= 1})
#> 6: ({Imidazole, Pyrazole}, {ring = penta, nitro >= 1, nitro >= 2})
#> 7: ({Thiazole, Thiophene}, {ring = penta, OS = S})
#> 8: ({Thiazole}, {ring = penta, OS = S, nitro >= 1})
#> 9: ({Furan, 1-3-Oxazole}, {ring = penta, OS = O})
#> 10: ({1-3-Oxazole}, {ring = penta, OS = O, nitro >= 1})
#> 11: ({Benzene, Pyrazine, Pyridine, Pyrimidine, 1-3-5-Triazine}, {ring = hex})
#> 12: ({Pyrazine, Pyridine, Pyrimidine, 1-3-5-Triazine}, {ring = hex, nitro >= 1})
#> 13: ({Pyrazine, Pyrimidine, 1-3-5-Triazine}, {ring = hex, nitro >= 1, nitro >= 2})
#> 14: ({1-3-5-Triazine}, {ring = hex, nitro >= 1, nitro >= 2, nitro >= 3})
#> 15: ({}, {ring = hex, ring = penta, OS = O, OS = S, nitro >= 1, nitro >= 2, nitro >= 3})
```
