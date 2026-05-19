# Crossing several factor variables

Create new factor variable by crossing levels in several variables

## Usage

``` r
CrossStrata(by, sep = "-", returnb = FALSE, asNumeric = FALSE, byExtra = NULL)
```

## Arguments

- by:

  Dataframe or matrix with several variables

- sep:

  Used to create new level names

- returnb:

  When TRUE an overview of original variabels according to new levels
  are also retuned.

- asNumeric:

  When TRUE the new variable is numeric.

- byExtra:

  Contains the same variables as by and represents another data set.

## Value

- a:

  The new variable

- aExtra:

  New variable according to byExtra

- b:

  Overview of original variabels according to new levels

## Examples

``` r

CrossStrata(cbind(factor(rep(1:3,2)),c('A',rep('B',5)) ))
#> [1] 1-A 2-B 3-B 1-B 2-B 3-B
#> Levels: 1-A 1-B 2-B 3-B
```
