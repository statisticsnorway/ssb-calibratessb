# MatchVarNames

MatchVarNames

## Usage

``` r
MatchVarNames(x, y, sep = ":", makeWarning = FALSE)
```

## Arguments

- x:

  x

- y:

  y

- sep:

  sep

- makeWarning:

  Warning when matching by reordering

## Value

An integer vector giving the position in y of the first match if there
is a match, otherwise NA.

## Examples

``` r
z <- data.frame(A = factor(c("a", "b", "c")), B = factor(1:2), C = 1:6)
x <- colnames(model.matrix(~B * C * A, z))
y <- colnames(model.matrix(~A * B + A:B:C, z))
MatchVarNames(x, y)
#>  [1]  1  4 NA  2  3 NA  5  6 NA NA 11 12
```
