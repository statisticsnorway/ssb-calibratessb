# OrderedVarNames

OrderedVarNames

## Usage

``` r
OrderedVarNames(x, sep = ":")
```

## Arguments

- x:

  input

- sep:

  character string used to separate

## Value

output

## Examples

``` r
z <- data.frame(A = factor(c("a", "b", "c")), B = factor(1:2), C = 1:6)
x <- colnames(model.matrix(~B * C * A, z))
OrderedVarNames(x)
#>  [1] "(Intercept)" "B2"          "C"           "Ab"          "Ac"         
#>  [6] "B2:C"        "Ab:B2"       "Ac:B2"       "Ab:C"        "Ac:C"       
#> [11] "Ab:B2:C"     "Ac:B2:C"    
```
