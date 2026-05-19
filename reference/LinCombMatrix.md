# Creation of linear combination matrices

Create matrices for changes (LagDiff), means (Period) and mean changes
(PeriodDiff).

## Usage

``` r
LinCombMatrix(
  n,
  period = NULL,
  lag = NULL,
  k = 0,
  takeMean = TRUE,
  removerows = TRUE,
  overlap = FALSE
)

LagDiff(n, lag = 1, removerows = TRUE)

Period(
  n,
  period = 1,
  k = 0,
  takeMean = TRUE,
  removerows = TRUE,
  overlap = FALSE
)

PeriodDiff(
  n,
  period = 1,
  lag = period,
  k = 0,
  takeMean = TRUE,
  removerows = TRUE,
  overlap = FALSE
)
```

## Arguments

- n:

  Number of variables

- period:

  Number of variables involved in each period

- lag:

  Lag used for difference calculation

- k:

  Shift the start of each period

- takeMean:

  Calculate mean over each period (sum when FALSE)

- removerows:

  Revove incomplete rows

- overlap:

  Overlap between periods (moving averages)

## Value

Linear combination matrix

## Note

It can be useful to add row names to the resulting matrix before further
use.

## Examples

``` r

# We assume two years of four quarters (n=8)

# Quarter to quarter differences
LagDiff(8)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]   -1    1    0    0    0    0    0    0
#> [2,]    0   -1    1    0    0    0    0    0
#> [3,]    0    0   -1    1    0    0    0    0
#> [4,]    0    0    0   -1    1    0    0    0
#> [5,]    0    0    0    0   -1    1    0    0
#> [6,]    0    0    0    0    0   -1    1    0
#> [7,]    0    0    0    0    0    0   -1    1

# Changes from same quarter last year
LagDiff(8,4)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,]   -1    0    0    0    1    0    0    0
#> [2,]    0   -1    0    0    0    1    0    0
#> [3,]    0    0   -1    0    0    0    1    0
#> [4,]    0    0    0   -1    0    0    0    1

# Yearly averages
Period(8,4)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,] 0.25 0.25 0.25 0.25 0.00 0.00 0.00 0.00
#> [2,] 0.00 0.00 0.00 0.00 0.25 0.25 0.25 0.25

# Moving yearly averages
Period(8,4,overlap=TRUE)
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#> [1,] 0.25 0.25 0.25 0.25 0.00 0.00 0.00 0.00
#> [2,] 0.00 0.25 0.25 0.25 0.25 0.00 0.00 0.00
#> [3,] 0.00 0.00 0.25 0.25 0.25 0.25 0.00 0.00
#> [4,] 0.00 0.00 0.00 0.25 0.25 0.25 0.25 0.00
#> [5,] 0.00 0.00 0.00 0.00 0.25 0.25 0.25 0.25

# Difference between yearly averages
PeriodDiff(8,4) # Also try n=16 with overlap=TRUE/FALSE
#>       [,1]  [,2]  [,3]  [,4] [,5] [,6] [,7] [,8]
#> [1,] -0.25 -0.25 -0.25 -0.25 0.25 0.25 0.25 0.25

# Combine two variants and add row names
lc = rbind(LagDiff(8,4),PeriodDiff(8,4))
rownames(lc) = c("diffQ1","diffQ2","diffQ3","diffQ4","diffYearMean")
lc
#>               [,1]  [,2]  [,3]  [,4] [,5] [,6] [,7] [,8]
#> diffQ1       -1.00  0.00  0.00  0.00 1.00 0.00 0.00 0.00
#> diffQ2        0.00 -1.00  0.00  0.00 0.00 1.00 0.00 0.00
#> diffQ3        0.00  0.00 -1.00  0.00 0.00 0.00 1.00 0.00
#> diffQ4        0.00  0.00  0.00 -1.00 0.00 0.00 0.00 1.00
#> diffYearMean -0.25 -0.25 -0.25 -0.25 0.25 0.25 0.25 0.25
```
