# Create or modify a CalSSB object

The elements of the CalSSB object are taken directly from the input
parameters.

## Usage

``` r
CalSSBobj(
  x = NULL,
  y = NULL,
  w = NULL,
  wGross = NULL,
  resids = NULL,
  resids2 = NULL,
  leverages = NULL,
  leverages2 = NULL,
  samplingWeights = NULL,
  extra = NULL,
  id = NULL,
  wave = NULL
)
```

## Arguments

- x:

  NULL or an existing calSSB object

- y:

  y

- w:

  w

- wGross:

  wGross

- resids:

  resids

- resids2:

  resids2

- leverages:

  leverages

- leverages2:

  leverages2

- samplingWeights:

  samplingWeights

- extra:

  extra

- id:

  id

- wave:

  wave

## Value

A CalSSB object. That is, an object of the type retuned by
[`CalibrateSSB`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSB.md).

## Note

If x is a ReGenesees/cal.analytic object, this function is a wrapper to
[`CalSSBobjReGenesees`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalSSBobjReGenesees.md).

## See also

[`CalibrateSSB`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSB.md),
[`CalSSBobjReGenesees`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalSSBobjReGenesees.md),
[`WideFromCalibrate`](https://statisticsnorway.github.io/ssb-calibratessb/reference/WideFromCalibrate.md),
[`PanelEstimation`](https://statisticsnorway.github.io/ssb-calibratessb/reference/PanelEstimation.md).

## Examples

``` r
#' # Generates data - two years
z <- AkuData(3000)  # 3000 in each quarter
zPop <- AkuData(10000)[, 1:7]

# Create a CalSSB object by CalibrateSSB
b <- CalibrateSSB(z, calmodel = "~ sex*age", partition = c("year", "q"), popData = zPop, 
                  y = c("unemployed", "workforce"))

# Modify the CalSSB object
a <- CalSSBobj(b, w = 10*b$w, wave = CrossStrata(z[, c("year", "q")]), id = z$id)

# Use the CalSSB object as input ...
PanelEstimation(WideFromCalibrate(a), "unemployed", linComb = PeriodDiff(8, 4))
#> $wTot
#> 2014-1 2014-2 2014-3 2014-4 2015-1 2015-2 2015-3 2015-4 
#>  1e+05  1e+05  1e+05  1e+05  1e+05  1e+05  1e+05  1e+05 
#> 
#> $estimates
#> unemployed-2014-1 unemployed-2014-2 unemployed-2014-3 unemployed-2014-4 
#>          2034.624          2158.891          1595.673          2066.855 
#> unemployed-2015-1 unemployed-2015-2 unemployed-2015-3 unemployed-2015-4 
#>          1696.414          2191.972          1900.522          1853.707 
#> 
#> $linCombs
#> [1] -53.35698
#> 
#> $varEstimates
#>   2014-1   2014-2   2014-3   2014-4   2015-1   2015-2   2015-3   2015-4 
#> 73910.73 79509.11 61042.94 81125.75 67323.78 87301.27 78545.71 78167.28 
#> 
#> $varLinCombs
#> [1] 46304.5
#> 

# Create CalSSB object without x as input
CalSSBobj(y = b$y, w = 10*b$w, resids = b$resids, wave = CrossStrata(z[, c("year", "q")]), 
          id = z$id)
#> ---- n = 24000     ny = 2 
#> 
#> ---- y names ----
#>  unemployed workforce 
#> 
#> ---- Calibrated weights, w ----
#>  38.48901 38.48901 38.18021 38.18021 38.18021 38.58311 ... 44.0604 
#> 
#> ---- Summary of calSSB object ----
#>        Length Class      Mode   
#> y          2  data.frame list   
#> w      24000  -none-     numeric
#> resids     2  data.frame list   
#> id     24000  -none-     numeric
#> wave   24000  factor     numeric
```
