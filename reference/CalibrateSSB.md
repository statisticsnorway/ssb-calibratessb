# Calibration weighting and estimation

Compute weights by calibration and corresponding estimates, totals and
residuals

## Usage

``` r
CalibrateSSB(
  grossSample,
  calmodel = NULL,
  response = "R",
  popTotals = NULL,
  y = NULL,
  by = NULL,
  partition = NULL,
  lRegmodel = NULL,
  popData = NULL,
  samplingWeights = NULL,
  usePackage = "survey",
  bounds = c(-Inf, Inf),
  calfun = "linear",
  onlyTotals = FALSE,
  onlyw = FALSE,
  uselRegWeights = FALSE,
  ids = NULL,
  residOutput = TRUE,
  leverageOutput = FALSE,
  yOutput = TRUE,
  samplingWeightsOutput = FALSE,
  dropResid2 = TRUE,
  wGrossOutput = TRUE,
  wave = NULL,
  id = NULL,
  extra = NULL,
  allowNApopTotals = NULL,
  partitionPrint = NULL,
  ...
)
```

## Arguments

- grossSample:

  Data frame.

- calmodel:

  Formula defining the linear structure of the calibration model.

- response:

  Variable name of response indicator (net sample when 1).

- popTotals:

  Population totals (similar to population totals as output).

- y:

  Names of variables of interest. Can be a list similar to "by" below.

- by:

  Names of the variables that define the "estimation domains". If NULL
  (the default option) or NA estimates refer to the whole population.
  Use list for multiple specifications (resulting in list as output).

- partition:

  Names of the variables that define the "calibration domains" for the
  model. NULL (the default) implies no calibration domains.

- lRegmodel:

  Formula defining the linear structure of a logistic regression model.

- popData:

  Data frame of population data.

- samplingWeights:

  Name of the variable with initial weights for the sampling units.

- usePackage:

  Specifying the package to be used: "survey" (the default),
  "ReGenesees" or "none".

- bounds:

  Bounds for the calibration weights. When ReGenesees: Allowed range for
  the ratios between calibrated and initial weights. The default is
  c(-Inf,Inf).

- calfun:

  The distance function for the calibration process; the default is
  'linear'.

- onlyTotals:

  When TRUE: Only population totals are returned.

- onlyw:

  When TRUE: Only the calibrated weights are returned.

- uselRegWeights:

  When TRUE: Weighted logistic regression is performed as a first
  calibration step.

- ids:

  Name of sampling unit identifier variable.

- residOutput:

  Residuals in output when TRUE. FALSE is default.

- leverageOutput:

  Leverages in output when TRUE. FALSE is default.

- yOutput:

  y in output when TRUE. FALSE is default.

- samplingWeightsOutput:

  samplingWeights in output when TRUE. FALSE is default.

- dropResid2:

  When TRUE (default) and when no missing population totals - only one
  set of residuals in output.

- wGrossOutput:

  wGross in output when TRUE (default) and when NA popTotals.

- wave:

  Time or another repeat variable (to be included in output).

- id:

  Identifier variable (to be included in output).

- extra:

  Variables for the extra dataset (to be included in output).

- allowNApopTotals:

  When TRUE missing population totals are allowed. Results in error when
  FALSE and warning when NULL.

- partitionPrint:

  When TRUE partition progress is printed. Automatic decision when NULL
  (about 1 min total computing time).

- ...:

  Further arguments sent to underlying functions.

## Value

Unless onlyTotals or onlyw is TRUE, the output is an object of class
calSSB. That is, a list with elements:

- popTotals:

  Population totals.

- w:

  The calibrated weights.

- wGross:

  Calibrated gross sample weights when NA popTotals.

- estTM:

  Estimates (with standard error).

- resids:

  Residuals, reduced model when NA popTotals.

- resids2:

  Residuals, full model.

- leverages:

  Diagonal elements of hat-matrix, reduced model when NA popTotals.

- leverages2:

  Diagonal elements of hat-matrix, full model.

- y:

  as input

- samplingWeights:

  as input

- wave:

  as input or via CrossStrata

- id:

  as input

- extra:

  as input

## Details

When popTotals as input is NULL, population totals are computed from
popData (when available) or from grossSample. Some elements of popTotals
may be missing (not allowed when using ReGenesees). When using
"ReGenesees", both weiging and estimation are done by that package. When
using "survey", only calibration weiging are done by that package. The
parameters `wave`, `id` and `extra` have no effect on the computations,
but result in extra elements in output (to be used by
WideFromCalibrate() later).

## See also

[`CalSSBobj`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalSSBobj.md),
[`WideFromCalibrate`](https://statisticsnorway.github.io/ssb-calibratessb/reference/WideFromCalibrate.md),
[`PanelEstimation`](https://statisticsnorway.github.io/ssb-calibratessb/reference/PanelEstimation.md),
[`CalibrateSSBpanel`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSBpanel.md).

## Examples

``` r

# Generates data  - two years
z    <- AkuData(3000)  # 3000 in each quarter
zPop <- AkuData(10000)[,1:7]

# Calibration using "survey"
a <- CalibrateSSB(z, calmodel = "~ sex*age",
                 partition = c("year","q"),  # calibrate within quarter
                 popData = zPop, y = c("unemployed","workforce"),
                 by = c("year","q")) # Estimate within quarter
head(a$w) # calibrated weights
#> [1] 4.202268 3.387097 3.387097 0.000000 3.387097 4.029891
a$estTM   # estimates
#>   year q unemployed workforce
#> 1 2014 1   255.1660  7268.272
#> 3 2014 2   271.8313  7333.530
#> 5 2014 3   225.0274  7325.043
#> 7 2014 4   177.0589  7346.346
#> 2 2015 1   199.6609  7296.701
#> 4 2015 2   166.6121  7281.029
#> 6 2015 3   185.7072  7343.383
#> 8 2015 4   167.9851  7288.499
a$popTotals   # popTotals used as input below
#>   year q (Intercept) sex1 age2 age3 sex1:age2 sex1:age3
#> 1 2014 1       10000 5059 4424 2718      2223      1353
#> 2 2014 2       10000 5046 4424 2614      2226      1299
#> 3 2014 3       10000 5057 4376 2622      2214      1306
#> 4 2014 4       10000 5082 4394 2557      2257      1277
#> 5 2015 1       10000 5088 4338 2684      2248      1358
#> 6 2015 2       10000 5093 4264 2724      2201      1380
#> 7 2015 3       10000 5081 4285 2638      2210      1318
#> 8 2015 4       10000 5072 4204 2629      2147      1338


# Calibration, no package, popTotals as input
b <- CalibrateSSB(z, popTotals=a$popTotals, calmodel="~ sex*age",
      partition = c("year","q"), usePackage = "none", y = c("unemployed","workforce"))
max(abs(a$w-b$w)) # Same weights as above
#> [1] 2.096101e-13

print(a)
#> ---- estTM ----
#>   year q unemployed workforce
#> 1 2014 1   255.1660  7268.272
#> 3 2014 2   271.8313  7333.530
#> 5 2014 3   225.0274  7325.043
#> 7 2014 4   177.0589  7346.346
#> 2 2015 1   199.6609  7296.701
#> 4 2015 2   166.6121  7281.029
#> 6 2015 3   185.7072  7343.383
#> 8 2015 4   167.9851  7288.499
#> 
#> 
#> ---- n = 24000     ny = 2 
#> 
#> ---- y names ----
#>  unemployed workforce 
#> 
#> ---- Calibrated weights, w ----
#>  4.202268 3.387097 3.387097 0 3.387097 4.029891 ... 4.54023 
#> 
#> ---- Summary of calSSB object ----
#>           Length Class      Mode   
#> popTotals     8  data.frame list   
#> w         24000  -none-     numeric
#> estTM         4  data.frame list   
#> resids        2  data.frame list   
#> y             2  data.frame list   
print(b)
#> ---- estTM ----
#>   unemployed workforce
#> 1   1649.049   58482.8
#> 
#> 
#> ---- n = 24000     ny = 2 
#> 
#> ---- y names ----
#>  unemployed workforce 
#> 
#> ---- Calibrated weights, w ----
#>  4.202268 3.387097 3.387097 0 3.387097 4.029891 ... 4.54023 
#> 
#> ---- Summary of calSSB object ----
#>           Length Class      Mode   
#> popTotals     8  data.frame list   
#> w         24000  -none-     numeric
#> estTM         2  data.frame list   
#> resids        2  data.frame list   
#> y             2  data.frame list   

if (FALSE) { # \dontrun{
require(ReGenesees)
# Calibration and estimation via ReGenesees
CalibrateSSB(z, calmodel = "~ sex*age",
             partition = c("year","q"),  # calibrate within quarter
             popData = zPop, usePackage = "ReGenesees",
             y = c("unemployed","workforce"),
             by = c("year","q")) # Estimate within quarter
} # }
```
