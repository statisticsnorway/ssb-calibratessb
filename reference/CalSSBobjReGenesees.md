# Create a CalSSB object from a ReGenesees/cal.analytic object

Create a CalSSB object from a ReGenesees/cal.analytic object

## Usage

``` r
CalSSBobjReGenesees(
  x,
  y,
  samplingWeights = NULL,
  extra = NULL,
  id = NULL,
  wave = NULL
)
```

## Arguments

- x:

  Output from ReGenesees::e.calibrate() (object of class cal.analytic)

- y:

  formula or variable names

- samplingWeights:

  NULL, TRUE (capture from x), formula, variable name or vector of data

- extra:

  NULL, formula, variable names or matrix of data

- id:

  NULL, TRUE (ids from x), formula, variable name or vector of data

- wave:

  NULL, formula, variable name or vector of data

## Value

A CalSSB object. That is, an object of the type retuned by
[`CalibrateSSB`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSB.md).

## See also

[`CalibrateSSB`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSB.md),
[`CalSSBobj`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalSSBobj.md),
[`WideFromCalibrate`](https://statisticsnorway.github.io/ssb-calibratessb/reference/WideFromCalibrate.md),
[`PanelEstimation`](https://statisticsnorway.github.io/ssb-calibratessb/reference/PanelEstimation.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Generates data - two years
z <- AkuData(3000)  # 3000 in each quarter
zPop <- AkuData(10000)[, 1:7]
z$samplingWeights <- 1
z$ids <- 1:NROW(z)

# Create a ReGenesees/cal.analytic object
library("ReGenesees")
desReGenesees <- e.svydesign(z[z$R == 1, ], ids = ~ids, weights = ~samplingWeights)
popTemplate <- pop.template(data = desReGenesees, calmodel = ~sex * age, partition = ~year + q)
popTotals <- fill.template(universe = zPop, template = popTemplate)
calReGenesees <- e.calibrate(design = desReGenesees, df.population = popTotals)

# Create CalSSB objects from a ReGenesees/cal.analytic object
CalSSBobjReGenesees(calReGenesees, y = ~unemployed + workforce, id = TRUE, 
                    samplingWeights = TRUE, extra = ~famid)
a <- CalSSBobjReGenesees(calReGenesees, y = c("unemployed", "workforce"), 
                         id = "id", extra = "famid", wave = c("year", "q"))

# Use the CalSSB object as input ...
PanelEstimation(WideFromCalibrate(a), "unemployed", linComb = PeriodDiff(8, 4))

} # }
```
