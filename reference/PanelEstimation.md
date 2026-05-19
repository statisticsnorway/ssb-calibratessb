# Variance estimation for panel data

Variance estimation of linear combinations of totals and ratios based on
output from wideFromCalibrate

## Usage

``` r
PanelEstimation(
  x,
  numerator,
  denominator = NULL,
  linComb = matrix(0, 0, n),
  linComb0 = NULL,
  estType = "robustModel",
  leveragePower = 1/2,
  group = NULL,
  returnCov = FALSE,
  usewGross = TRUE
)
```

## Arguments

- x:

  Output from wideFromCalibrate.

- numerator:

  y variable name or number.

- denominator:

  y variable name or number.

- linComb:

  Matrix defining linear combinations of waves.

- linComb0:

  Linear combination matrix to be used prior to ratio calculations.

- estType:

  Estimation type: "robustModel" (default), "ssbAKU", "robustModelww",
  "robustModelGroup" or "robustModelGroupww" (see below)

- leveragePower:

  Power used when adjusting residuals using leverages.

- group:

  Extra variable name or number for cluster robust estimation.

- returnCov:

  Return covariance matrices instead of variance vectors.

- usewGross:

  Use wGross (if avaliable) instead of design weights to adjust
  covariance matrix in the case of NA popTotals

## Value

- wTot:

  Sum of weights

- estimates:

  Ordinary estimates

- linCombs:

  Estimates of linear combinations

- varEstimates:

  Variance of estimates

- varLinCombs:

  Variance of estimates of linear combinations

When denominator is specified the above output refer to ratios. Then,
similar output for numerator and denominator are also included.

## Details

When denominator=NULL, only estimates for a single y-variable
(numerator) are calculated. When denominator is specified, estimates for
numerator, denominator and ratio are calculated. The default estimation
type parameter, "robustModel", is equation (12) in paper. "ssbAKU" is
(16), "robustModelww" is (9) and "robustModelGroup" and
"robustModelGroupww" are cluster robust variants based on \\(w-1)^2\\
and \\w^2\\ .

## See also

[`CalibrateSSB`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSB.md),
[`CalSSBobj`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalSSBobj.md),
[`WideFromCalibrate`](https://statisticsnorway.github.io/ssb-calibratessb/reference/WideFromCalibrate.md),
[`CalibrateSSBpanel`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSBpanel.md).

## Examples

``` r

# Generates data  - two years
z    = AkuData(3000)  # 3000 in each quarter
zPop = AkuData(10000)[,1:7]

# Calibration and "WideFromCalibrate"
b = CalibrateSSB(z,calmodel="~ sex*age", partition=c("year","q"),
        popData=zPop, y=c("unemployed","workforce"))
bWide = WideFromCalibrate(b,CrossStrata(z[,c("year","q")]),z$id)

# Define linear combination matrix
lc = rbind(LagDiff(8,4),PeriodDiff(8,4))
rownames(lc) = c("diffQ1","diffQ2","diffQ3","diffQ4","diffYearMean")
colnames(lc) = colnames(head(bWide$y[[1]]))
lc
#>              2014-1 2014-2 2014-3 2014-4 2015-1 2015-2 2015-3 2015-4
#> diffQ1        -1.00   0.00   0.00   0.00   1.00   0.00   0.00   0.00
#> diffQ2         0.00  -1.00   0.00   0.00   0.00   1.00   0.00   0.00
#> diffQ3         0.00   0.00  -1.00   0.00   0.00   0.00   1.00   0.00
#> diffQ4         0.00   0.00   0.00  -1.00   0.00   0.00   0.00   1.00
#> diffYearMean  -0.25  -0.25  -0.25  -0.25   0.25   0.25   0.25   0.25

# Unemployed: Totals and linear combinations
d1=PanelEstimation(bWide,"unemployed",linComb=lc)  #

# Table of output
cbind(tot=d1$estimates,se=sqrt(d1$varEstimates))
#>                        tot       se
#> unemployed-2014-1 203.2301 23.37956
#> unemployed-2014-2 207.3415 24.22232
#> unemployed-2014-3 136.7243 20.14655
#> unemployed-2014-4 171.9668 23.18518
#> unemployed-2015-1 230.9932 27.03446
#> unemployed-2015-2 187.6141 24.00314
#> unemployed-2015-3 162.6746 22.06865
#> unemployed-2015-4 217.8148 26.50928
cbind(tot=d1$linCombs,se=sqrt(d1$varLinCombs))
#>                    tot       se
#> diffQ1        27.76308 33.62251
#> diffQ2       -19.72731 33.93508
#> diffQ3        25.95028 27.83716
#> diffQ4        45.84805 34.17877
#> diffYearMean  19.95852 18.45563

# Ratio: Totals and linear combinations
d=PanelEstimation(bWide,numerator="unemployed",denominator="workforce",linComb=lc)
cbind(tot=d$estimates,se=sqrt(d$varEstimates))
#>               tot          se
#> 2014-1 0.02799162 0.003206968
#> 2014-2 0.02849977 0.003314161
#> 2014-3 0.01901122 0.002794352
#> 2014-4 0.02441536 0.003281478
#> 2015-1 0.03174747 0.003700882
#> 2015-2 0.02538527 0.003237273
#> 2015-3 0.02242822 0.003032731
#> 2015-4 0.02994036 0.003629313
cbind(tot=d$linCombs,se=sqrt(d$varLinCombs))
#>                       tot          se
#> diffQ1        0.003755844 0.004592525
#> diffQ2       -0.003114504 0.004596087
#> diffQ3        0.003416997 0.003836811
#> diffQ4        0.005524996 0.004727154
#> diffYearMean  0.002395833 0.002546163

if (FALSE) { # \dontrun{
# Calibration when som population totals unknown (edu)
# Leverages in output (will be used to adjust residuals)
# Cluster robust estimation (families/famid)
b2 = CalibrateSSB(z,popData=zPop,calmodel="~ edu*sex + sex*age",
           partition=c("year","q"), y=c("unemployed","workforce"),
           leverageOutput=TRUE)
b2Wide = WideFromCalibrate(b2,CrossStrata(z[,c("year","q")]),z$id,extra=z$famid)
d2 = PanelEstimation(b2Wide,"unemployed",linComb=lc,group=1,estType = "robustModelGroup")
cbind(tot=d2$linCombs,se=sqrt(d2$varLinCombs))
} # }


# Yearly mean before ratio calculation (linComb0)
# and difference between years (linComb)
g=PanelEstimation(bWide,numerator="unemployed",denominator="workforce",
    linComb= LagDiff(2),linComb0=Period(8,4))
cbind(tot=g$linCombs,se=sqrt(g$varLinCombs))
#>              tot          se
#> [1,] 0.002371494 0.002548562
```
