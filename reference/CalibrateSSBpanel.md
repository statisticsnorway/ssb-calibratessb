# Calibration weighting and variance estimation for panel data

Calibration weighting and variance estimation for panel data

## Usage

``` r
CalibrateSSBpanel(...)
```

## Arguments

- ...:

  Input to CalibrateSSB() and PanelEstimation()

## Value

Output from PanelEstimation()

## See also

[`CalibrateSSB`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSB.md),
[`PanelEstimation`](https://statisticsnorway.github.io/ssb-calibratessb/reference/PanelEstimation.md).

## Examples

``` r
z    = AkuData(3000)  # 3000 in each quarter
zPop = AkuData(10000)[,1:7]
lc = rbind(LagDiff(8,4),PeriodDiff(8,4))
rownames(lc) = c("diffQ1","diffQ2","diffQ3","diffQ4","diffYearMean")
CalibrateSSBpanel(grossSample=z,calmodel="~ sex*age", partition=c("year","q"),popData=zPop, 
       y=c("unemployed","workforce"),id="id",wave=c("year","q"),
       numerator="unemployed",linComb=lc)
#> $wTot
#> 2014-1 2014-2 2014-3 2014-4 2015-1 2015-2 2015-3 2015-4 
#>  10000  10000  10000  10000  10000  10000  10000  10000 
#> 
#> $estimates
#> unemployed-2014-1 unemployed-2014-2 unemployed-2014-3 unemployed-2014-4 
#>          173.9406          218.1464          197.3393          154.4800 
#> unemployed-2015-1 unemployed-2015-2 unemployed-2015-3 unemployed-2015-4 
#>          258.1992          171.8048          222.0931          217.4283 
#> 
#> $linCombs
#>       diffQ1       diffQ2       diffQ3       diffQ4 diffYearMean 
#>     84.25857    -46.34153     24.75383     62.94829     31.40479 
#> 
#> $varEstimates
#>   2014-1   2014-2   2014-3   2014-4   2015-1   2015-2   2015-3   2015-4 
#> 485.3383 645.7710 608.5104 513.1668 825.1369 554.6611 706.3391 725.3746 
#> 
#> $varLinCombs
#>       diffQ1       diffQ2       diffQ3       diffQ4 diffYearMean 
#>    1324.7488    1105.0741    1308.0700    1186.2428     366.5035 
#> 
```
