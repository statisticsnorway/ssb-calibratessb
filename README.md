# R package CalibrateSSB

Weighting and Estimation for Panel Data with Non-Response

```r
library(devtools)                               # Load package containing install_github
install_github("statisticsnorway/CalibrateSSB") # Install CalibrateSSB from GitHub
library(CalibrateSSB)                           # Load CalibrateSSB
?CalibrateSSB                                   # Help documentation of function CalibrateSSB
?PanelEstimation                                # Help documentation of function PanelEstimation
```

 Function        |   |
| ---------------------------- | -------------------------------------------------------------- |
| AkuData | Generate test data |
| CalibrateSSB | Calibration weighting and estimation |
| CalibrateSSBpanel | Calibration weighting and variance estimation for panel data |
| CalSSBobj | Create or modify a CalSSB object |
| CalSSBobjReGenesees | Create a CalSSB object from a ReGenesees/cal.analytic object |
| CrossStrata | Crossing several factor variables |
| LagDiff | Creation of linear combination matrices |
| LinCombMatrix | Creation of linear combination matrices |
| PanelEstimation | Variance estimation for panel data |
| Period | Creation of linear combination matrices |
| PeriodDiff | Creation of linear combination matrices |
| WideFromCalibrate | Rearrange output from CalibrateSSB (calSSB object). Ready for input to PanelEstimation. |

-----------------------------------

#### Description

CalibrateSSB is an R-package that handles repeated surveys with partially overlapping samples. Initially the samples are weighted by linear calibration using known or estimated population totals. A robust model based covariance matrix for all relevant estimated totals is calculated from the residuals according to the calibration model. Alternatively a design based covariance matrix is calculated in a very similar way. A cluster robust version is also possible. In the case of estimated populations totals the covariance matrix is adjusted by utilizing the theory of Särndal and Lundström (2005). Variances of linear combinations (changes and averages) and ratios are calculated from this covariance matrix. The linear combinations and ratios can involve variables within and/or between sample waves. 

#### References

Langsrud, Ø (2016): “A variance estimation R-package for repeated surveys - useful for estimates of changes in quarterly and annual averages”, Romanian Statistical Review nr. 2 / 2016, pp. 17-28. CONFERENCE: New Challenges for Statistical Software - The Use of R in Official Statistics, Bucharest, Romania, 7-8 April. 

Särndal, C.-E. and Lundström, S. (2005): Estimation in Surveys with Nonresponse, John Wiley and Sons, New York.

