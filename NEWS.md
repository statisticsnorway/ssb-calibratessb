## CalibrateSSB 1.4.0

* New parameters to `CalibrateSSB()`: `ginvtol`, `boundstol` and `resids_by_lm`
  - Control of robustness and precision in regression computations.
* Support for tibble and data.table input to `CalibrateSSB()` (`grossSample`, `popData` and `popTotals`)
  - Internal coercion to data.frame using `as.data.frame()` where necessary.
* Bugfix: `samplingWeightsOutput = TRUE` in `CalibrateSSB()` now works correctly
  - Correct output of sampling-weight data instead of the `samplingWeights` variable name.
  - Avoidance of downstream errors in `WideFromCalibrate()` and `CalibrateSSBpanel()`.


## CalibrateSSB	1.3.0

* The official CRAN version allows ReGenesees 

  
## CalibrateSSB	1.2

* Last version before any news