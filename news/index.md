# Changelog

## CalibrateSSB 1.4.0

- New pkgdown website for the package
  - This package now has a documentation site at
    <https://statisticsnorway.github.io/ssb-calibratessb/>.
- New parameters to
  [`CalibrateSSB()`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSB.md):
  `ginvtol`, `boundstol` and `resids_by_lm`
  - Control of robustness and precision in regression computations.
- Support for tibble and data.table input to
  [`CalibrateSSB()`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSB.md)
  (`grossSample`, `popData` and `popTotals`)
  - Internal coercion to data.frame using
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) where
    necessary.
- Bugfix: `samplingWeightsOutput = TRUE` in
  [`CalibrateSSB()`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSB.md)
  now works correctly
  - Correct output of sampling-weight data instead of the
    `samplingWeights` variable name.
  - Avoidance of downstream errors in
    [`WideFromCalibrate()`](https://statisticsnorway.github.io/ssb-calibratessb/reference/WideFromCalibrate.md)
    and
    [`CalibrateSSBpanel()`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSBpanel.md).

## CalibrateSSB 1.3.0

CRAN release: 2020-08-04

- The official CRAN version allows ReGenesees

## CalibrateSSB 1.2

CRAN release: 2019-12-06

- Last version before any news
