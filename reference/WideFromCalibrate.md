# Rearrange output from CalibrateSSB (calSSB object). Ready for input to PanelEstimation.

One row for each id and one column for each wave.

## Usage

``` r
WideFromCalibrate(a, wave = NULL, id = NULL, subSet = NULL, extra = NULL)
```

## Arguments

- a:

  A calSSB object. That is, output from CalibrateSSB() or CalSSBobj().

- wave:

  Time or another repeat variable.

- id:

  Identifier variable.

- subSet:

  Grouping variable for splitting ouput.

- extra:

  Dataset with extra variables not in `a`.

## Value

Output has the same elements (+ extra) as input (a), but rearranged.
When subSet is input otput is alist according to the subSet levels.

## Details

When wave, id or extra is NULL, corresponding elements in the input
object (`a`) will be used if available,

## See also

[`CalibrateSSB`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalibrateSSB.md),
[`CalSSBobj`](https://statisticsnorway.github.io/ssb-calibratessb/reference/CalSSBobj.md),
[`PanelEstimation`](https://statisticsnorway.github.io/ssb-calibratessb/reference/PanelEstimation.md).

## Examples

``` r

# See examples in PanelEstimation and CalSSBobj
```
