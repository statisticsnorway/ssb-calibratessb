# Generate test data

Generate test data of eight quarters

## Usage

``` r
AkuData(n)
```

## Arguments

- n:

  Number of observations within each quarter.

## Value

A data frame with the following variables:

- id:

  Sample unit identifier

- year:

  Year

- q:

  Quarter

- month:

  Month

- R:

  Response indicator

- age:

  Age group

- sex:

  Education group

- famid:

  Family identifier

- unemployed:

  Unemployed

- workforce:

  In workforce

## Examples

``` r

# Generates data - two years
z = AkuData(3000) # 3000 in each quarter
```
