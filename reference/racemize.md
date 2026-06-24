# Simulate D/L accumulation over a full temperature history

Integrates racemization from the oldest to the youngest point in a
temperature time series, splitting each timestep into summer and winter
halves. Returns the final D/L ratio after back-transforming from
\\(D/L)^x\\ space.

## Usage

``` r
racemize(age, sumT, winT, x, dT, startingDL = 0, scenario = NA, ...)
```

## Arguments

- age:

  Numeric vector of ages (ka BP). Sorted old-to-young internally.

- sumT:

  Numeric vector of summer half-year temperatures (°C), same length as
  `age`.

- winT:

  Numeric vector of winter half-year temperatures (°C), same length as
  `age`.

- x:

  Power-law exponent. Default `3`.

- dT:

  Timestep in years.

- startingDL:

  Starting D/L value. Default `0` (unracemized).

- scenario:

  Character label stored in the output. Default `NA`.

- ...:

  Additional arguments passed to
  [`arrhenius`](https://nickmckay.github.io/AARP/reference/arrhenius.md).

## Value

A one-row data frame with columns `age` (maximum input age), `DL`, and
`scenario`.

## See also

[`racemizeSample`](https://nickmckay.github.io/AARP/reference/racemizeSample.md)
for a version that also applies sediment temperature damping;
[`dampSedTemps`](https://nickmckay.github.io/AARP/reference/dampSedTemps.md)
for the damping model.

## Examples

``` r
age  <- seq(0, 10, by = 0.01)
sumT <- rep(15, length(age))
winT <- rep(4,  length(age))
racemize(age, sumT, winT, x = 3, dT = 10)
#>   age        DL scenario
#> 1  10 0.2383851       NA
```
