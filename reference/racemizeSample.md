# Simulate racemization for a single downcore sample (with burial damping)

Full pipeline for one sample: selects the portion of the temperature
history from deposition to present, applies
[`dampSedTemps`](https://nickmckay.github.io/AARP/reference/dampSedTemps.md)
to attenuate the seasonal signal with burial depth, then integrates
racemization via
[`racemize`](https://nickmckay.github.io/AARP/reference/racemize.md).

## Usage

``` r
racemizeSample(
  sampleAge,
  ageVec,
  sumT,
  winT,
  x,
  dT,
  startingDL = 0,
  scenario = NA,
  startDamp = 1,
  endDamp = 0.5,
  dampTime = 10,
  ...
)
```

## Arguments

- sampleAge:

  Age of the sample (ka BP). Only temperature history at or older than
  this age is used.

- ageVec:

  Numeric vector of ages spanning the full temperature history (ka BP).

- sumT:

  Numeric vector of summer half-year temperatures (°C), same length as
  `ageVec`.

- winT:

  Numeric vector of winter half-year temperatures (°C), same length as
  `ageVec`.

- x:

  Power-law exponent.

- dT:

  Timestep in years.

- startingDL:

  Starting D/L value. Default `0`.

- scenario:

  Character label for the output. Default `NA`.

- startDamp, endDamp, dampTime:

  Passed to
  [`dampSedTemps`](https://nickmckay.github.io/AARP/reference/dampSedTemps.md).

- ...:

  Additional arguments passed to
  [`arrhenius`](https://nickmckay.github.io/AARP/reference/arrhenius.md).

## Value

A one-row data frame with columns `age`, `DL`, and `scenario`.

## See also

[`racemize`](https://nickmckay.github.io/AARP/reference/racemize.md),
[`dampSedTemps`](https://nickmckay.github.io/AARP/reference/dampSedTemps.md),
[`racemize_depth_series`](https://nickmckay.github.io/AARP/reference/racemize_depth_series.md)
for the more physically rigorous depth-series workflow.

## Examples

``` r
age  <- seq(0, 10, by = 0.01)
sumT <- rep(15, length(age))
winT <- rep(4,  length(age))
racemizeSample(sampleAge = 5, ageVec = age, sumT = sumT, winT = winT,
               x = 3, dT = 10)
#>   age        DL scenario
#> 1   5 0.1780704       NA
```
