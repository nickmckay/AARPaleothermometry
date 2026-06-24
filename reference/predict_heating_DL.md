# Predict D/L for lab heating experiments

Forward model for constant-temperature, constant-time heating
experiments. Assumes starting D/L = 0 (surface sediment with negligible
natural racemization). Reuses
[`arrhenius`](https://nickmckay.github.io/AARP/reference/arrhenius.md)
and [`dRacPL`](https://nickmckay.github.io/AARP/reference/dRacPL.md).

## Usage

``` r
predict_heating_DL(temp_C, time_yr, AAR_params)
```

## Arguments

- temp_C:

  Numeric vector of heating temperatures in °C.

- time_yr:

  Numeric vector of heating durations in years.

- AAR_params:

  Named list with elements `Ae`, `Ea`, `R`, `x` (see
  [`default_AAR_params`](https://nickmckay.github.io/AARP/reference/default_AAR_params.md)).

## Value

Numeric vector of predicted D/L values.

## Examples

``` r
predict_heating_DL(temp_C = c(80, 100, 110), time_yr = 1,
                   AAR_params = default_AAR_params)
#> [1] 0.3946227 0.8805985 1.2747449
```
