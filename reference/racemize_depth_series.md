# Simulate AAR racemization for a depth series of sediment samples

Applies
[`racemize_one_depth`](https://nickmckay.github.io/AARP/reference/racemize_one_depth.md)
over a vector of sample depths and returns the results as a single data
frame. This is the primary entry point for forward-modelling a downcore
D/L profile.

## Usage

``` r
racemize_depth_series(
  depths_cm,
  age_model,
  temp_model,
  sumT_model = NULL,
  winT_model = NULL,
  dT_yr = 10,
  AAR_params = default_AAR_params,
  kappa_m2yr = 1.6
)
```

## Arguments

- depths_cm:

  Numeric vector of sample depths in cm.

- age_model:

  Age-depth model from
  [`make_age_model`](https://nickmckay.github.io/AARP/reference/make_age_model.md)
  or
  [`rate_to_age_model`](https://nickmckay.github.io/AARP/reference/rate_to_age_model.md).

- temp_model:

  Mean annual lake bottom water temperature: scalar or
  `data.frame(age_ka, temp_C)`.

- sumT_model:

  Optional summer half-year temperature (same format as `temp_model`).
  Supply with `winT_model` for seasonal mode.

- winT_model:

  Optional winter half-year temperature.

- dT_yr:

  Timestep in years. Default `10`.

- AAR_params:

  Named list of kinetic parameters. Default
  [`default_AAR_params`](https://nickmckay.github.io/AARP/reference/default_AAR_params.md).

- kappa_m2yr:

  Sediment thermal diffusivity in m\\^2\\/yr. Default `1.6`.

## Value

A data frame with columns `depth_cm`, `deposition_age_ka`, `DL`.

## See also

[`racemize_one_depth`](https://nickmckay.github.io/AARP/reference/racemize_one_depth.md)
for the single-sample function and full documentation of the two
temperature modes.

## Examples

``` r
am     <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 500)
depths <- seq(0, 500, by = 10)
out    <- racemize_depth_series(depths, age_model = am, temp_model = 5)
head(out)
#>   depth_cm deposition_age_ka         DL
#> 1        0               0.0 0.00000000
#> 2       10               0.2 0.04066688
#> 3       20               0.4 0.05123706
#> 4       30               0.6 0.05865179
#> 5       40               0.8 0.06455465
#> 6       50               1.0 0.06953939
```
