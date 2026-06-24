# Build an age-depth model from tie-point depths and ages

Creates a pair of linear-interpolation functions for converting between
depth (cm) and age (ka BP) in both directions. Both functions use flat
extrapolation (rule = 2) beyond the range of the control points.

## Usage

``` r
make_age_model(depth_cm, age_ka)
```

## Arguments

- depth_cm:

  Numeric vector of tie-point depths in cm. Must start at 0 (the
  sediment surface) and be monotonically increasing.

- age_ka:

  Numeric vector of ages in ka BP corresponding to `depth_cm`. Must
  start at 0 and be the same length as `depth_cm`.

## Value

A named list with two elements:

- depth_to_age:

  A function `f(depth_cm)` that returns age (ka BP).

- age_to_depth:

  A function `f(age_ka)` that returns depth (cm).

## See also

[`rate_to_age_model`](https://nickmckay.github.io/AARP/reference/rate_to_age_model.md)
for the constant-rate convenience wrapper.

## Examples

``` r
am <- make_age_model(depth_cm = c(0, 100, 300, 500),
                     age_ka   = c(0,   2,   6,  10))
am$depth_to_age(250)  # age at 250 cm
#> [1] 5
am$age_to_depth(5)    # depth at 5 ka BP
#> [1] 250
```
