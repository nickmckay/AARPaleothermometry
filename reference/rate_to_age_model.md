# Build an age-depth model from a constant sedimentation rate

Convenience wrapper around
[`make_age_model`](https://nickmckay.github.io/AARP/reference/make_age_model.md)
for the common case of a constant sedimentation rate. Constructs a
two-point age-depth model anchored at the surface (0 cm, 0 ka) and at
`max_depth_cm`.

## Usage

``` r
rate_to_age_model(rate_cm_per_ka, max_depth_cm)
```

## Arguments

- rate_cm_per_ka:

  Sedimentation rate in cm/ka.

- max_depth_cm:

  Maximum depth (cm) of the core.

## Value

A named list with `depth_to_age` and `age_to_depth` functions (see
[`make_age_model`](https://nickmckay.github.io/AARP/reference/make_age_model.md)).

## Examples

``` r
am <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 500)
am$depth_to_age(250)  # should return 5 ka
#> [1] 5
```
