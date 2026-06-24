# Log-prior for temperature history control points

Weakly informative Normal(5, 3) prior on each knot temperature, plus a
first-difference smoothness penalty that discourages large jumps between
adjacent control points.

## Usage

``` r
log_prior_temperature(T_knots, log_sigma)
```

## Arguments

- T_knots:

  Numeric vector of temperatures at age control points (deg C).

- log_sigma:

  Log of observation noise SD. Prior: Normal(log(0.02), 1).

## Value

Scalar log-prior.
