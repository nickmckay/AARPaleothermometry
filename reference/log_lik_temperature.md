# Log-likelihood for downcore D/L data at unknown temperature

Log-likelihood for downcore D/L data at unknown temperature

## Usage

``` r
log_lik_temperature(
  T_knots,
  knot_ages_ka,
  log_sigma,
  kinetics_params,
  downcore_data,
  age_model
)
```

## Arguments

- T_knots:

  Numeric vector of temperatures (deg C) at age control points.

- knot_ages_ka:

  Numeric vector of ages (ka BP) for the control points.

- log_sigma:

  Log of the observation noise standard deviation.

- kinetics_params:

  Named vector or list with elements `log_Ae`, `Ea`, `x`.

- downcore_data:

  Data frame with columns `depth_cm` and `DL_obs`.

- age_model:

  Age-depth model.

## Value

Scalar log-likelihood.
