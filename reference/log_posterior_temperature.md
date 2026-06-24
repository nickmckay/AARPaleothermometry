# Log-posterior for temperature history (Stage 2)

Kinetic parameters are fixed at the values in `kinetics_params`
(typically the Stage 1 posterior means).

## Usage

``` r
log_posterior_temperature(
  params,
  knot_ages_ka,
  kinetics_params,
  downcore_target_data,
  age_model_target
)
```

## Arguments

- params:

  Named numeric vector of temperature knot values and `log_sigma`.
  Names: `T_0ka`, `T_2ka`, ..., `log_sigma`.

- knot_ages_ka:

  Numeric vector of knot ages (ka BP).

- kinetics_params:

  Named vector or list with `log_Ae`, `Ea`, `x`.

- downcore_target_data:

  Data frame from
  [`sim_downcore_data`](https://nickmckay.github.io/AARP/reference/sim_downcore_data.md)
  for the unknown-temperature target core.

- age_model_target:

  Age-depth model for the target core.

## Value

Scalar log-posterior.
