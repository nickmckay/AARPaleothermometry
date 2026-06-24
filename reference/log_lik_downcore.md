# Log-likelihood for downcore D/L data at known temperature

Log-likelihood for downcore D/L data at known temperature

## Usage

``` r
log_lik_downcore(
  log_Ae,
  Ea,
  x,
  log_sigma,
  downcore_data,
  age_model,
  temp_model
)
```

## Arguments

- log_Ae:

  Log of the Arrhenius pre-exponential factor.

- Ea:

  Activation energy (kcal/mol).

- x:

  Power-law exponent.

- log_sigma:

  Log of the observation noise standard deviation.

- downcore_data:

  Data frame from
  [`sim_downcore_data`](https://nickmckay.github.io/AARP/reference/sim_downcore_data.md).

- age_model:

  Age-depth model from
  [`make_age_model`](https://nickmckay.github.io/AARP/reference/make_age_model.md).

- temp_model:

  Bottom water temperature (scalar or data frame).

## Value

Scalar log-likelihood.
