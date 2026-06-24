# Log-likelihood for lab heating experiment data

Log-likelihood for lab heating experiment data

## Usage

``` r
log_lik_heating(log_Ae, Ea, x, log_sigma, heating_data)
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

- heating_data:

  Data frame from
  [`sim_heating_data`](https://nickmckay.github.io/AARP/reference/sim_heating_data.md).

## Value

Scalar log-likelihood, or `-Inf` for invalid parameters.
