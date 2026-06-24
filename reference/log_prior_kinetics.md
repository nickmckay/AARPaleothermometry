# Log-prior for kinetic parameters

Weakly informative priors centred on Kaufman (2000) literature values.
Parameters are sampled on transformed scales (`log_Ae`, `log_sigma`) to
enforce positivity without hard truncation.

## Usage

``` r
log_prior_kinetics(log_Ae, Ea, x, log_sigma)
```

## Arguments

- log_Ae:

  Log of the pre-exponential factor. Prior: Normal(42.12, 2).

- Ea:

  Activation energy (kcal/mol). Prior: Normal(31.5, 5).

- x:

  Power-law exponent. Prior: Normal(3, 0.5).

- log_sigma:

  Log of observation noise SD. Prior: Normal(log(0.02), 1).

## Value

Scalar log-prior.
