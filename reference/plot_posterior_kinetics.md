# Posterior density plots for kinetic parameters

Posterior density plots for kinetic parameters

## Usage

``` r
plot_posterior_kinetics(mcmc_out, true_params = NULL, burnin = 1000)
```

## Arguments

- mcmc_out:

  List returned by
  [`run_mcmc`](https://nickmckay.github.io/AARP/reference/run_mcmc.md).

- true_params:

  Named numeric vector of true parameter values (for synthetic data
  validation). `NULL` to omit reference lines.

- burnin:

  Integer. Burn-in iterations to discard before plotting. Default
  `1000`.

## Value

A ggplot object.
