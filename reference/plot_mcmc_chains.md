# Trace plots for MCMC output

Trace plots for MCMC output

## Usage

``` r
plot_mcmc_chains(mcmc_out, burnin = 1000)
```

## Arguments

- mcmc_out:

  List returned by
  [`run_mcmc`](https://nickmckay.github.io/AARP/reference/run_mcmc.md).

- burnin:

  Integer. Burn-in iterations to shade (not removed). Default `1000`.

## Value

A ggplot object.
