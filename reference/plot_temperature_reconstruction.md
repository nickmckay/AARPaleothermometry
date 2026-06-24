# Temperature history reconstruction plot

Posterior median and 95%/50% credible intervals for the reconstructed
temperature history, optionally overlaid with the true synthetic
history.

## Usage

``` r
plot_temperature_reconstruction(
  mcmc_out,
  knot_ages_ka,
  true_T_history = NULL,
  burnin = 1000
)
```

## Arguments

- mcmc_out:

  List returned by
  [`run_mcmc`](https://nickmckay.github.io/AARP/reference/run_mcmc.md)
  from Stage 2.

- knot_ages_ka:

  Numeric vector of knot ages (ka BP).

- true_T_history:

  Optional data frame with columns `age_ka` and `temp_C` for the true
  synthetic temperature history.

- burnin:

  Integer. Burn-in iterations to discard. Default `1000`.

## Value

A ggplot object.
