# Posterior predictive D/L depth profiles

Draws parameter sets from the posterior and runs the forward model to
produce an envelope of predicted D/L vs. depth, optionally overlaid with
observed data.

## Usage

``` r
posterior_predictive_DL(
  mcmc_out,
  age_model,
  depths_cm,
  temp_model,
  AAR_params_fn = function(row) {
     list(Ae = exp(row[["log_Ae"]]), Ea = row[["Ea"]],
    R = 0.001987, x = row[["x"]])
 },
  observed_data = NULL,
  burnin = 1000,
  n_draws = 200
)
```

## Arguments

- mcmc_out:

  List returned by
  [`run_mcmc`](https://nickmckay.github.io/AARP/reference/run_mcmc.md).

- age_model:

  Age-depth model.

- depths_cm:

  Numeric vector of depths at which to evaluate predictions.

- temp_model:

  Temperature model for predictions.

- AAR_params_fn:

  Function taking one row of the posterior sample matrix and returning
  an `AAR_params` list. Defaults to extracting `log_Ae`, `Ea`, `x` from
  the Stage 1 posterior.

- observed_data:

  Optional data frame with columns `depth_cm` and `DL_obs` to overlay as
  points.

- burnin:

  Integer. Burn-in iterations to discard. Default `1000`.

- n_draws:

  Integer. Number of posterior draws. Default `200`.

## Value

A ggplot object.
