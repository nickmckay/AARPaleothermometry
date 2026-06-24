# Random-walk Metropolis-Hastings MCMC sampler

A simple general-purpose sampler. Each iteration proposes a new
parameter vector by adding independent Gaussian noise to the current
state, then accepts or rejects via the Metropolis criterion.

## Usage

``` r
run_mcmc(log_post_fn, init, n_iter, proposal_sd, ...)
```

## Arguments

- log_post_fn:

  Function taking a named numeric vector and returning a scalar
  log-posterior.

- init:

  Named numeric vector of starting parameter values.

- n_iter:

  Integer. Total iterations (including burn-in).

- proposal_sd:

  Named numeric vector of proposal standard deviations, one per
  parameter. Tune for 20-40% acceptance rate.

- ...:

  Additional arguments passed to `log_post_fn`.

## Value

A list with:

- samples:

  Matrix `[n_iter x n_params]` with column names from `init`.

- acceptance_rate:

  Overall Metropolis acceptance rate.

- log_post:

  Numeric vector of log-posterior values at each iteration.

## Examples

``` r
if (FALSE) { # \dontrun{
# See vignette("kinetics-fitting") for a worked example.
} # }
```
