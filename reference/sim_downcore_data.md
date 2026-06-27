# Simulate downcore D/L observations from a known-temperature lake

Generates synthetic downcore D/L values by running the forward model
([`racemize_depth_series`](https://nickmckay.github.io/AARP/reference/racemize_depth_series.md))
and adding Gaussian noise. Used for both the 4°C calibration core
(Stage 1) and the unknown-temperature target core (Stage 2).

## Usage

``` r
sim_downcore_data(
  age_model,
  temp_model,
  sample_ages_ka,
  AAR_params = default_AAR_params,
  sigma = 0.015,
  seed = NULL,
  sumT_model = NULL,
  winT_model = NULL,
  toc_model = NULL,
  f_bac = 0
)
```

## Arguments

- age_model:

  Age-depth model from
  [`make_age_model`](https://nickmckay.github.io/AARP/reference/make_age_model.md)
  or
  [`rate_to_age_model`](https://nickmckay.github.io/AARP/reference/rate_to_age_model.md).

- temp_model:

  Bottom water temperature history: scalar or
  `data.frame(age_ka, temp_C)`.

- sample_ages_ka:

  Numeric vector of sample ages (ka BP). Converted to depths via the age
  model.

- AAR_params:

  True kinetic parameters used to generate the data. Default
  [`default_AAR_params`](https://nickmckay.github.io/AARP/reference/default_AAR_params.md).

- sigma:

  Standard deviation of Gaussian observation noise. Default `0.015`.

- seed:

  Optional integer seed.

- sumT_model:

  Optional summer half-year temperature.

- winT_model:

  Optional winter half-year temperature.

- toc_model:

  Optional TOC interpolation function from
  [`make_toc_model`](https://nickmckay.github.io/AARP/reference/make_toc_model.md)
  for bacterial resetting. Default `NULL`.

- f_bac:

  Bacterial resetting rate constant (yr\\^{-1}\\). Default `0`.

## Value

A data frame with columns: `depth_cm`, `deposition_age_ka`, `age_ka`,
`DL_pred`, `DL_obs`.

## Examples

``` r
am <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 750)
dc <- sim_downcore_data(am, temp_model = 4,
                        sample_ages_ka = seq(0.5, 15, by = 0.5),
                        seed = 1)
head(dc)
#>   depth_cm deposition_age_ka age_ka    DL_pred     DL_obs
#> 1       25               0.5    0.5 0.05153297 0.04213616
#> 2       50               1.0    1.0 0.06492747 0.06768212
#> 3       75               1.5    1.5 0.07432340 0.06178897
#> 4      100               2.0    2.0 0.08180349 0.10573270
#> 5      125               2.5    2.5 0.08812014 0.09306275
#> 6      150               3.0    3.0 0.09364162 0.08133459
```
