# Simulate lab heating experiment D/L data

Generates synthetic D/L observations for sediment heated at constant
elevated temperatures, following the controlled-temperature experiment
design in NSF proposal 2317409 (80, 100, 110°C; 8 time steps; 3
replicates per condition, ~72 total observations).

## Usage

``` r
sim_heating_data(
  temps_C = c(80, 100, 110),
  times_yr = c(1/8760, 1/365, 7/365, 30/365, 90/365, 180/365, 270/365, 1),
  n_reps = 3,
  AAR_params = default_AAR_params,
  sigma = 0.01,
  seed = NULL
)
```

## Arguments

- temps_C:

  Numeric vector of heating temperatures (°C). Default
  `c(80, 100, 110)`.

- times_yr:

  Numeric vector of heating durations (years). Default covers ~1 hour to
  1 year:
  `c(1/8760, 1/365, 7/365, 30/365, 90/365, 180/365, 270/365, 1)`.

- n_reps:

  Integer. Replicates per temperature-time combination. Default `3`.

- AAR_params:

  Named list of true kinetic parameters used to generate the data.
  Default
  [`default_AAR_params`](https://nickmckay.github.io/AARP/reference/default_AAR_params.md).

- sigma:

  Standard deviation of Gaussian observation noise added to predicted
  D/L. Default `0.01`.

- seed:

  Optional integer seed for reproducibility.

## Value

A data frame with columns: `temp_C`, `time_yr`, `replicate`, `DL_pred`,
`DL_obs`.

## Examples

``` r
hd <- sim_heating_data(seed = 42)
head(hd)
#>   temp_C      time_yr replicate    DL_pred     DL_obs
#> 1     80 0.0001141553         1 0.01914318 0.03285276
#> 2    100 0.0001141553         1 0.04271790 0.03707092
#> 3    110 0.0001141553         1 0.06183797 0.06546926
#> 4     80 0.0027397260         1 0.05521848 0.06154711
#> 5    100 0.0027397260         1 0.12321974 0.12726243
#> 6    110 0.0027397260         1 0.17837158 0.17731033
```
