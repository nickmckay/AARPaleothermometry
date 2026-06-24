# Log-posterior for kinetic parameters (Stage 1)

Combines likelihoods from heating experiment data and a
known-temperature (4deg C) calibration core with the kinetics prior.

## Usage

``` r
log_posterior_kinetics(
  params,
  heating_data,
  downcore_cal_data,
  age_model_cal,
  temp_cal = 4
)
```

## Arguments

- params:

  Named numeric vector: `log_Ae`, `Ea`, `x`, `log_sigma`.

- heating_data:

  Data frame from
  [`sim_heating_data`](https://nickmckay.github.io/AARP/reference/sim_heating_data.md).

- downcore_cal_data:

  Data frame from
  [`sim_downcore_data`](https://nickmckay.github.io/AARP/reference/sim_downcore_data.md)
  for the 4deg C calibration lake.

- age_model_cal:

  Age-depth model for the calibration core.

- temp_cal:

  Bottom water temperature of the calibration lake (default `4`deg C).

## Value

Scalar log-posterior (possibly `-Inf` for invalid parameters).
