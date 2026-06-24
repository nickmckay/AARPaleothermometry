# Simulate AAR racemization for a single sediment sample at a given depth

Computes the expected D/L ratio for a sediment sample currently at depth
`depth_cm` by integrating racemization from the moment of deposition to
the present. At each timestep, the sample's burial depth is computed
from the age-depth model, and the sediment temperature is derived from
the lake bottom water temperature with optional thermal diffusion
attenuation of the seasonal signal.

## Usage

``` r
racemize_one_depth(
  depth_cm,
  age_model,
  temp_model,
  sumT_model = NULL,
  winT_model = NULL,
  dT_yr = 10,
  AAR_params = default_AAR_params,
  kappa_m2yr = 1.6
)
```

## Arguments

- depth_cm:

  Current depth of the sample in cm.

- age_model:

  Age-depth model from
  [`make_age_model`](https://nickmckay.github.io/AARP/reference/make_age_model.md)
  or
  [`rate_to_age_model`](https://nickmckay.github.io/AARP/reference/rate_to_age_model.md).

- temp_model:

  Mean annual lake bottom water temperature: scalar (°C) or
  `data.frame(age_ka, temp_C)`.

- sumT_model:

  Optional summer half-year lake bottom temperature (same format as
  `temp_model`). Supply with `winT_model` to activate seasonal mode.

- winT_model:

  Optional winter half-year lake bottom temperature. Supply with
  `sumT_model` to activate seasonal mode.

- dT_yr:

  Timestep in years. Default `10`.

- AAR_params:

  Named list of kinetic parameters. Default
  [`default_AAR_params`](https://nickmckay.github.io/AARP/reference/default_AAR_params.md).

- kappa_m2yr:

  Sediment thermal diffusivity in m\\^2\\/yr. Used only in seasonal mode
  to attenuate the seasonal anomaly with depth. Default `1.6`.

## Value

A one-row data frame with columns `depth_cm`, `deposition_age_ka`, and
`DL`.

## Details

**Mean-annual mode** (default): sediment temperature equals lake bottom
water temperature at all depths. Valid because the e-folding depth for
Holocene-period signals (centuries to millennia) is much larger than
typical core depths.

**Seasonal mode** (`sumT_model` and `winT_model` both supplied): the
seasonal anomaly is attenuated with burial depth via
[`diffusion_attenuation`](https://nickmckay.github.io/AARP/reference/diffusion_attenuation.md)
at `period_yr = 1`. Each timestep is split into equal summer and winter
halves. Because the Arrhenius relationship is nonlinear, identical mean
temperatures with different seasonal amplitudes produce different D/L
values (Jensen's inequality). The seasonal signal is largely gone by
~2-3 m depth.

## See also

[`racemize_depth_series`](https://nickmckay.github.io/AARP/reference/racemize_depth_series.md)
to run over a vector of depths.

## Examples

``` r
am <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 500)
racemize_one_depth(depth_cm = 250, age_model = am, temp_model = 5)
#>   depth_cm deposition_age_ka        DL
#> 1      250                 5 0.1189107

# Seasonal mode
racemize_one_depth(depth_cm = 250, age_model = am,
                   temp_model = 5, sumT_model = 10, winT_model = 0)
#>   depth_cm deposition_age_ka        DL
#> 1      250                 5 0.1218346
```
