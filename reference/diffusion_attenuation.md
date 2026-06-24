# Thermal diffusion attenuation factor for a periodic temperature signal

Computes the amplitude attenuation of a sinusoidal surface temperature
forcing at burial depth `depth_m`, using the analytical solution to the
1D heat equation for a semi-infinite medium: \$\$T(z,t) =
T\_\mathrm{mean} + A \\ e^{-z/\delta} \sin\\\left(\omega t -
z/\delta\right)\$\$ where \\\delta = \sqrt{\kappa T\_\mathrm{period} /
\pi}\\.

## Usage

``` r
diffusion_attenuation(depth_m, kappa_m2yr, period_yr)
```

## Arguments

- depth_m:

  Burial depth in metres.

- kappa_m2yr:

  Thermal diffusivity of the sediment in m\\^2\\/yr. Typical silty lake
  sediment: ~1.6 m\\^2\\/yr (5e-8 m\\^2\\/s).

- period_yr:

  Period of the temperature forcing in years. Use `1` for the annual
  cycle; use a large value (e.g., `1000`) for Holocene-scale mean annual
  changes (attenuation then approaches 1 at all typical depths).

## Value

Numeric vector of attenuation factors (0 to 1).

## Details

For `kappa_m2yr = 1.6` and `period_yr = 1` (annual cycle), \\\delta
\approx 0.71\\ m. The seasonal amplitude is reduced to 37% at 0.71 m
depth and is effectively zero by ~2-3 m. With a 50 cm/ka sedimentation
rate, the seasonal signal is largely attenuated in samples older than
~4-6 ka.

## Examples

``` r
depths <- seq(0, 3, by = 0.1)
plot(depths,
     diffusion_attenuation(depths, kappa_m2yr = 1.6, period_yr = 1),
     type = "l", xlab = "Depth (m)", ylab = "Attenuation")

```
