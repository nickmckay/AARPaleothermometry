# Apply sediment burial temperature damping

As sediment is buried deeper, the seasonal air temperature signal
attenuates: summer and winter sediment temperatures converge toward the
mean annual. This function models that process as a linear interpolation
from full seasonal amplitude (fresh surface sediment,
`weight1 = startDamp`) to attenuated signal (deeply buried sediment,
`weight1 = endDamp`) over `dampTime` ka. The physical motivation is
thermal diffusion in the sediment column.

## Usage

``` r
dampSedTemps(sumT, winT, age, startDamp = 1, endDamp = 0.5, dampTime = 8)
```

## Arguments

- sumT:

  Numeric vector of summer half-year air temperatures (°C).

- winT:

  Numeric vector of winter half-year air temperatures (°C).

- age:

  Numeric vector of ages (ka BP), same length as `sumT` and `winT`.
  Sorted old-to-young internally.

- startDamp:

  Weight on the same-season temperature at the surface (most recent).
  Default `1` (no attenuation at surface).

- endDamp:

  Weight on the same-season temperature at full burial. Default `0.5`
  (summer and winter sediment temperatures equal at depth).

- dampTime:

  Time scale over which damping is applied (ka). Default `8`.

## Value

A data frame with columns `age`, `sumT`, `winT`, `sumTsed` (dampened
summer sediment temperature), and `winTsed` (dampened winter sediment
temperature).

## See also

[`racemizeSample`](https://nickmckay.github.io/AARP/reference/racemizeSample.md)
which calls this function internally;
[`diffusion_attenuation`](https://nickmckay.github.io/AARP/reference/diffusion_attenuation.md)
for the physically motivated exponential depth-based alternative used in
the depth-series model.

## Examples

``` r
age  <- seq(0, 10, by = 0.01)
sumT <- rep(15, length(age))
winT <- rep(4,  length(age))
d <- dampSedTemps(sumT, winT, age)
head(d)
#>     age sumT winT  sumTsed  winTsed
#> 1 10.00   15    4 15.00000 4.000000
#> 2  9.99   15    4 14.99312 4.006875
#> 3  9.98   15    4 14.98625 4.013750
#> 4  9.97   15    4 14.97938 4.020625
#> 5  9.96   15    4 14.97250 4.027500
#> 6  9.95   15    4 14.96563 4.034375
```
