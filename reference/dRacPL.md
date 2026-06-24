# Incremental racemization in power-law space

Computes the increment in \\(D/L)^x\\ accumulated over one timestep at
rate constant `kt`. Racemization is integrated in power-law \\(D/L)^x\\
space (the Bada linearization) rather than D/L space, because
\\(D/L)^x\\ increases linearly with \\k_T t\\ and avoids the sigmoidal
curvature that makes D/L accumulation rate-dependent on the current
value.

## Usage

``` r
dRacPL(kt, delta.yr)
```

## Arguments

- kt:

  Numeric. Rate constant from
  [`arrhenius`](https://nickmckay.github.io/AARP/reference/arrhenius.md).

- delta.yr:

  Numeric. Timestep in years.

## Value

Numeric vector of incremental \\(D/L)^x\\ values.

## Examples

``` r
kt <- arrhenius(Temp = 278)
dRacPL(kt, delta.yr = 10)
#> [1] 3.362735e-06
```
