# Arrhenius rate constant for amino acid racemization

Computes the temperature-dependent rate constant \\k\\ from the
Arrhenius equation: \$\$\ln(k) = -\frac{E_a}{R} T^{-1} + \ln(A_e)\$\$

## Usage

``` r
arrhenius(Ae = exp(42.12), Ea = 31.5, Temp = 278, R = 0.001987)
```

## Arguments

- Ae:

  Pre-exponential (frequency) factor. Default `exp(42.12)` (Kaufman
  2000).

- Ea:

  Activation energy in kcal/mol. Default `31.5`.

- Temp:

  Temperature in Kelvin. Default `278` (≈ 5°C).

- R:

  Gas constant in kcal K\\^{-1}\\ mol\\^{-1}\\. Default `0.001987`.

## Value

Numeric vector of rate constants \\k\\ at the supplied temperature(s).

## Examples

``` r
# Rate constant at 5°C
arrhenius(Temp = 278)
#> [1] 3.362735e-07

# Rate constant across a temperature range
T_K <- seq(273, 298, by = 1)
plot(T_K - 273, arrhenius(Temp = T_K), type = "l",
     xlab = "Temperature (°C)", ylab = "k")

```
