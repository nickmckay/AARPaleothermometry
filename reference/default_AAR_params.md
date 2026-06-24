# Default AAR kinetic parameters

A named list of Arrhenius and power-law constants used as defaults
throughout the package. Values are from Kaufman (2000) for isoleucine
epimerization and have not yet been calibrated to site-specific data.

## Usage

``` r
default_AAR_params
```

## Format

A named list with four elements:

- Ae:

  Pre-exponential (frequency) factor: `exp(42.12)`.

- Ea:

  Activation energy in kcal/mol: `31.5`.

- R:

  Gas constant in kcal K\\^{-1}\\ mol\\^{-1}\\: `0.001987`.

- x:

  Power-law exponent for D/L linearization: `3`.
