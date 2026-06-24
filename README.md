# AARP <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/nickmckay/AARP/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nickmckay/AARP/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/nickmckay/AARP/actions/workflows/pkgdown.yaml/badge.svg)](https://nickmckay.github.io/AARP)
<!-- badges: end -->

**Amino Acid Racemization Paleothermometry** — a forward model and Bayesian inversion framework for inferring Holocene paleotemperatures from D/L ratios of isoleucine in lake sediment.

Amino acid racemization (AAR) is temperature-sensitive: the rate at which the L-enantiomer converts to the D-form depends on both time and temperature via Arrhenius kinetics. In a sediment core whose age–depth relationship is independently constrained (e.g. by radiocarbon), the downcore D/L profile encodes the integrated thermal history of the lake bottom. AARP provides the machinery to decode it.

## Installation

```r
# install.packages("pak")
pak::pkg_install("nickmckay/AARP")
```

## Two-stage workflow

Simultaneous inference of kinetics and temperature from a single core is under-determined. AARP separates the problem into two stages:

```
Stage 1 — Kinetics calibration
  ├── Lab heating experiments (80/100/110 °C, controlled time)
  └── Downcore D/L from a 4 °C lake (known temperature)
          ↓
  Posterior on (log Ae, Ea, x, log σ)

Stage 2 — Temperature reconstruction
  ├── Fixed kinetics from Stage 1
  └── Downcore D/L from target lake (unknown temperature)
          ↓
  Posterior on temperature history T(t) at 6 control knots
```

Stage 1 uses heating experiments to constrain the Arrhenius relationship and a known-temperature (4 °C hypolimnion) calibration core to pin the power-law exponent. Stage 2 fixes kinetics at the Stage 1 posterior median and inverts D/L from a new core for temperature.

## Quick start

### Forward model

```r
library(AARP)

# Build a constant-rate age–depth model (50 cm/ka, 750 cm core)
am <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 750)

# Simulate a downcore D/L profile under a linear 5→8 °C Holocene cooling
T_history <- data.frame(age_ka = c(0, 10), temp_C = c(5, 8))
dl <- racemize_depth_series(seq(0, 750, by = 25), am, T_history,
                            AAR_params = default_AAR_params)
head(dl)
```

### Stage 1 — Kinetics MCMC

```r
set.seed(1)
heating_data <- sim_heating_data(sigma = 0.01, seed = 1)
cal_data     <- sim_downcore_data(am, temp_model = 4,
                                  sample_ages_ka = seq(0.5, 15, by = 0.5),
                                  sigma = 0.015, seed = 2)

mcmc_kin <- run_mcmc(
  log_posterior_kinetics,
  init        = c(log_Ae = 42.12, Ea = 31.5, x = 3.0, log_sigma = log(0.01)),
  n_iter      = 10000,
  proposal_sd = c(log_Ae = 0.005, Ea = 0.05, x = 0.003, log_sigma = 0.008),
  heating_data      = heating_data,
  downcore_cal_data = cal_data,
  age_model_cal     = am,
  temp_cal          = 4
)

plot_mcmc_chains(mcmc_kin, burnin = 1000)
plot_posterior_kinetics(mcmc_kin, burnin = 1000)
```

### Stage 2 — Temperature reconstruction

```r
kin_fixed <- apply(mcmc_kin$samples[1001:10000, ], 2, median)

target_data <- sim_downcore_data(am, temp_model = T_history,
                                 sample_ages_ka = seq(0.5, 10, by = 0.5),
                                 sigma = 0.015, seed = 3)

knot_ages <- c(0, 2, 4, 6, 8, 10)

mcmc_T <- run_mcmc(
  log_posterior_temperature,
  init        = c(T_0ka = 6, T_2ka = 6, T_4ka = 6,
                  T_6ka = 6, T_8ka = 6, T_10ka = 6, log_sigma = log(0.015)),
  n_iter      = 10000,
  proposal_sd = c(T_0ka = 0.3, T_2ka = 0.3, T_4ka = 0.3,
                  T_6ka = 0.3, T_8ka = 0.3, T_10ka = 0.3, log_sigma = 0.1),
  knot_ages_ka         = knot_ages,
  kinetics_params      = kin_fixed,
  downcore_target_data = target_data,
  age_model_target     = am
)

plot_temperature_reconstruction(mcmc_T, knot_ages_ka = knot_ages, burnin = 1000)
```

## Vignettes

Full worked examples with synthetic data and diagnostic plots:

| Vignette | Description |
|----------|-------------|
| [Forward model basics](https://nickmckay.github.io/AARP/articles/forward-model-basics.html) | Arrhenius kinetics, the power-law linearization, and seasonal temperature splitting |
| [Depth-series simulation](https://nickmckay.github.io/AARP/articles/depth-series.html) | Building age–depth models and running the forward model on a sediment core |
| [Stage 1: Kinetics estimation](https://nickmckay.github.io/AARP/articles/kinetics-fitting.html) | Bayesian calibration of Ae, Ea, and x from heating experiments and a 4 °C core |
| [Stage 2: Temperature reconstruction](https://nickmckay.github.io/AARP/articles/temperature-reconstruction.html) | Inferring a Holocene temperature history from downcore D/L with prior specification and posterior predictive checks |

## Priors

Both stages use weakly informative priors centred on published Ile-Ile values from Kaufman (2000):

| Parameter | Prior | Centre | SD |
|-----------|-------|--------|----|
| log(*A*e) | Normal | 42.12 | 2 |
| *E*a (kcal/mol) | Normal | 31.5 | 5 |
| *x* | Normal | 3 | 0.5 |
| log(σ) | Normal | log(0.02) | 1 |
| *T* knots (°C) | Normal | 5 | 3 |
| Δ*T* between knots | Normal | 0 | 2 |

## Funding

Developed as part of NSF grant 2317409, *"Testing amino acid paleothermometry in radiocarbon-dated lake sediment."*

## Citation

```r
citation("AARP")
```
