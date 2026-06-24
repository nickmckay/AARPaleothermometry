# AAR depth-series forward model — depth_series.R
# Extends functions.R to simulate racemization for a depth series of sediment
# samples. Each sample's D/L is computed by integrating racemization over its
# full temperature history from deposition to present.
#
# Inputs:
#   - Sample depths (cm) where cores will be subsampled
#   - Age-depth model: data.frame(depth_cm, age_ka) for depth <-> age conversion
#   - Bottom water temperature: scalar (constant) or data.frame(age_ka, temp_C)
#   - AAR kinetic parameters and thermal diffusivity
#
# Requires functions.R (arrhenius, dRacPL).

source("functions.R")
library(tidyverse)


# Default AAR kinetic parameters. Ae and Ea are Kaufman (2000) literature values
# for isoleucine; not yet calibrated to site-specific data.
default_AAR_params <- list(
  Ae = exp(42.12),   # pre-exponential factor
  Ea = 31.5,         # activation energy (kcal/mol)
  R  = 0.001987,     # gas constant (kcal K-1 mol-1)
  x  = 3             # power-law exponent for D/L linearization
)


#' Build an age-depth model from tie-point depths and ages
#'
#' Creates a pair of linear-interpolation functions for converting between
#' depth (cm) and age (ka BP) in both directions. Both functions use flat
#' extrapolation (rule = 2) beyond the range of the control points.
#'
#' @param depth_cm Numeric vector of tie-point depths in cm. Must start at 0
#'   (the sediment surface) and be monotonically increasing.
#' @param age_ka Numeric vector of ages in ka BP corresponding to
#'   \code{depth_cm}. Must start at 0 and be the same length as
#'   \code{depth_cm}.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{depth_to_age}{A function \code{f(depth_cm)} that returns age (ka BP).}
#'     \item{age_to_depth}{A function \code{f(age_ka)} that returns depth (cm).}
#'   }
#'
#' @seealso \code{\link{rate_to_age_model}} for the constant-rate convenience
#'   wrapper.
#'
#' @examples
#' am <- make_age_model(depth_cm = c(0, 100, 300, 500),
#'                      age_ka   = c(0,   2,   6,  10))
#' am$depth_to_age(250)  # age at 250 cm
#' am$age_to_depth(5)    # depth at 5 ka BP
make_age_model <- function(depth_cm, age_ka) {
  stopifnot(length(depth_cm) == length(age_ka))
  stopifnot(depth_cm[1] == 0 && age_ka[1] == 0)  # must be anchored at surface

  list(
    depth_to_age = approxfun(depth_cm, age_ka, rule = 2),
    age_to_depth = approxfun(age_ka, depth_cm, rule = 2)
  )
}


#' Build an age-depth model from a constant sedimentation rate
#'
#' Convenience wrapper around \code{\link{make_age_model}} for the common case
#' of a constant sedimentation rate. Constructs a two-point age-depth model
#' anchored at the surface (0 cm, 0 ka) and at \code{max_depth_cm}.
#'
#' @param rate_cm_per_ka Sedimentation rate in cm/ka (centimetres per
#'   thousand years).
#' @param max_depth_cm Maximum depth (cm) of the core; defines the range of
#'   the model. Should be at least as deep as the deepest sample.
#'
#' @return A named list with \code{depth_to_age} and \code{age_to_depth}
#'   interpolation functions (see \code{\link{make_age_model}}).
#'
#' @examples
#' # 50 cm/ka rate, 500 cm core (= 10 ka record)
#' am <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 500)
#' am$depth_to_age(250)  # should return 5 ka
rate_to_age_model <- function(rate_cm_per_ka, max_depth_cm) {
  max_age_ka <- max_depth_cm / rate_cm_per_ka
  make_age_model(depth_cm = c(0, max_depth_cm),
                 age_ka   = c(0, max_age_ka))
}


#' Return lake bottom water temperature at one or more times
#'
#' Retrieves mean annual lake bottom water temperature (°C) at the requested
#' time(s). Accepts either a constant scalar or a time-varying data frame.
#'
#' @param t_ka Numeric vector of times in ka BP at which to evaluate
#'   temperature.
#' @param temp_input Temperature specification. Either:
#'   \describe{
#'     \item{scalar}{A single numeric value; temperature is constant through
#'       time.}
#'     \item{data.frame}{A data frame with columns \code{age_ka} (ka BP) and
#'       \code{temp_C} (°C). Values are linearly interpolated; flat
#'       extrapolation is used beyond the range of the data.}
#'   }
#'
#' @return Numeric vector of temperatures (°C), the same length as
#'   \code{t_ka}.
#'
#' @examples
#' # Constant temperature
#' get_temp_at_time(c(0, 5, 10), temp_input = 5)
#'
#' # Time-varying: Holocene cooling from 8°C at 10 ka to 5°C at present
#' temp_df <- data.frame(age_ka = c(0, 10), temp_C = c(5, 8))
#' get_temp_at_time(c(0, 5, 10), temp_input = temp_df)
get_temp_at_time <- function(t_ka, temp_input) {
  if (is.numeric(temp_input) && length(temp_input) == 1) {
    return(rep(temp_input, length(t_ka)))
  }
  stopifnot(is.data.frame(temp_input))
  stopifnot(all(c("age_ka", "temp_C") %in% names(temp_input)))
  f <- approxfun(temp_input$age_ka, temp_input$temp_C, rule = 2)
  f(t_ka)
}


#' Thermal diffusion attenuation factor for a periodic temperature signal
#'
#' Computes the amplitude attenuation of a sinusoidal surface temperature
#' forcing at burial depth \code{depth_m}, using the analytical solution to
#' the 1D heat equation for a semi-infinite medium:
#'
#' \deqn{T(z,t) = T_\mathrm{mean} + A \, e^{-z/\delta}
#'   \sin\!\left(\omega t - z/\delta\right)}
#'
#' where \eqn{\delta = \sqrt{\kappa T_\mathrm{period} / \pi}} is the
#' e-folding depth and \eqn{\omega = 2\pi / T_\mathrm{period}}.
#'
#' The function returns only the amplitude factor \eqn{e^{-z/\delta}}.
#'
#' @param depth_m Burial depth in metres.
#' @param kappa_m2yr Thermal diffusivity of the sediment in m\eqn{^2}/yr.
#'   Typical value for silty lake sediment: ~1.6 m\eqn{^2}/yr
#'   (5 \eqn{\times} 10\eqn{^{-8}} m\eqn{^2}/s).
#' @param period_yr Period of the temperature forcing in years. Use 1 for the
#'   annual (seasonal) cycle. For Holocene-scale mean annual temperature
#'   changes use a large value (e.g. 1000); the attenuation then approaches
#'   1 at all typical core depths.
#'
#' @return Numeric vector of attenuation factors between 0 and 1, the same
#'   length as \code{depth_m}.
#'
#' @details
#' For \code{kappa_m2yr = 1.6} and \code{period_yr = 1} (annual cycle),
#' \eqn{\delta \approx 0.71} m. The seasonal amplitude is reduced to 37\% at
#' 0.71 m depth and is effectively zero by ~2–3 m depth. With a typical
#' Holocene sedimentation rate of 50 cm/ka, this means seasonal signals are
#' largely attenuated in samples older than ~4–6 ka.
#'
#' @examples
#' # Attenuation of the annual cycle vs. depth
#' depths <- seq(0, 3, by = 0.1)
#' plot(depths, diffusion_attenuation(depths, kappa_m2yr = 1.6, period_yr = 1),
#'      type = "l", xlab = "Depth (m)", ylab = "Attenuation",
#'      main = "Annual cycle attenuation (kappa = 1.6 m2/yr)")
diffusion_attenuation <- function(depth_m, kappa_m2yr, period_yr) {
  delta <- sqrt(kappa_m2yr * period_yr / pi)
  exp(-depth_m / delta)
}


#' Simulate AAR racemization for a single sediment sample at a given depth
#'
#' Computes the expected D/L ratio for a sediment sample currently at depth
#' \code{depth_cm} by integrating racemization from the moment of deposition
#' to the present. At each timestep, the sample's burial depth is computed
#' from the age-depth model, and the sediment temperature is derived from the
#' lake bottom water temperature with optional thermal diffusion attenuation of
#' the seasonal signal.
#'
#' @param depth_cm Current depth of the sample in cm.
#' @param age_model Age-depth model returned by \code{\link{make_age_model}} or
#'   \code{\link{rate_to_age_model}}.
#' @param temp_model Mean annual lake bottom water temperature. Either a scalar
#'   (°C, constant through time) or a data frame with columns \code{age_ka}
#'   and \code{temp_C} (see \code{\link{get_temp_at_time}}).
#' @param sumT_model Optional. Summer half-year lake bottom temperature,
#'   in the same format as \code{temp_model}. Must be supplied together with
#'   \code{winT_model} to activate seasonal mode.
#' @param winT_model Optional. Winter half-year lake bottom temperature, in
#'   the same format as \code{temp_model}. Must be supplied together with
#'   \code{sumT_model} to activate seasonal mode.
#' @param dT_yr Timestep in years. Default 10.
#' @param AAR_params Named list of AAR kinetic parameters:
#'   \describe{
#'     \item{Ae}{Pre-exponential (frequency) factor in the Arrhenius equation.}
#'     \item{Ea}{Activation energy in kcal/mol.}
#'     \item{R}{Gas constant in kcal K\eqn{^{-1}} mol\eqn{^{-1}}.}
#'     \item{x}{Power-law exponent: D/L = Rx\eqn{^{1/x}}.}
#'   }
#'   Defaults to \code{default_AAR_params} (Kaufman 2000 literature values).
#' @param kappa_m2yr Thermal diffusivity of the sediment in m\eqn{^2}/yr,
#'   used only in seasonal mode to attenuate the seasonal anomaly with depth.
#'   Default 1.6 m\eqn{^2}/yr (typical silty lake sediment).
#'
#' @return A one-row data frame with columns:
#'   \describe{
#'     \item{depth_cm}{The input sample depth (cm).}
#'     \item{deposition_age_ka}{Age of the sample at deposition (ka BP).}
#'     \item{DL}{Simulated D/L ratio at the present day.}
#'   }
#'
#' @details
#' \strong{Mean-annual mode} (default, \code{sumT_model} and
#' \code{winT_model} both \code{NULL}): sediment temperature equals lake
#' bottom water temperature at all depths. This is valid because the
#' attenuation of Holocene-scale temperature signals is negligible at typical
#' core depths (the e-folding depth for a 1000-yr period is ~22 m).
#'
#' \strong{Seasonal mode} (\code{sumT_model} and \code{winT_model} both
#' supplied): the seasonal anomaly (departure of summer/winter temperature
#' from the mean) is attenuated with burial depth using
#' \code{\link{diffusion_attenuation}} at \code{period_yr = 1}. Each
#' timestep is split into equal summer and winter halves. Because the
#' Arrhenius relationship is nonlinear, two climates with the same mean annual
#' temperature but different seasonal amplitudes will produce different D/L
#' values — warmer summers contribute disproportionately more than cooler
#' winters subtract (Jensen's inequality). The seasonal signal is largely gone
#' by ~2–3 m depth for typical sediment thermal diffusivity.
#'
#' @seealso \code{\link{racemize_depth_series}} to run this over a vector of
#'   depths; \code{\link{make_age_model}}, \code{\link{get_temp_at_time}},
#'   \code{\link{diffusion_attenuation}}.
#'
#' @examples
#' am <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 500)
#'
#' # Mean-annual mode
#' racemize_one_depth(depth_cm = 250, age_model = am, temp_model = 5)
#'
#' # Seasonal mode
#' racemize_one_depth(depth_cm   = 250,
#'                    age_model  = am,
#'                    temp_model = 5,
#'                    sumT_model = 10,
#'                    winT_model = 0)
racemize_one_depth <- function(depth_cm,
                               age_model,
                               temp_model,
                               sumT_model = NULL,
                               winT_model = NULL,
                               dT_yr      = 10,
                               AAR_params = default_AAR_params,
                               kappa_m2yr = 1.6) {

  seasonal <- !is.null(sumT_model) && !is.null(winT_model)

  # Deposition age: when did this layer arrive at the sediment surface?
  t_dep_ka <- age_model$depth_to_age(depth_cm)

  if (t_dep_ka == 0) {
    return(data.frame(depth_cm = depth_cm, deposition_age_ka = 0, DL = 0))
  }

  # Time vector from deposition to present (old to young, in ka)
  dT_ka    <- dT_yr / 1000
  t_vec_ka <- seq(t_dep_ka, 0, by = -dT_ka)
  t_steps  <- t_vec_ka[-length(t_vec_ka)]

  # Burial depth at each timestep: 0 at deposition, depth_cm at present.
  # Sediment above this layer at time t = all material deposited since time t.
  z_t_cm <- pmax(depth_cm - age_model$age_to_depth(t_steps), 0)

  if (seasonal) {
    # --- Seasonal mode ---
    T_sum_bottom <- get_temp_at_time(t_steps, sumT_model)
    T_win_bottom <- get_temp_at_time(t_steps, winT_model)
    T_mean       <- get_temp_at_time(t_steps, temp_model)

    # Attenuate the seasonal anomaly (departure from mean) with burial depth.
    # The annual cycle has period = 1 yr; deeper sediment converges to T_mean.
    atten_annual <- diffusion_attenuation(z_t_cm / 100, kappa_m2yr, period_yr = 1)

    T_sed_sum <- T_mean + (T_sum_bottom - T_mean) * atten_annual
    T_sed_win <- T_mean + (T_win_bottom - T_mean) * atten_annual

    kt_sum <- arrhenius(Ae   = AAR_params$Ae,
                        Ea   = AAR_params$Ea,
                        Temp = T_sed_sum + 273,
                        R    = AAR_params$R)
    kt_win <- arrhenius(Ae   = AAR_params$Ae,
                        Ea   = AAR_params$Ea,
                        Temp = T_sed_win + 273,
                        R    = AAR_params$R)

    # Each year = summer half + winter half
    Rx <- sum(dRacPL(kt_sum, dT_yr / 2) + dRacPL(kt_win, dT_yr / 2))

  } else {
    # --- Mean-annual mode ---
    # Holocene-period signals penetrate fully to all typical depths (delta >> depth),
    # so sediment temperature = lake bottom water temperature directly.
    T_sed <- get_temp_at_time(t_steps, temp_model)

    kt <- arrhenius(Ae   = AAR_params$Ae,
                    Ea   = AAR_params$Ea,
                    Temp = T_sed + 273,
                    R    = AAR_params$R)

    Rx <- sum(dRacPL(kt = kt, delta.yr = dT_yr))
  }

  data.frame(
    depth_cm          = depth_cm,
    deposition_age_ka = t_dep_ka,
    DL                = Rx^(1 / AAR_params$x)
  )
}


#' Simulate AAR racemization for a depth series of sediment samples
#'
#' Applies \code{\link{racemize_one_depth}} over a vector of sample depths
#' and returns the results as a single data frame. This is the primary entry
#' point for forward-modelling a downcore D/L profile.
#'
#' @param depths_cm Numeric vector of sample depths in cm.
#' @param age_model Age-depth model returned by \code{\link{make_age_model}}
#'   or \code{\link{rate_to_age_model}}.
#' @param temp_model Mean annual lake bottom water temperature. Either a
#'   scalar (°C) or a data frame with columns \code{age_ka} and
#'   \code{temp_C}.
#' @param sumT_model Optional. Summer half-year lake bottom temperature (same
#'   format as \code{temp_model}). Supply together with \code{winT_model} to
#'   activate seasonal mode.
#' @param winT_model Optional. Winter half-year lake bottom temperature (same
#'   format as \code{temp_model}). Supply together with \code{sumT_model} to
#'   activate seasonal mode.
#' @param dT_yr Timestep in years. Default 10.
#' @param AAR_params Named list of AAR kinetic parameters. Defaults to
#'   \code{default_AAR_params} (Kaufman 2000 literature values for
#'   isoleucine).
#' @param kappa_m2yr Sediment thermal diffusivity in m\eqn{^2}/yr. Used only
#'   in seasonal mode. Default 1.6 m\eqn{^2}/yr.
#'
#' @return A data frame with one row per element of \code{depths_cm} and
#'   columns:
#'   \describe{
#'     \item{depth_cm}{Sample depth (cm).}
#'     \item{deposition_age_ka}{Age at deposition (ka BP).}
#'     \item{DL}{Simulated D/L ratio.}
#'   }
#'
#' @seealso \code{\link{racemize_one_depth}} for the single-sample function
#'   and full details of the two temperature modes.
#'
#' @examples
#' am     <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 500)
#' depths <- seq(0, 500, by = 10)
#'
#' # Mean-annual mode: constant 5°C bottom water
#' out_ann <- racemize_depth_series(depths, age_model = am, temp_model = 5)
#'
#' # Seasonal mode: same mean T but with 10°C amplitude
#' out_sea <- racemize_depth_series(depths,
#'                                  age_model  = am,
#'                                  temp_model = 5,
#'                                  sumT_model = 10,
#'                                  winT_model = 0)
#'
#' # D/L increases with depth; seasonal > mean-annual due to Arrhenius nonlinearity
#' library(ggplot2)
#' bind_rows(dplyr::mutate(out_ann, mode = "mean-annual"),
#'           dplyr::mutate(out_sea, mode = "seasonal")) |>
#'   ggplot(aes(x = DL, y = depth_cm, color = mode)) +
#'   geom_line() + scale_y_reverse() + theme_bw()
racemize_depth_series <- function(depths_cm,
                                  age_model,
                                  temp_model,
                                  sumT_model = NULL,
                                  winT_model = NULL,
                                  dT_yr      = 10,
                                  AAR_params = default_AAR_params,
                                  kappa_m2yr = 1.6) {

  map_dfr(depths_cm,
          racemize_one_depth,
          age_model  = age_model,
          temp_model = temp_model,
          sumT_model = sumT_model,
          winT_model = winT_model,
          dT_yr      = dT_yr,
          AAR_params = AAR_params,
          kappa_m2yr = kappa_m2yr)
}
