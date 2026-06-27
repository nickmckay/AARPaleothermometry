#' Build a TOC-depth model for bacterial resetting
#'
#' Creates a linear-interpolation function for total organic carbon (TOC) as a
#' function of depth. The interpolant is used inside
#' \code{\link{racemize_one_depth}} to scale the bacterial resetting term at
#' each burial depth during time integration.
#'
#' @param depth_cm Numeric vector of measurement depths in cm. Must start at 0
#'   and be monotonically increasing.
#' @param toc_pct Numeric vector of TOC values (percent dry weight) at each
#'   depth. Same length as \code{depth_cm}.
#'
#' @return A function `f(z_cm)` that returns TOC (percent) at any depth via
#'   linear interpolation. Values beyond the data range are held constant at
#'   the nearest endpoint (rule = 2).
#'
#' @seealso \code{\link{racemize_one_depth}} for how \code{toc_model} and
#'   \code{f_bac} are used in the forward model.
#'
#' @examples
#' toc_fn <- make_toc_model(depth_cm = c(0, 100, 300, 500),
#'                          toc_pct  = c(5,   3,   1, 0.5))
#' toc_fn(150)  # interpolated TOC at 150 cm
#'
#' @importFrom stats approxfun
#' @export
make_toc_model <- function(depth_cm, toc_pct) {
  stopifnot(length(depth_cm) == length(toc_pct))
  stopifnot(depth_cm[1] == 0)
  stats::approxfun(depth_cm, toc_pct, rule = 2)
}


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
#' \describe{
#'   \item{depth_to_age}{A function `f(depth_cm)` that returns age (ka BP).}
#'   \item{age_to_depth}{A function `f(age_ka)` that returns depth (cm).}
#' }
#'
#' @seealso \code{\link{rate_to_age_model}} for the constant-rate convenience
#'   wrapper.
#'
#' @examples
#' am <- make_age_model(depth_cm = c(0, 100, 300, 500),
#'                      age_ka   = c(0,   2,   6,  10))
#' am$depth_to_age(250)  # age at 250 cm
#' am$age_to_depth(5)    # depth at 5 ka BP
#'
#' @importFrom stats approxfun
#' @export
make_age_model <- function(depth_cm, age_ka) {
  stopifnot(length(depth_cm) == length(age_ka))
  stopifnot(depth_cm[1] == 0 && age_ka[1] == 0)

  list(
    depth_to_age = stats::approxfun(depth_cm, age_ka, rule = 2),
    age_to_depth = stats::approxfun(age_ka, depth_cm, rule = 2)
  )
}


#' Build an age-depth model from a constant sedimentation rate
#'
#' Convenience wrapper around \code{\link{make_age_model}} for the common case
#' of a constant sedimentation rate. Constructs a two-point age-depth model
#' anchored at the surface (0 cm, 0 ka) and at \code{max_depth_cm}.
#'
#' @param rate_cm_per_ka Sedimentation rate in cm/ka.
#' @param max_depth_cm Maximum depth (cm) of the core.
#'
#' @return A named list with `depth_to_age` and `age_to_depth` functions (see
#'   \code{\link{make_age_model}}).
#'
#' @examples
#' am <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 500)
#' am$depth_to_age(250)  # should return 5 ka
#'
#' @export
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
#' \describe{
#'   \item{scalar}{A single numeric value; temperature is constant through time.}
#'   \item{data.frame}{Columns `age_ka` (ka BP) and `temp_C` (°C). Values are
#'     linearly interpolated; flat extrapolation beyond the data range.}
#' }
#'
#' @return Numeric vector of temperatures (°C), same length as \code{t_ka}.
#'
#' @examples
#' get_temp_at_time(c(0, 5, 10), temp_input = 5)
#'
#' temp_df <- data.frame(age_ka = c(0, 10), temp_C = c(5, 8))
#' get_temp_at_time(c(0, 5, 10), temp_input = temp_df)
#'
#' @importFrom stats approxfun
#' @export
get_temp_at_time <- function(t_ka, temp_input) {
  if (is.numeric(temp_input) && length(temp_input) == 1) {
    return(rep(temp_input, length(t_ka)))
  }
  stopifnot(is.data.frame(temp_input))
  stopifnot(all(c("age_ka", "temp_C") %in% names(temp_input)))
  f <- stats::approxfun(temp_input$age_ka, temp_input$temp_C, rule = 2)
  f(t_ka)
}


#' Thermal diffusion attenuation factor for a periodic temperature signal
#'
#' Computes the amplitude attenuation of a sinusoidal surface temperature
#' forcing at burial depth \code{depth_m}, using the analytical solution to
#' the 1D heat equation for a semi-infinite medium:
#' \deqn{T(z,t) = T_\mathrm{mean} + A \, e^{-z/\delta}
#'   \sin\!\left(\omega t - z/\delta\right)}
#' where \eqn{\delta = \sqrt{\kappa T_\mathrm{period} / \pi}}.
#'
#' @param depth_m Burial depth in metres.
#' @param kappa_m2yr Thermal diffusivity of the sediment in m\eqn{^2}/yr.
#'   Typical silty lake sediment: ~1.6 m\eqn{^2}/yr (5e-8 m\eqn{^2}/s).
#' @param period_yr Period of the temperature forcing in years. Use `1` for
#'   the annual cycle; use a large value (e.g., `1000`) for Holocene-scale
#'   mean annual changes (attenuation then approaches 1 at all typical depths).
#'
#' @return Numeric vector of attenuation factors (0 to 1).
#'
#' @details
#' For `kappa_m2yr = 1.6` and `period_yr = 1` (annual cycle),
#' \eqn{\delta \approx 0.71} m. The seasonal amplitude is reduced to 37% at
#' 0.71 m depth and is effectively zero by ~2-3 m. With a 50 cm/ka
#' sedimentation rate, the seasonal signal is largely attenuated in samples
#' older than ~4-6 ka.
#'
#' @examples
#' depths <- seq(0, 3, by = 0.1)
#' plot(depths,
#'      diffusion_attenuation(depths, kappa_m2yr = 1.6, period_yr = 1),
#'      type = "l", xlab = "Depth (m)", ylab = "Attenuation")
#'
#' @export
diffusion_attenuation <- function(depth_m, kappa_m2yr, period_yr) {
  delta <- sqrt(kappa_m2yr * period_yr / pi)
  exp(-depth_m / delta)
}


#' Predict D/L for lab heating experiments
#'
#' Forward model for constant-temperature, constant-time heating experiments.
#' Assumes starting D/L = 0 (surface sediment with negligible natural
#' racemization). Reuses \code{\link{arrhenius}} and \code{\link{dRacPL}}.
#'
#' @param temp_C Numeric vector of heating temperatures in °C.
#' @param time_yr Numeric vector of heating durations in years.
#' @param AAR_params Named list with elements `Ae`, `Ea`, `R`, `x` (see
#'   \code{\link{default_AAR_params}}).
#'
#' @return Numeric vector of predicted D/L values.
#'
#' @examples
#' predict_heating_DL(temp_C = c(80, 100, 110), time_yr = 1,
#'                    AAR_params = default_AAR_params)
#'
#' @export
predict_heating_DL <- function(temp_C, time_yr, AAR_params) {
  kt <- arrhenius(Ae   = AAR_params$Ae,
                  Ea   = AAR_params$Ea,
                  Temp = temp_C + 273,
                  R    = AAR_params$R)
  Rx <- dRacPL(kt, time_yr)
  Rx^(1 / AAR_params$x)
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
#' @param age_model Age-depth model from \code{\link{make_age_model}} or
#'   \code{\link{rate_to_age_model}}.
#' @param temp_model Mean annual lake bottom water temperature: scalar (°C) or
#'   `data.frame(age_ka, temp_C)`.
#' @param sumT_model Optional summer half-year lake bottom temperature (same
#'   format as \code{temp_model}). Supply with \code{winT_model} to activate
#'   seasonal mode.
#' @param winT_model Optional winter half-year lake bottom temperature. Supply
#'   with \code{sumT_model} to activate seasonal mode.
#' @param dT_yr Timestep in years. Default `10`.
#' @param AAR_params Named list of kinetic parameters. Default
#'   \code{\link{default_AAR_params}}.
#' @param kappa_m2yr Sediment thermal diffusivity in m\eqn{^2}/yr. Used only
#'   in seasonal mode to attenuate the seasonal anomaly with depth. Default
#'   `1.6`.
#' @param toc_model Optional TOC interpolation function from
#'   \code{\link{make_toc_model}}. When supplied together with a nonzero
#'   \code{f_bac}, bacterial resetting is applied at each timestep.
#' @param f_bac Bacterial resetting rate constant (yr\eqn{^{-1}}). At each
#'   timestep the accumulated \eqn{(D/L)^x} (Rx) is reduced by
#'   \eqn{f\_bac \times (TOC_z / TOC_0) \times Rx \times dt}, where \eqn{TOC_z}
#'   is the TOC at the current burial depth and \eqn{TOC_0} is the surface TOC.
#'   Default `0` (pure abiotic racemization). Based on necromass turnover times
#'   of hundreds to thousands of years from Braun et al. (2017), plausible
#'   values are 1e-4 to 1e-2 yr\eqn{^{-1}}.
#'
#' @return A one-row data frame with columns `depth_cm`,
#'   `deposition_age_ka`, and `DL`.
#'
#' @details
#' **Mean-annual mode** (default): sediment temperature equals lake bottom
#' water temperature at all depths. Valid because the e-folding depth for
#' Holocene-period signals (centuries to millennia) is much larger than
#' typical core depths.
#'
#' **Seasonal mode** (`sumT_model` and `winT_model` both supplied): the
#' seasonal anomaly is attenuated with burial depth via
#' \code{\link{diffusion_attenuation}} at `period_yr = 1`. Each timestep is
#' split into equal summer and winter halves. Because the Arrhenius
#' relationship is nonlinear, identical mean temperatures with different
#' seasonal amplitudes produce different D/L values (Jensen's inequality).
#' The seasonal signal is largely gone by ~2-3 m depth.
#'
#' @seealso \code{\link{racemize_depth_series}} to run over a vector of depths.
#'
#' @examples
#' am <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 500)
#' racemize_one_depth(depth_cm = 250, age_model = am, temp_model = 5)
#'
#' # Seasonal mode
#' racemize_one_depth(depth_cm = 250, age_model = am,
#'                    temp_model = 5, sumT_model = 10, winT_model = 0)
#'
#' @export
racemize_one_depth <- function(depth_cm,
                               age_model,
                               temp_model,
                               sumT_model = NULL,
                               winT_model = NULL,
                               dT_yr      = 10,
                               AAR_params = default_AAR_params,
                               kappa_m2yr = 1.6,
                               toc_model  = NULL,
                               f_bac      = 0) {
  seasonal <- !is.null(sumT_model) && !is.null(winT_model)
  use_bac  <- !is.null(toc_model) && f_bac > 0

  t_dep_ka <- age_model$depth_to_age(depth_cm)

  if (t_dep_ka == 0) {
    return(data.frame(depth_cm = depth_cm, deposition_age_ka = 0, DL = 0))
  }

  dT_ka    <- dT_yr / 1000
  t_vec_ka <- seq(t_dep_ka, 0, by = -dT_ka)
  t_steps  <- t_vec_ka[-length(t_vec_ka)]

  z_t_cm <- pmax(depth_cm - age_model$age_to_depth(t_steps), 0)

  # Compute Arrhenius rate constants at all timesteps (vectorized regardless of
  # whether bacterial correction is used)
  if (seasonal) {
    T_sum_bottom <- get_temp_at_time(t_steps, sumT_model)
    T_win_bottom <- get_temp_at_time(t_steps, winT_model)
    T_mean       <- get_temp_at_time(t_steps, temp_model)

    # Attenuate seasonal anomaly with burial depth (annual cycle, period = 1 yr)
    atten_annual <- diffusion_attenuation(z_t_cm / 100, kappa_m2yr, period_yr = 1)

    T_sed_sum <- T_mean + (T_sum_bottom - T_mean) * atten_annual
    T_sed_win <- T_mean + (T_win_bottom - T_mean) * atten_annual

    kt_sum <- arrhenius(Ae = AAR_params$Ae, Ea = AAR_params$Ea,
                        Temp = T_sed_sum + 273, R = AAR_params$R)
    kt_win <- arrhenius(Ae = AAR_params$Ae, Ea = AAR_params$Ea,
                        Temp = T_sed_win + 273, R = AAR_params$R)
  } else {
    T_sed <- get_temp_at_time(t_steps, temp_model)
    kt    <- arrhenius(Ae = AAR_params$Ae, Ea = AAR_params$Ea,
                       Temp = T_sed + 273, R = AAR_params$R)
  }

  # Accumulate D/L in power-law (Rx) space.
  # Bacterial correction introduces a state-dependent decay term that depends on
  # Rx at the previous step, so a sequential loop is required when f_bac > 0.
  # When f_bac = 0 the vectorized sum is used (unchanged from abiotic model).
  if (use_bac) {
    toc_0 <- toc_model(0)
    Rx    <- 0
    for (i in seq_along(t_steps)) {
      dRx_abiotic <- if (seasonal) {
        dRacPL(kt_sum[i], dT_yr / 2) + dRacPL(kt_win[i], dT_yr / 2)
      } else {
        dRacPL(kt[i], dT_yr)
      }
      toc_z <- toc_model(z_t_cm[i])
      Rx    <- max(Rx + dRx_abiotic - f_bac * (toc_z / toc_0) * Rx * dT_yr, 0)
    }
  } else {
    Rx <- if (seasonal) {
      sum(dRacPL(kt_sum, dT_yr / 2) + dRacPL(kt_win, dT_yr / 2))
    } else {
      sum(dRacPL(kt = kt, delta.yr = dT_yr))
    }
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
#' @param age_model Age-depth model from \code{\link{make_age_model}} or
#'   \code{\link{rate_to_age_model}}.
#' @param temp_model Mean annual lake bottom water temperature: scalar or
#'   `data.frame(age_ka, temp_C)`.
#' @param sumT_model Optional summer half-year temperature (same format as
#'   \code{temp_model}). Supply with \code{winT_model} for seasonal mode.
#' @param winT_model Optional winter half-year temperature.
#' @param dT_yr Timestep in years. Default `10`.
#' @param AAR_params Named list of kinetic parameters. Default
#'   \code{\link{default_AAR_params}}.
#' @param kappa_m2yr Sediment thermal diffusivity in m\eqn{^2}/yr. Default
#'   `1.6`.
#' @param toc_model Optional TOC interpolation function from
#'   \code{\link{make_toc_model}}. Passed to each \code{\link{racemize_one_depth}}
#'   call.
#' @param f_bac Bacterial resetting rate constant (yr\eqn{^{-1}}). Default `0`.
#'   See \code{\link{racemize_one_depth}} for details.
#'
#' @return A data frame with columns `depth_cm`, `deposition_age_ka`, `DL`.
#'
#' @seealso \code{\link{racemize_one_depth}} for the single-sample function
#'   and full documentation of the two temperature modes.
#'
#' @examples
#' am     <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 500)
#' depths <- seq(0, 500, by = 10)
#' out    <- racemize_depth_series(depths, age_model = am, temp_model = 5)
#' head(out)
#'
#' @importFrom purrr map_dfr
#' @export
racemize_depth_series <- function(depths_cm,
                                  age_model,
                                  temp_model,
                                  sumT_model = NULL,
                                  winT_model = NULL,
                                  dT_yr      = 10,
                                  AAR_params = default_AAR_params,
                                  kappa_m2yr = 1.6,
                                  toc_model  = NULL,
                                  f_bac      = 0) {
  purrr::map_dfr(depths_cm,
                 racemize_one_depth,
                 age_model  = age_model,
                 temp_model = temp_model,
                 sumT_model = sumT_model,
                 winT_model = winT_model,
                 dT_yr      = dT_yr,
                 AAR_params = AAR_params,
                 kappa_m2yr = kappa_m2yr,
                 toc_model  = toc_model,
                 f_bac      = f_bac)
}
