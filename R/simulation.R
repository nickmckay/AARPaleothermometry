#' Simulate lab heating experiment D/L data
#'
#' Generates synthetic D/L observations for sediment heated at constant
#' elevated temperatures, following the controlled-temperature experiment
#' design in NSF proposal 2317409 (80, 100, 110°C; 8 time steps; 3 replicates
#' per condition, ~72 total observations).
#'
#' @param temps_C Numeric vector of heating temperatures (°C). Default
#'   `c(80, 100, 110)`.
#' @param times_yr Numeric vector of heating durations (years). Default covers
#'   ~1 hour to 1 year: `c(1/8760, 1/365, 7/365, 30/365, 90/365, 180/365,
#'   270/365, 1)`.
#' @param n_reps Integer. Replicates per temperature-time combination.
#'   Default `3`.
#' @param AAR_params Named list of true kinetic parameters used to generate
#'   the data. Default \code{\link{default_AAR_params}}.
#' @param sigma Standard deviation of Gaussian observation noise added to
#'   predicted D/L. Default `0.01`.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A data frame with columns: `temp_C`, `time_yr`, `replicate`,
#'   `DL_pred`, `DL_obs`.
#'
#' @examples
#' hd <- sim_heating_data(seed = 42)
#' head(hd)
#'
#' @importFrom stats rnorm
#' @export
sim_heating_data <- function(temps_C    = c(80, 100, 110),
                             times_yr   = c(1/8760, 1/365, 7/365, 30/365,
                                            90/365, 180/365, 270/365, 1),
                             n_reps     = 3,
                             AAR_params = default_AAR_params,
                             sigma      = 0.01,
                             seed       = NULL) {
  if (!is.null(seed)) set.seed(seed)

  grid         <- expand.grid(temp_C = temps_C, time_yr = times_yr,
                              replicate = seq_len(n_reps))
  grid$DL_pred <- predict_heating_DL(grid$temp_C, grid$time_yr, AAR_params)
  grid$DL_obs  <- pmax(grid$DL_pred + stats::rnorm(nrow(grid), 0, sigma), 0)
  grid
}


#' Simulate downcore D/L observations from a known-temperature lake
#'
#' Generates synthetic downcore D/L values by running the forward model
#' (\code{\link{racemize_depth_series}}) and adding Gaussian noise. Used for
#' both the 4°C calibration core (Stage 1) and the unknown-temperature target
#' core (Stage 2).
#'
#' @param age_model Age-depth model from \code{\link{make_age_model}} or
#'   \code{\link{rate_to_age_model}}.
#' @param temp_model Bottom water temperature history: scalar or
#'   `data.frame(age_ka, temp_C)`.
#' @param sample_ages_ka Numeric vector of sample ages (ka BP). Converted to
#'   depths via the age model.
#' @param AAR_params True kinetic parameters used to generate the data.
#'   Default \code{\link{default_AAR_params}}.
#' @param sigma Standard deviation of Gaussian observation noise. Default
#'   `0.015`.
#' @param seed Optional integer seed.
#' @param sumT_model Optional summer half-year temperature.
#' @param winT_model Optional winter half-year temperature.
#' @param toc_model Optional TOC interpolation function from
#'   \code{\link{make_toc_model}} for bacterial resetting. Default `NULL`.
#' @param f_bac Bacterial resetting rate constant (yr\eqn{^{-1}}). Default `0`.
#'
#' @return A data frame with columns: `depth_cm`, `deposition_age_ka`,
#'   `age_ka`, `DL_pred`, `DL_obs`.
#'
#' @examples
#' am <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 750)
#' dc <- sim_downcore_data(am, temp_model = 4,
#'                         sample_ages_ka = seq(0.5, 15, by = 0.5),
#'                         seed = 1)
#' head(dc)
#'
#' @importFrom stats rnorm
#' @export
sim_downcore_data <- function(age_model,
                              temp_model,
                              sample_ages_ka,
                              AAR_params  = default_AAR_params,
                              sigma       = 0.015,
                              seed        = NULL,
                              sumT_model  = NULL,
                              winT_model  = NULL,
                              toc_model   = NULL,
                              f_bac       = 0) {
  if (!is.null(seed)) set.seed(seed)

  depths_cm <- age_model$age_to_depth(sample_ages_ka)

  fwd <- racemize_depth_series(depths_cm,
                               age_model  = age_model,
                               temp_model = temp_model,
                               sumT_model = sumT_model,
                               winT_model = winT_model,
                               AAR_params = AAR_params,
                               toc_model  = toc_model,
                               f_bac      = f_bac)

  fwd$age_ka  <- sample_ages_ka
  fwd$DL_pred <- fwd$DL
  fwd$DL_obs  <- pmax(fwd$DL_pred + stats::rnorm(nrow(fwd), 0, sigma), 0)
  fwd$DL      <- NULL
  fwd
}
