# Bayesian AAR inversion — bayesian_inversion.R
#
# Two-stage Bayesian inversion framework for amino acid racemization
# paleothermometry.
#
# Stage 1: Fit kinetic parameters (Ae, Ea, x) from:
#   (a) Lab heating experiments at known temperatures and times
#   (b) Downcore D/L from a lake with known constant 4°C bottom water
#
# Stage 2: With kinetics fixed at Stage 1 posterior means, infer an unknown
#   Holocene temperature history from a target core's D/L profile.
#
# MCMC: hand-rolled random-walk Metropolis-Hastings (no package dependencies).
#
# Requires depth_series.R (which sources functions.R).

source("depth_series.R")


# =============================================================================
# Section 1 — Synthetic data generators
# =============================================================================

#' Simulate lab heating experiment D/L data
#'
#' Generates synthetic D/L observations for sediment heated at constant
#' elevated temperatures, mimicking the controlled-temperature experiments
#' described in NSF proposal 2317409 (80, 100, 110°C; 8 time steps; 3
#' replicates per condition).
#'
#' @param temps_C Numeric vector of heating temperatures in °C.
#' @param times_yr Numeric vector of heating durations in years.
#' @param n_reps Integer. Number of replicates per temperature-time combination.
#' @param AAR_params Named list of true kinetic parameters (see
#'   \code{default_AAR_params}).
#' @param sigma Numeric. Standard deviation of Gaussian observation noise
#'   added to predicted D/L.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A data frame with columns: \code{temp_C}, \code{time_yr},
#'   \code{replicate}, \code{DL_pred}, \code{DL_obs}.
sim_heating_data <- function(temps_C   = c(80, 100, 110),
                             times_yr  = c(1/8760, 1/365, 7/365, 30/365,
                                           90/365, 180/365, 270/365, 1),
                             n_reps    = 3,
                             AAR_params = default_AAR_params,
                             sigma     = 0.01,
                             seed      = NULL) {
  if (!is.null(seed)) set.seed(seed)

  grid <- expand.grid(temp_C = temps_C, time_yr = times_yr,
                      replicate = seq_len(n_reps))

  grid$DL_pred <- predict_heating_DL(grid$temp_C, grid$time_yr, AAR_params)
  grid$DL_obs  <- pmax(grid$DL_pred + rnorm(nrow(grid), 0, sigma), 0)
  grid
}


#' Simulate downcore D/L observations from a known-temperature lake
#'
#' Generates synthetic downcore D/L values by running the forward model
#' (\code{racemize_depth_series}) and adding Gaussian noise. Used for both
#' the 4°C calibration core and the unknown-temperature target core.
#'
#' @param age_model Age-depth model from \code{\link{make_age_model}} or
#'   \code{\link{rate_to_age_model}}.
#' @param temp_model Bottom water temperature history: scalar or
#'   \code{data.frame(age_ka, temp_C)}.
#' @param sample_ages_ka Numeric vector of sample ages (ka BP) at which to
#'   simulate observations. Converted to depths via the age model.
#' @param AAR_params True kinetic parameters used to generate the data.
#' @param sigma Standard deviation of Gaussian observation noise.
#' @param seed Optional integer seed.
#' @param sumT_model Optional summer half-year temperature (see
#'   \code{\link{racemize_depth_series}}).
#' @param winT_model Optional winter half-year temperature.
#'
#' @return A data frame with columns: \code{depth_cm}, \code{age_ka},
#'   \code{DL_pred}, \code{DL_obs}.
sim_downcore_data <- function(age_model,
                              temp_model,
                              sample_ages_ka,
                              AAR_params  = default_AAR_params,
                              sigma       = 0.015,
                              seed        = NULL,
                              sumT_model  = NULL,
                              winT_model  = NULL) {
  if (!is.null(seed)) set.seed(seed)

  depths_cm <- age_model$age_to_depth(sample_ages_ka)

  fwd <- racemize_depth_series(depths_cm,
                               age_model  = age_model,
                               temp_model = temp_model,
                               sumT_model = sumT_model,
                               winT_model = winT_model,
                               AAR_params = AAR_params)

  fwd$age_ka  <- sample_ages_ka
  fwd$DL_pred <- fwd$DL
  fwd$DL_obs  <- pmax(fwd$DL_pred + rnorm(nrow(fwd), 0, sigma), 0)
  fwd$DL      <- NULL
  fwd
}


# =============================================================================
# Section 2 — Heating experiment forward model
# =============================================================================

#' Predict D/L for lab heating experiments
#'
#' Forward model for a constant-temperature, constant-time heating experiment.
#' Assumes starting D/L = 0 (surface sediment with negligible natural
#' racemization). Reuses \code{arrhenius()} and \code{dRacPL()} from
#' \code{functions.R}.
#'
#' @param temp_C Numeric vector of heating temperatures in °C.
#' @param time_yr Numeric vector of heating durations in years (same length as
#'   \code{temp_C}, or recycled).
#' @param AAR_params Named list with elements \code{Ae}, \code{Ea}, \code{R},
#'   \code{x}.
#'
#' @return Numeric vector of predicted D/L values.
predict_heating_DL <- function(temp_C, time_yr, AAR_params) {
  kt <- arrhenius(Ae   = AAR_params$Ae,
                  Ea   = AAR_params$Ea,
                  Temp = temp_C + 273,
                  R    = AAR_params$R)
  Rx <- dRacPL(kt, time_yr)
  Rx^(1 / AAR_params$x)
}


# =============================================================================
# Section 3 — Log-likelihood functions
# =============================================================================

# Internal helper: assemble AAR_params list from sampled (log_Ae, Ea, x)
.make_AAR <- function(log_Ae, Ea, x) {
  list(Ae = exp(log_Ae), Ea = Ea, R = 0.001987, x = x)
}


#' Log-likelihood for lab heating experiment data
#'
#' @param log_Ae Log of the Arrhenius pre-exponential factor.
#' @param Ea Activation energy (kcal/mol).
#' @param x Power-law exponent.
#' @param log_sigma Log of the observation noise standard deviation.
#' @param heating_data Data frame returned by \code{\link{sim_heating_data}}.
#'
#' @return Scalar log-likelihood.
log_lik_heating <- function(log_Ae, Ea, x, log_sigma, heating_data) {
  if (x <= 0 || Ea <= 0) return(-Inf)
  sigma   <- exp(log_sigma)
  params  <- .make_AAR(log_Ae, Ea, x)
  DL_pred <- predict_heating_DL(heating_data$temp_C, heating_data$time_yr, params)
  if (any(!is.finite(DL_pred))) return(-Inf)
  sum(dnorm(heating_data$DL_obs, DL_pred, sigma, log = TRUE))
}


#' Log-likelihood for downcore D/L data at known temperature
#'
#' @param log_Ae Log of the Arrhenius pre-exponential factor.
#' @param Ea Activation energy (kcal/mol).
#' @param x Power-law exponent.
#' @param log_sigma Log of the observation noise standard deviation.
#' @param downcore_data Data frame returned by \code{\link{sim_downcore_data}}.
#' @param age_model Age-depth model from \code{\link{make_age_model}}.
#' @param temp_model Bottom water temperature (scalar or data frame).
#'
#' @return Scalar log-likelihood.
log_lik_downcore <- function(log_Ae, Ea, x, log_sigma,
                             downcore_data, age_model, temp_model) {
  if (x <= 0 || Ea <= 0) return(-Inf)
  sigma  <- exp(log_sigma)
  params <- .make_AAR(log_Ae, Ea, x)
  fwd    <- racemize_depth_series(downcore_data$depth_cm,
                                  age_model  = age_model,
                                  temp_model = temp_model,
                                  AAR_params = params)
  if (any(!is.finite(fwd$DL))) return(-Inf)
  sum(dnorm(downcore_data$DL_obs, fwd$DL, sigma, log = TRUE))
}


#' Log-likelihood for downcore D/L data at unknown temperature
#'
#' @param T_knots Numeric vector of temperatures (°C) at age control points.
#' @param knot_ages_ka Numeric vector of ages (ka BP) corresponding to
#'   \code{T_knots}. Must be the same length.
#' @param log_sigma Log of the observation noise standard deviation.
#' @param kinetics_params Named list with \code{log_Ae}, \code{Ea}, \code{x}.
#' @param downcore_data Data frame with columns \code{depth_cm} and
#'   \code{DL_obs}.
#' @param age_model Age-depth model.
#'
#' @return Scalar log-likelihood.
log_lik_temperature <- function(T_knots, knot_ages_ka, log_sigma,
                                kinetics_params, downcore_data, age_model) {
  sigma      <- exp(log_sigma)
  params     <- .make_AAR(kinetics_params[["log_Ae"]],
                          kinetics_params[["Ea"]],
                          kinetics_params[["x"]])
  temp_model <- data.frame(age_ka = knot_ages_ka, temp_C = T_knots)
  fwd        <- racemize_depth_series(downcore_data$depth_cm,
                                      age_model  = age_model,
                                      temp_model = temp_model,
                                      AAR_params = params)
  if (any(!is.finite(fwd$DL))) return(-Inf)
  sum(dnorm(downcore_data$DL_obs, fwd$DL, sigma, log = TRUE))
}


# =============================================================================
# Section 4 — Log-prior functions
# =============================================================================

#' Log-prior for kinetic parameters
#'
#' Weakly informative priors centred on literature values (Kaufman 2000).
#' Working in log space for Ae and sigma enforces positivity without hard
#' truncation.
#'
#' @param log_Ae Log of the pre-exponential factor. Prior: Normal(42.12, 2).
#' @param Ea Activation energy (kcal/mol). Prior: Normal(31.5, 5), soft
#'   lower bound at 0 via -Inf guard.
#' @param x Power-law exponent. Prior: Normal(3, 0.5), soft lower bound at 0.
#' @param log_sigma Log of observation noise SD. Prior: Normal(log(0.02), 1).
#'
#' @return Scalar log-prior.
log_prior_kinetics <- function(log_Ae, Ea, x, log_sigma) {
  if (x <= 0 || Ea <= 0) return(-Inf)
  dnorm(log_Ae,   42.12,      2,   log = TRUE) +
  dnorm(Ea,       31.5,       5,   log = TRUE) +
  dnorm(x,        3,          0.5, log = TRUE) +
  dnorm(log_sigma, log(0.02), 1,   log = TRUE)
}


#' Log-prior for temperature history control points
#'
#' Each knot temperature has a weakly informative Normal(5, 3) prior
#' representing plausible mean annual lake bottom temperatures. A
#' first-difference smoothness penalty discourages unrealistic oscillations
#' between adjacent control points.
#'
#' @param T_knots Numeric vector of temperatures at age control points (°C).
#' @param log_sigma Log of observation noise SD. Prior: Normal(log(0.02), 1).
#'
#' @return Scalar log-prior.
log_prior_temperature <- function(T_knots, log_sigma) {
  # Marginal prior on each knot
  lp <- sum(dnorm(T_knots, mean = 5, sd = 3, log = TRUE))
  # Smoothness: penalise large steps between adjacent knots
  lp <- lp + sum(dnorm(diff(T_knots), 0, 2, log = TRUE))
  lp + dnorm(log_sigma, log(0.02), 1, log = TRUE)
}


# =============================================================================
# Section 5 — Log-posterior functions
# =============================================================================

#' Log-posterior for kinetic parameters (Stage 1)
#'
#' Combines log-likelihoods from heating experiment data and downcore 4°C
#' calibration data with the kinetics prior.
#'
#' @param params Named numeric vector with elements \code{log_Ae}, \code{Ea},
#'   \code{x}, \code{log_sigma}.
#' @param heating_data Data frame from \code{\link{sim_heating_data}}.
#' @param downcore_cal_data Data frame from \code{\link{sim_downcore_data}}
#'   for the 4°C calibration lake.
#' @param age_model_cal Age-depth model for the calibration core.
#' @param temp_cal Bottom water temperature of the calibration lake (scalar,
#'   default 4°C).
#'
#' @return Scalar log-posterior (possibly -Inf for invalid parameters).
log_posterior_kinetics <- function(params,
                                   heating_data,
                                   downcore_cal_data,
                                   age_model_cal,
                                   temp_cal = 4) {
  log_Ae    <- params[["log_Ae"]]
  Ea        <- params[["Ea"]]
  x         <- params[["x"]]
  log_sigma <- params[["log_sigma"]]

  lp <- log_prior_kinetics(log_Ae, Ea, x, log_sigma)
  if (!is.finite(lp)) return(-Inf)

  ll_heat <- log_lik_heating(log_Ae, Ea, x, log_sigma, heating_data)
  ll_dc   <- log_lik_downcore(log_Ae, Ea, x, log_sigma,
                               downcore_cal_data, age_model_cal, temp_cal)
  lp + ll_heat + ll_dc
}


#' Log-posterior for temperature history (Stage 2)
#'
#' Kinetic parameters are held fixed at the values passed in
#' \code{kinetics_params} (typically the Stage 1 posterior means).
#'
#' @param params Named numeric vector of temperature knot values and
#'   \code{log_sigma}. Names should be \code{T_0ka}, \code{T_2ka}, ...,
#'   \code{log_sigma}.
#' @param knot_ages_ka Numeric vector of knot ages (ka BP).
#' @param kinetics_params Named vector or list with \code{log_Ae}, \code{Ea},
#'   \code{x}.
#' @param downcore_target_data Data frame from \code{\link{sim_downcore_data}}
#'   for the target (unknown-temperature) core.
#' @param age_model_target Age-depth model for the target core.
#'
#' @return Scalar log-posterior.
log_posterior_temperature <- function(params,
                                      knot_ages_ka,
                                      kinetics_params,
                                      downcore_target_data,
                                      age_model_target) {
  T_knots   <- params[seq_along(knot_ages_ka)]
  log_sigma <- params[["log_sigma"]]

  lp <- log_prior_temperature(T_knots, log_sigma)
  if (!is.finite(lp)) return(-Inf)

  log_lik_temperature(T_knots, knot_ages_ka, log_sigma,
                      kinetics_params, downcore_target_data, age_model_target) + lp
}


# =============================================================================
# Section 6 — Metropolis-Hastings MCMC sampler
# =============================================================================

#' Random-walk Metropolis-Hastings MCMC sampler
#'
#' A simple general-purpose sampler. Each iteration proposes a new parameter
#' vector by adding independent Gaussian noise to the current state, then
#' accepts or rejects via the Metropolis criterion.
#'
#' @param log_post_fn Function that takes a named numeric vector and returns
#'   the scalar log-posterior. Must accept the same names as \code{init}.
#' @param init Named numeric vector of starting parameter values.
#' @param n_iter Integer. Total number of MCMC iterations (including burn-in).
#' @param proposal_sd Named numeric vector of proposal standard deviations,
#'   one per parameter. Tune so that acceptance rate is 20–40%.
#' @param ... Additional arguments passed to \code{log_post_fn}.
#'
#' @return A list with:
#'   \describe{
#'     \item{samples}{Matrix of shape \code{[n_iter, n_params]} with column
#'       names matching \code{init}.}
#'     \item{acceptance_rate}{Overall Metropolis acceptance rate.}
#'     \item{log_post}{Numeric vector of log-posterior values at each
#'       iteration (useful for diagnosing convergence).}
#'   }
run_mcmc <- function(log_post_fn, init, n_iter, proposal_sd, ...) {
  n_params  <- length(init)
  pnames    <- names(init)
  samples   <- matrix(NA_real_, nrow = n_iter, ncol = n_params,
                      dimnames = list(NULL, pnames))
  log_post_trace <- numeric(n_iter)

  current  <- init
  lp_curr  <- log_post_fn(current, ...)
  accepted <- 0L

  for (i in seq_len(n_iter)) {
    proposal <- current + rnorm(n_params, 0, proposal_sd)
    names(proposal) <- pnames

    lp_prop <- log_post_fn(proposal, ...)
    log_r   <- lp_prop - lp_curr

    if (is.finite(log_r) && log(runif(1)) < log_r) {
      current  <- proposal
      lp_curr  <- lp_prop
      accepted <- accepted + 1L
    }

    samples[i, ]       <- current
    log_post_trace[i]  <- lp_curr
  }

  list(samples         = samples,
       acceptance_rate = accepted / n_iter,
       log_post        = log_post_trace)
}


# =============================================================================
# Section 7 — Diagnostics and visualization
# =============================================================================

#' Trace plots for MCMC output
#'
#' @param mcmc_out List returned by \code{\link{run_mcmc}}.
#' @param burnin Integer. Number of burn-in iterations to shade (not removed).
#'
#' @return A ggplot object.
plot_mcmc_chains <- function(mcmc_out, burnin = 1000) {
  df <- as.data.frame(mcmc_out$samples)
  df$iteration <- seq_len(nrow(df))

  long <- pivot_longer(df, -iteration, names_to = "parameter", values_to = "value")

  ggplot(long, aes(x = iteration, y = value)) +
    annotate("rect", xmin = 0, xmax = burnin,
             ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
    geom_line(linewidth = 0.3, colour = "steelblue") +
    facet_wrap(~parameter, scales = "free_y", ncol = 1) +
    labs(x = "Iteration", y = "Value",
         caption = paste0("Acceptance rate: ",
                          round(mcmc_out$acceptance_rate * 100, 1), "%")) +
    theme_bw()
}


#' Posterior density plots for kinetic parameters with true-value reference
#'
#' @param mcmc_out List returned by \code{\link{run_mcmc}}.
#' @param true_params Named numeric vector of true parameter values (for
#'   synthetic data validation). Set to NULL to omit reference lines.
#' @param burnin Integer. Burn-in iterations to discard before plotting.
#'
#' @return A ggplot object.
plot_posterior_kinetics <- function(mcmc_out, true_params = NULL, burnin = 1000) {
  post <- as.data.frame(tail(mcmc_out$samples, nrow(mcmc_out$samples) - burnin))
  long <- pivot_longer(post, everything(), names_to = "parameter", values_to = "value")

  p <- ggplot(long, aes(x = value)) +
    geom_density(fill = "steelblue", alpha = 0.4) +
    facet_wrap(~parameter, scales = "free", ncol = 2) +
    labs(x = "Value", y = "Density", title = "Posterior: kinetic parameters") +
    theme_bw()

  if (!is.null(true_params)) {
    ref <- data.frame(parameter = names(true_params), value = as.numeric(true_params))
    p   <- p + geom_vline(data = ref, aes(xintercept = value),
                          colour = "red", linetype = "dashed", linewidth = 0.8)
  }
  p
}


#' Temperature history reconstruction plot
#'
#' Shows the posterior median and 95% credible interval for the temperature
#' history against the true synthetic history.
#'
#' @param mcmc_out List returned by \code{\link{run_mcmc}} from Stage 2.
#' @param knot_ages_ka Numeric vector of knot ages (ka BP).
#' @param true_T_history Optional data frame with columns \code{age_ka} and
#'   \code{temp_C} for the true synthetic temperature history.
#' @param burnin Integer. Burn-in iterations to discard.
#'
#' @return A ggplot object.
plot_temperature_reconstruction <- function(mcmc_out, knot_ages_ka,
                                            true_T_history = NULL,
                                            burnin = 1000) {
  post    <- tail(mcmc_out$samples, nrow(mcmc_out$samples) - burnin)
  T_cols  <- grep("^T_", colnames(post), value = TRUE)
  T_post  <- post[, T_cols, drop = FALSE]

  summary_df <- data.frame(
    age_ka  = knot_ages_ka,
    median  = apply(T_post, 2, median),
    lo95    = apply(T_post, 2, quantile, 0.025),
    hi95    = apply(T_post, 2, quantile, 0.975),
    lo50    = apply(T_post, 2, quantile, 0.25),
    hi50    = apply(T_post, 2, quantile, 0.75)
  )

  p <- ggplot(summary_df, aes(x = age_ka)) +
    geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "steelblue", alpha = 0.2) +
    geom_ribbon(aes(ymin = lo50, ymax = hi50), fill = "steelblue", alpha = 0.4) +
    geom_line(aes(y = median), colour = "steelblue", linewidth = 1) +
    scale_x_reverse() +
    labs(x = "Age (ka BP)", y = "Temperature (°C)",
         title = "Reconstructed bottom-water temperature",
         subtitle = "Shading: 50% and 95% credible intervals") +
    theme_bw()

  if (!is.null(true_T_history)) {
    p <- p + geom_line(data = true_T_history,
                       aes(x = age_ka, y = temp_C),
                       colour = "red", linetype = "dashed", linewidth = 0.8)
  }
  p
}


#' Posterior predictive D/L depth profiles
#'
#' Draws parameter sets from the posterior and runs the forward model to show
#' the envelope of predicted D/L values vs. depth alongside the observations.
#'
#' @param mcmc_out List returned by \code{\link{run_mcmc}}.
#' @param age_model Age-depth model.
#' @param depths_cm Numeric vector of depths at which to evaluate predictions.
#' @param temp_model Temperature model used for predictions (scalar, data
#'   frame, or constructed from Stage 2 posterior — caller's responsibility).
#' @param AAR_params_fn Function that takes one row of the posterior sample
#'   matrix and returns an AAR_params list. Defaults to kinetics Stage 1
#'   usage (\code{log_Ae}, \code{Ea}, \code{x} columns).
#' @param observed_data Optional data frame with columns \code{depth_cm} and
#'   \code{DL_obs} to overlay.
#' @param burnin Integer. Burn-in iterations to discard.
#' @param n_draws Integer. Number of posterior draws to use.
#'
#' @return A ggplot object.
posterior_predictive_DL <- function(mcmc_out,
                                    age_model,
                                    depths_cm,
                                    temp_model,
                                    AAR_params_fn = function(row) {
                                      list(Ae = exp(row[["log_Ae"]]),
                                           Ea = row[["Ea"]],
                                           R  = 0.001987,
                                           x  = row[["x"]])
                                    },
                                    observed_data = NULL,
                                    burnin        = 1000,
                                    n_draws       = 200) {
  post    <- tail(mcmc_out$samples, nrow(mcmc_out$samples) - burnin)
  idx     <- sample(nrow(post), min(n_draws, nrow(post)))

  pred_list <- lapply(idx, function(i) {
    params <- AAR_params_fn(post[i, ])
    fwd    <- racemize_depth_series(depths_cm, age_model, temp_model,
                                    AAR_params = params)
    fwd$draw <- i
    fwd
  })
  pred_df <- bind_rows(pred_list)

  # Summarise across draws
  summ <- pred_df |>
    group_by(depth_cm) |>
    summarise(median = median(DL),
              lo95   = quantile(DL, 0.025),
              hi95   = quantile(DL, 0.975),
              .groups = "drop")

  p <- ggplot(summ, aes(x = DL, y = depth_cm)) +
    geom_ribbon(aes(xmin = lo95, xmax = hi95, y = depth_cm),
                fill = "steelblue", alpha = 0.3) +
    geom_line(aes(x = median), colour = "steelblue", linewidth = 1) +
    scale_y_reverse() +
    labs(x = "D/L", y = "Depth (cm)",
         title = "Posterior predictive D/L profile") +
    theme_bw()

  if (!is.null(observed_data)) {
    p <- p + geom_point(data = observed_data,
                        aes(x = DL_obs, y = depth_cm),
                        colour = "black", size = 1.5)
  }
  p
}


# =============================================================================
# Section 8 — End-to-end demo
# =============================================================================
# Uncomment to run the full two-stage synthetic example.
# Adjust n_iter and proposal_sd to tune acceptance rates (target 20-40%).

# library(ggplot2)
#
# # --- True parameters ---
# true_AAR <- default_AAR_params   # Ae=exp(42.12), Ea=31.5, x=3
# true_sigma_kin  <- 0.01
# true_sigma_down <- 0.015
#
# # --- Age-depth models ---
# am_cal    <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 750)  # 15 ka cal core
# am_target <- rate_to_age_model(rate_cm_per_ka = 50, max_depth_cm = 500)  # 10 ka target core
#
# # --- Synthetic data ---
# heating_data    <- sim_heating_data(AAR_params = true_AAR, sigma = true_sigma_kin, seed = 1)
# cal_data        <- sim_downcore_data(am_cal,
#                                      temp_model    = 4,
#                                      sample_ages_ka = seq(0.5, 15, by = 0.5),
#                                      AAR_params    = true_AAR,
#                                      sigma         = true_sigma_down,
#                                      seed          = 2)
# true_T_history  <- data.frame(age_ka = c(0, 10), temp_C = c(5, 8))  # linear cooling
# target_data     <- sim_downcore_data(am_target,
#                                      temp_model    = true_T_history,
#                                      sample_ages_ka = seq(0.5, 10, by = 0.5),
#                                      AAR_params    = true_AAR,
#                                      sigma         = true_sigma_down,
#                                      seed          = 3)
#
# # --- Stage 1: fit kinetics ---
# init_kin <- c(log_Ae = 42.12, Ea = 31.5, x = 3, log_sigma = log(0.01))
# prop_kin <- c(log_Ae = 0.05,  Ea = 0.3,  x = 0.05, log_sigma = 0.1)
#
# mcmc_kin <- run_mcmc(log_posterior_kinetics,
#                      init          = init_kin,
#                      n_iter        = 20000,
#                      proposal_sd   = prop_kin,
#                      heating_data       = heating_data,
#                      downcore_cal_data  = cal_data,
#                      age_model_cal      = am_cal,
#                      temp_cal           = 4)
#
# cat("Stage 1 acceptance rate:", round(mcmc_kin$acceptance_rate * 100, 1), "%\n")
# plot_mcmc_chains(mcmc_kin, burnin = 2000)
# plot_posterior_kinetics(mcmc_kin,
#                         true_params = c(log_Ae = 42.12, Ea = 31.5,
#                                         x = 3, log_sigma = log(0.01)),
#                         burnin = 2000)
#
# # --- Stage 2: fit temperature ---
# knot_ages   <- c(0, 2, 4, 6, 8, 10)
# kin_means   <- colMeans(tail(mcmc_kin$samples, 18000))
# kin_fixed   <- kin_means[c("log_Ae", "Ea", "x")]
#
# init_temp <- setNames(c(rep(6, length(knot_ages)), log(0.015)),
#                       c(paste0("T_", knot_ages, "ka"), "log_sigma"))
# prop_temp <- setNames(c(rep(0.15, length(knot_ages)), 0.1),
#                       names(init_temp))
#
# mcmc_temp <- run_mcmc(log_posterior_temperature,
#                       init        = init_temp,
#                       n_iter      = 20000,
#                       proposal_sd = prop_temp,
#                       knot_ages_ka         = knot_ages,
#                       kinetics_params      = kin_fixed,
#                       downcore_target_data = target_data,
#                       age_model_target     = am_target)
#
# cat("Stage 2 acceptance rate:", round(mcmc_temp$acceptance_rate * 100, 1), "%\n")
# plot_mcmc_chains(mcmc_temp, burnin = 2000)
# plot_temperature_reconstruction(mcmc_temp,
#                                 knot_ages_ka   = knot_ages,
#                                 true_T_history = true_T_history,
#                                 burnin         = 2000)
