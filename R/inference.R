# Internal helper: assemble AAR_params list from sampled transformed parameters
.make_AAR <- function(log_Ae, Ea, x) {
  list(Ae = exp(log_Ae), Ea = Ea, R = 0.001987, x = x)
}


# =============================================================================
# Log-likelihood functions
# =============================================================================

#' Log-likelihood for lab heating experiment data
#'
#' @param log_Ae Log of the Arrhenius pre-exponential factor.
#' @param Ea Activation energy (kcal/mol).
#' @param x Power-law exponent.
#' @param log_sigma Log of the observation noise standard deviation.
#' @param heating_data Data frame from \code{\link{sim_heating_data}}.
#'
#' @return Scalar log-likelihood, or `-Inf` for invalid parameters.
#'
#' @importFrom stats dnorm
#' @export
log_lik_heating <- function(log_Ae, Ea, x, log_sigma, heating_data) {
  if (x <= 0 || Ea <= 0) return(-Inf)
  sigma   <- exp(log_sigma)
  params  <- .make_AAR(log_Ae, Ea, x)
  DL_pred <- predict_heating_DL(heating_data$temp_C, heating_data$time_yr,
                                params)
  if (any(!is.finite(DL_pred))) return(-Inf)
  sum(stats::dnorm(heating_data$DL_obs, DL_pred, sigma, log = TRUE))
}


#' Log-likelihood for downcore D/L data at known temperature
#'
#' @param log_Ae Log of the Arrhenius pre-exponential factor.
#' @param Ea Activation energy (kcal/mol).
#' @param x Power-law exponent.
#' @param log_sigma Log of the observation noise standard deviation.
#' @param downcore_data Data frame from \code{\link{sim_downcore_data}}.
#' @param age_model Age-depth model from \code{\link{make_age_model}}.
#' @param temp_model Bottom water temperature (scalar or data frame).
#'
#' @return Scalar log-likelihood.
#'
#' @importFrom stats dnorm
#' @export
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
  sum(stats::dnorm(downcore_data$DL_obs, fwd$DL, sigma, log = TRUE))
}


#' Log-likelihood for downcore D/L data at unknown temperature
#'
#' @param T_knots Numeric vector of temperatures (deg C) at age control points.
#' @param knot_ages_ka Numeric vector of ages (ka BP) for the control points.
#' @param log_sigma Log of the observation noise standard deviation.
#' @param kinetics_params Named vector or list with elements `log_Ae`,
#'   `Ea`, `x`.
#' @param downcore_data Data frame with columns `depth_cm` and `DL_obs`.
#' @param age_model Age-depth model.
#'
#' @return Scalar log-likelihood.
#'
#' @importFrom stats dnorm
#' @export
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
  sum(stats::dnorm(downcore_data$DL_obs, fwd$DL, sigma, log = TRUE))
}


# =============================================================================
# Log-prior functions
# =============================================================================

#' Log-prior for kinetic parameters
#'
#' Weakly informative priors centred on Kaufman (2000) literature values.
#' Parameters are sampled on transformed scales (`log_Ae`, `log_sigma`) to
#' enforce positivity without hard truncation.
#'
#' @param log_Ae Log of the pre-exponential factor. Prior: Normal(42.12, 2).
#' @param Ea Activation energy (kcal/mol). Prior: Normal(31.5, 5).
#' @param x Power-law exponent. Prior: Normal(3, 0.5).
#' @param log_sigma Log of observation noise SD. Prior: Normal(log(0.02), 1).
#'
#' @return Scalar log-prior.
#'
#' @importFrom stats dnorm
#' @export
log_prior_kinetics <- function(log_Ae, Ea, x, log_sigma) {
  if (x <= 0 || Ea <= 0) return(-Inf)
  stats::dnorm(log_Ae,    42.12,      2,   log = TRUE) +
  stats::dnorm(Ea,        31.5,       5,   log = TRUE) +
  stats::dnorm(x,         3,          0.5, log = TRUE) +
  stats::dnorm(log_sigma, log(0.02),  1,   log = TRUE)
}


#' Log-prior for temperature history control points
#'
#' Weakly informative Normal(5, 3) prior on each knot temperature, plus a
#' first-difference smoothness penalty that discourages large jumps between
#' adjacent control points.
#'
#' @param T_knots Numeric vector of temperatures at age control points (deg C).
#' @param log_sigma Log of observation noise SD. Prior: Normal(log(0.02), 1).
#'
#' @return Scalar log-prior.
#'
#' @importFrom stats dnorm
#' @export
log_prior_temperature <- function(T_knots, log_sigma) {
  lp <- sum(stats::dnorm(T_knots, mean = 5, sd = 3, log = TRUE))
  # Smoothness: penalise large steps between adjacent knots
  lp <- lp + sum(stats::dnorm(diff(T_knots), 0, 2, log = TRUE))
  lp + stats::dnorm(log_sigma, log(0.02), 1, log = TRUE)
}


# =============================================================================
# Log-posterior functions
# =============================================================================

#' Log-posterior for kinetic parameters (Stage 1)
#'
#' Combines likelihoods from heating experiment data and a known-temperature
#' (4deg C) calibration core with the kinetics prior.
#'
#' @param params Named numeric vector: `log_Ae`, `Ea`, `x`, `log_sigma`.
#' @param heating_data Data frame from \code{\link{sim_heating_data}}.
#' @param downcore_cal_data Data frame from \code{\link{sim_downcore_data}}
#'   for the 4deg C calibration lake.
#' @param age_model_cal Age-depth model for the calibration core.
#' @param temp_cal Bottom water temperature of the calibration lake (default
#'   `4`deg C).
#'
#' @return Scalar log-posterior (possibly `-Inf` for invalid parameters).
#'
#' @export
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

  lp +
    log_lik_heating(log_Ae, Ea, x, log_sigma, heating_data) +
    log_lik_downcore(log_Ae, Ea, x, log_sigma,
                     downcore_cal_data, age_model_cal, temp_cal)
}


#' Log-posterior for temperature history (Stage 2)
#'
#' Kinetic parameters are fixed at the values in \code{kinetics_params}
#' (typically the Stage 1 posterior means).
#'
#' @param params Named numeric vector of temperature knot values and
#'   `log_sigma`. Names: `T_0ka`, `T_2ka`, ..., `log_sigma`.
#' @param knot_ages_ka Numeric vector of knot ages (ka BP).
#' @param kinetics_params Named vector or list with `log_Ae`, `Ea`, `x`.
#' @param downcore_target_data Data frame from \code{\link{sim_downcore_data}}
#'   for the unknown-temperature target core.
#' @param age_model_target Age-depth model for the target core.
#'
#' @return Scalar log-posterior.
#'
#' @export
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
                      kinetics_params, downcore_target_data,
                      age_model_target) + lp
}


# =============================================================================
# MCMC sampler
# =============================================================================

#' Random-walk Metropolis-Hastings MCMC sampler
#'
#' A simple general-purpose sampler. Each iteration proposes a new parameter
#' vector by adding independent Gaussian noise to the current state, then
#' accepts or rejects via the Metropolis criterion.
#'
#' @param log_post_fn Function taking a named numeric vector and returning a
#'   scalar log-posterior.
#' @param init Named numeric vector of starting parameter values.
#' @param n_iter Integer. Total iterations (including burn-in).
#' @param proposal_sd Named numeric vector of proposal standard deviations,
#'   one per parameter. Tune for 20-40% acceptance rate.
#' @param ... Additional arguments passed to \code{log_post_fn}.
#'
#' @return A list with:
#' \describe{
#'   \item{samples}{Matrix `[n_iter x n_params]` with column names from
#'     `init`.}
#'   \item{acceptance_rate}{Overall Metropolis acceptance rate.}
#'   \item{log_post}{Numeric vector of log-posterior values at each
#'     iteration.}
#' }
#'
#' @examples
#' \dontrun{
#' # See vignette("kinetics-fitting") for a worked example.
#' }
#'
#' @importFrom stats rnorm runif
#' @export
run_mcmc <- function(log_post_fn, init, n_iter, proposal_sd, ...) {
  n_params <- length(init)
  pnames   <- names(init)
  samples  <- matrix(NA_real_, nrow = n_iter, ncol = n_params,
                     dimnames = list(NULL, pnames))
  log_post_trace <- numeric(n_iter)

  current  <- init
  lp_curr  <- log_post_fn(current, ...)
  accepted <- 0L

  for (i in seq_len(n_iter)) {
    proposal        <- current + stats::rnorm(n_params, 0, proposal_sd)
    names(proposal) <- pnames

    lp_prop <- log_post_fn(proposal, ...)
    log_r   <- lp_prop - lp_curr

    if (is.finite(log_r) && log(stats::runif(1)) < log_r) {
      current  <- proposal
      lp_curr  <- lp_prop
      accepted <- accepted + 1L
    }

    samples[i, ]      <- current
    log_post_trace[i] <- lp_curr
  }

  list(samples         = samples,
       acceptance_rate = accepted / n_iter,
       log_post        = log_post_trace)
}


# =============================================================================
# Diagnostics and visualization
# =============================================================================

#' Trace plots for MCMC output
#'
#' @param mcmc_out List returned by \code{\link{run_mcmc}}.
#' @param burnin Integer. Burn-in iterations to shade (not removed). Default
#'   `1000`.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_line annotate facet_wrap labs theme_bw
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export
plot_mcmc_chains <- function(mcmc_out, burnin = 1000) {
  df            <- as.data.frame(mcmc_out$samples)
  df$iteration  <- seq_len(nrow(df))
  long          <- tidyr::pivot_longer(df, -"iteration",
                                       names_to = "parameter",
                                       values_to = "value")

  ggplot2::ggplot(long, ggplot2::aes(x = .data$iteration, y = .data$value)) +
    ggplot2::annotate("rect", xmin = 0, xmax = burnin,
                      ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5) +
    ggplot2::geom_line(linewidth = 0.3, colour = "steelblue") +
    ggplot2::facet_wrap(~parameter, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = "Iteration", y = "Value",
                  caption = paste0("Acceptance rate: ",
                                   round(mcmc_out$acceptance_rate * 100, 1),
                                   "%")) +
    ggplot2::theme_bw()
}


#' Posterior density plots for kinetic parameters
#'
#' @param mcmc_out List returned by \code{\link{run_mcmc}}.
#' @param true_params Named numeric vector of true parameter values (for
#'   synthetic data validation). `NULL` to omit reference lines.
#' @param burnin Integer. Burn-in iterations to discard before plotting.
#'   Default `1000`.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_density geom_vline facet_wrap labs
#'   theme_bw
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export
plot_posterior_kinetics <- function(mcmc_out, true_params = NULL,
                                    burnin = 1000) {
  post <- as.data.frame(
    utils::tail(mcmc_out$samples, nrow(mcmc_out$samples) - burnin)
  )
  long <- tidyr::pivot_longer(post, dplyr::everything(),
                              names_to = "parameter", values_to = "value")

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_density(fill = "steelblue", alpha = 0.4) +
    ggplot2::facet_wrap(~parameter, scales = "free", ncol = 2) +
    ggplot2::labs(x = "Value", y = "Density",
                  title = "Posterior: kinetic parameters") +
    ggplot2::theme_bw()

  if (!is.null(true_params)) {
    ref <- data.frame(parameter = names(true_params),
                      value     = as.numeric(true_params))
    p   <- p + ggplot2::geom_vline(data = ref,
                                   ggplot2::aes(xintercept = .data$value),
                                   colour = "red", linetype = "dashed",
                                   linewidth = 0.8)
  }
  p
}


#' Temperature history reconstruction plot
#'
#' Posterior median and 95%/50% credible intervals for the reconstructed
#' temperature history, optionally overlaid with the true synthetic history.
#'
#' @param mcmc_out List returned by \code{\link{run_mcmc}} from Stage 2.
#' @param knot_ages_ka Numeric vector of knot ages (ka BP).
#' @param true_T_history Optional data frame with columns `age_ka` and
#'   `temp_C` for the true synthetic temperature history.
#' @param burnin Integer. Burn-in iterations to discard. Default `1000`.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line scale_x_reverse labs
#'   theme_bw
#' @importFrom stats median quantile
#' @importFrom rlang .data
#' @export
plot_temperature_reconstruction <- function(mcmc_out, knot_ages_ka,
                                            true_T_history = NULL,
                                            burnin = 1000) {
  post   <- utils::tail(mcmc_out$samples, nrow(mcmc_out$samples) - burnin)
  T_cols <- grep("^T_", colnames(post), value = TRUE)
  T_post <- post[, T_cols, drop = FALSE]

  summary_df <- data.frame(
    age_ka = knot_ages_ka,
    median = apply(T_post, 2, stats::median),
    lo95   = apply(T_post, 2, stats::quantile, 0.025),
    hi95   = apply(T_post, 2, stats::quantile, 0.975),
    lo50   = apply(T_post, 2, stats::quantile, 0.25),
    hi50   = apply(T_post, 2, stats::quantile, 0.75)
  )

  p <- ggplot2::ggplot(summary_df, ggplot2::aes(x = .data$age_ka)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo95, ymax = .data$hi95),
                         fill = "steelblue", alpha = 0.2) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo50, ymax = .data$hi50),
                         fill = "steelblue", alpha = 0.4) +
    ggplot2::geom_line(ggplot2::aes(y = .data$median),
                       colour = "steelblue", linewidth = 1) +
    ggplot2::scale_x_reverse() +
    ggplot2::labs(x = "Age (ka BP)", y = "Temperature (deg C)",
                  title = "Reconstructed bottom-water temperature",
                  subtitle = "Shading: 50% and 95% credible intervals") +
    ggplot2::theme_bw()

  if (!is.null(true_T_history)) {
    p <- p + ggplot2::geom_line(data = true_T_history,
                                ggplot2::aes(x = .data$age_ka,
                                             y = .data$temp_C),
                                colour = "red", linetype = "dashed",
                                linewidth = 0.8)
  }
  p
}


#' Posterior predictive D/L depth profiles
#'
#' Draws parameter sets from the posterior and runs the forward model to
#' produce an envelope of predicted D/L vs. depth, optionally overlaid with
#' observed data.
#'
#' @param mcmc_out List returned by \code{\link{run_mcmc}}.
#' @param age_model Age-depth model.
#' @param depths_cm Numeric vector of depths at which to evaluate predictions.
#' @param temp_model Temperature model for predictions.
#' @param AAR_params_fn Function taking one row of the posterior sample matrix
#'   and returning an `AAR_params` list. Defaults to extracting `log_Ae`,
#'   `Ea`, `x` from the Stage 1 posterior.
#' @param observed_data Optional data frame with columns `depth_cm` and
#'   `DL_obs` to overlay as points.
#' @param burnin Integer. Burn-in iterations to discard. Default `1000`.
#' @param n_draws Integer. Number of posterior draws. Default `200`.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point
#'   scale_y_reverse labs theme_bw
#' @importFrom dplyr group_by summarise
#' @importFrom purrr map_dfr
#' @importFrom rlang .data
#' @export
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
  post <- utils::tail(mcmc_out$samples, nrow(mcmc_out$samples) - burnin)
  idx  <- sample(nrow(post), min(n_draws, nrow(post)))

  pred_df <- purrr::map_dfr(idx, function(i) {
    params     <- AAR_params_fn(post[i, ])
    fwd        <- racemize_depth_series(depths_cm, age_model, temp_model,
                                        AAR_params = params)
    fwd$draw   <- i
    fwd
  })

  summ <- pred_df |>
    dplyr::group_by(.data$depth_cm) |>
    dplyr::summarise(median = stats::median(.data$DL),
                     lo95   = stats::quantile(.data$DL, 0.025),
                     hi95   = stats::quantile(.data$DL, 0.975),
                     .groups = "drop")

  p <- ggplot2::ggplot(summ, ggplot2::aes(x = .data$median,
                                           y = .data$depth_cm)) +
    ggplot2::geom_ribbon(ggplot2::aes(xmin = .data$lo95, xmax = .data$hi95,
                                      y = .data$depth_cm),
                         fill = "steelblue", alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(x = .data$median),
                       colour = "steelblue", linewidth = 1) +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(x = "D/L", y = "Depth (cm)",
                  title = "Posterior predictive D/L profile") +
    ggplot2::theme_bw()

  if (!is.null(observed_data)) {
    p <- p + ggplot2::geom_point(data = observed_data,
                                 ggplot2::aes(x = .data$DL_obs,
                                              y = .data$depth_cm),
                                 colour = "black", size = 1.5)
  }
  p
}
