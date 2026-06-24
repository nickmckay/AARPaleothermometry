#' Default AAR kinetic parameters
#'
#' A named list of Arrhenius and power-law constants used as defaults
#' throughout the package. Values are from Kaufman (2000) for isoleucine
#' epimerization and have not yet been calibrated to site-specific data.
#'
#' @format A named list with four elements:
#' \describe{
#'   \item{Ae}{Pre-exponential (frequency) factor: `exp(42.12)`.}
#'   \item{Ea}{Activation energy in kcal/mol: `31.5`.}
#'   \item{R}{Gas constant in kcal K\eqn{^{-1}} mol\eqn{^{-1}}: `0.001987`.}
#'   \item{x}{Power-law exponent for D/L linearization: `3`.}
#' }
#' @export
default_AAR_params <- list(
  Ae = exp(42.12),
  Ea = 31.5,
  R  = 0.001987,
  x  = 3
)


#' Arrhenius rate constant for amino acid racemization
#'
#' Computes the temperature-dependent rate constant \eqn{k} from the
#' Arrhenius equation:
#' \deqn{\ln(k) = -\frac{E_a}{R} T^{-1} + \ln(A_e)}
#'
#' @param Ae Pre-exponential (frequency) factor. Default `exp(42.12)`
#'   (Kaufman 2000).
#' @param Ea Activation energy in kcal/mol. Default `31.5`.
#' @param Temp Temperature in Kelvin. Default `278` (≈ 5°C).
#' @param R Gas constant in kcal K\eqn{^{-1}} mol\eqn{^{-1}}. Default
#'   `0.001987`.
#'
#' @return Numeric vector of rate constants \eqn{k} at the supplied
#'   temperature(s).
#'
#' @examples
#' # Rate constant at 5°C
#' arrhenius(Temp = 278)
#'
#' # Rate constant across a temperature range
#' T_K <- seq(273, 298, by = 1)
#' plot(T_K - 273, arrhenius(Temp = T_K), type = "l",
#'      xlab = "Temperature (°C)", ylab = "k")
#'
#' @export
arrhenius <- function(Ae = exp(42.12), Ea = 31.5, Temp = 278, R = 0.001987) {
  lnk <- -(Ea / R) * Temp^-1 + log(Ae)
  exp(lnk)
}


#' Incremental racemization in power-law space
#'
#' Computes the increment in \eqn{(D/L)^x} accumulated over one timestep at
#' rate constant \code{kt}. Racemization is integrated in power-law
#' \eqn{(D/L)^x} space (the Bada linearization) rather than D/L space,
#' because \eqn{(D/L)^x} increases linearly with \eqn{k_T t} and avoids the
#' sigmoidal curvature that makes D/L accumulation rate-dependent on the
#' current value.
#'
#' @param kt Numeric. Rate constant from \code{\link{arrhenius}}.
#' @param delta.yr Numeric. Timestep in years.
#'
#' @return Numeric vector of incremental \eqn{(D/L)^x} values.
#'
#' @examples
#' kt <- arrhenius(Temp = 278)
#' dRacPL(kt, delta.yr = 10)
#'
#' @export
dRacPL <- function(kt, delta.yr) {
  kt * delta.yr
}


#' Simulate D/L accumulation over a full temperature history
#'
#' Integrates racemization from the oldest to the youngest point in a
#' temperature time series, splitting each timestep into summer and winter
#' halves. Returns the final D/L ratio after back-transforming from
#' \eqn{(D/L)^x} space.
#'
#' @param age Numeric vector of ages (ka BP). Sorted old-to-young internally.
#' @param sumT Numeric vector of summer half-year temperatures (°C),
#'   same length as \code{age}.
#' @param winT Numeric vector of winter half-year temperatures (°C),
#'   same length as \code{age}.
#' @param x Power-law exponent. Default `3`.
#' @param dT Timestep in years.
#' @param startingDL Starting D/L value. Default `0` (unracemized).
#' @param scenario Character label stored in the output. Default `NA`.
#' @param ... Additional arguments passed to \code{\link{arrhenius}}.
#'
#' @return A one-row data frame with columns \code{age} (maximum input age),
#'   \code{DL}, and \code{scenario}.
#'
#' @seealso \code{\link{racemizeSample}} for a version that also applies
#'   sediment temperature damping; \code{\link{dampSedTemps}} for the
#'   damping model.
#'
#' @examples
#' age  <- seq(0, 10, by = 0.01)
#' sumT <- rep(15, length(age))
#' winT <- rep(4,  length(age))
#' racemize(age, sumT, winT, x = 3, dT = 10)
#'
#' @export
racemize <- function(age, sumT, winT, x, dT, startingDL = 0,
                     scenario = NA, ...) {
  # Sort old to young so racemization accumulates forward in time
  as   <- sort(age, decreasing = TRUE, index.return = TRUE)
  age  <- age[as$ix]
  sumT <- sumT[as$ix]
  winT <- winT[as$ix]

  Rx <- startingDL
  for (i in 2:length(age)) {
    drSum <- dRacPL(kt = arrhenius(Temp = sumT[i] + 273, ...),
                    delta.yr = dT / 2)
    drWin <- dRacPL(kt = arrhenius(Temp = winT[i] + 273, ...),
                    delta.yr = dT / 2)
    Rx <- Rx + drSum + drWin
  }

  # Rx is accumulated in (D/L)^x space; back-transform to D/L for output
  data.frame(age = max(age), DL = Rx^(1 / x), scenario = scenario)
}


#' Apply sediment burial temperature damping
#'
#' As sediment is buried deeper, the seasonal air temperature signal
#' attenuates: summer and winter sediment temperatures converge toward the
#' mean annual. This function models that process as a linear interpolation
#' from full seasonal amplitude (fresh surface sediment, `weight1 = startDamp`)
#' to attenuated signal (deeply buried sediment, `weight1 = endDamp`) over
#' `dampTime` ka. The physical motivation is thermal diffusion in the
#' sediment column.
#'
#' @param sumT Numeric vector of summer half-year air temperatures (°C).
#' @param winT Numeric vector of winter half-year air temperatures (°C).
#' @param age Numeric vector of ages (ka BP), same length as \code{sumT} and
#'   \code{winT}. Sorted old-to-young internally.
#' @param startDamp Weight on the same-season temperature at the surface
#'   (most recent). Default `1` (no attenuation at surface).
#' @param endDamp Weight on the same-season temperature at full burial.
#'   Default `0.5` (summer and winter sediment temperatures equal at depth).
#' @param dampTime Time scale over which damping is applied (ka). Default `8`.
#'
#' @return A data frame with columns \code{age}, \code{sumT}, \code{winT},
#'   \code{sumTsed} (dampened summer sediment temperature), and \code{winTsed}
#'   (dampened winter sediment temperature).
#'
#' @seealso \code{\link{racemizeSample}} which calls this function internally;
#'   \code{\link{diffusion_attenuation}} for the physically motivated
#'   exponential depth-based alternative used in the depth-series model.
#'
#' @examples
#' age  <- seq(0, 10, by = 0.01)
#' sumT <- rep(15, length(age))
#' winT <- rep(4,  length(age))
#' d <- dampSedTemps(sumT, winT, age)
#' head(d)
#'
#' @export
dampSedTemps <- function(sumT, winT, age,
                         startDamp = 1, endDamp = 0.5, dampTime = 8) {
  as   <- sort(age, decreasing = TRUE, index.return = TRUE)
  age  <- age[as$ix]
  sumT <- sumT[as$ix]
  winT <- winT[as$ix]

  filterStart <- max(age, na.rm = TRUE)
  ffrac       <- (filterStart - age) / dampTime
  tf          <- endDamp * ffrac + startDamp * (1 - ffrac)
  tf[tf < endDamp] <- endDamp

  weight1 <- tf
  weight0 <- 1 - tf

  data.frame(
    age     = age,
    sumT    = sumT,
    winT    = winT,
    sumTsed = sumT * weight1 + winT * weight0,
    winTsed = winT * weight1 + sumT * weight0
  )
}


#' Simulate racemization for a single downcore sample (with burial damping)
#'
#' Full pipeline for one sample: selects the portion of the temperature
#' history from deposition to present, applies \code{\link{dampSedTemps}} to
#' attenuate the seasonal signal with burial depth, then integrates
#' racemization via \code{\link{racemize}}.
#'
#' @param sampleAge Age of the sample (ka BP). Only temperature history at or
#'   older than this age is used.
#' @param ageVec Numeric vector of ages spanning the full temperature history
#'   (ka BP).
#' @param sumT Numeric vector of summer half-year temperatures (°C), same
#'   length as \code{ageVec}.
#' @param winT Numeric vector of winter half-year temperatures (°C), same
#'   length as \code{ageVec}.
#' @param x Power-law exponent.
#' @param dT Timestep in years.
#' @param startingDL Starting D/L value. Default `0`.
#' @param scenario Character label for the output. Default `NA`.
#' @param startDamp,endDamp,dampTime Passed to \code{\link{dampSedTemps}}.
#' @param ... Additional arguments passed to \code{\link{arrhenius}}.
#'
#' @return A one-row data frame with columns \code{age}, \code{DL}, and
#'   \code{scenario}.
#'
#' @seealso \code{\link{racemize}}, \code{\link{dampSedTemps}},
#'   \code{\link{racemize_depth_series}} for the more physically rigorous
#'   depth-series workflow.
#'
#' @examples
#' age  <- seq(0, 10, by = 0.01)
#' sumT <- rep(15, length(age))
#' winT <- rep(4,  length(age))
#' racemizeSample(sampleAge = 5, ageVec = age, sumT = sumT, winT = winT,
#'                x = 3, dT = 10)
#'
#' @export
racemizeSample <- function(sampleAge, ageVec, sumT, winT, x, dT,
                           startingDL = 0, scenario = NA,
                           startDamp = 1, endDamp = 0.5, dampTime = 10,
                           ...) {
  ageIndex <- which(ageVec <= sampleAge)
  sAge     <- ageVec[ageIndex]
  sSumT    <- sumT[ageIndex]
  sWinT    <- winT[ageIndex]

  dampened <- dampSedTemps(sumT = sSumT, winT = sWinT, age = sAge,
                           startDamp = startDamp, endDamp = endDamp,
                           dampTime = dampTime)

  racemize(age = dampened$age, sumT = dampened$sumTsed,
           winT = dampened$winTsed, x = x, dT = dT,
           startingDL = startingDL, scenario = scenario, ...)
}


#' Plot temperature inputs for a damped sediment record
#'
#' Convenience plotting function for the output of \code{\link{dampSedTemps}}.
#' Pivots the data frame to long format and draws air and sediment temperature
#' histories.
#'
#' @param dampened Data frame returned by \code{\link{dampSedTemps}}.
#'
#' @return A ggplot object.
#'
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom ggplot2 ggplot aes geom_line theme_bw scale_color_manual
#'   scale_linetype_manual
#' @importFrom rlang .data
#' @export
plotInputs <- function(dampened) {
  toPlot <- tidyr::pivot_longer(
    dampened,
    tidyr::starts_with("sumT") | tidyr::starts_with("winT") | tidyr::starts_with("annT"),
    values_to = "Temperature", names_to = "scenario"
  )

  ggplot2::ggplot(toPlot) +
    ggplot2::geom_line(ggplot2::aes(x = .data$age, y = .data$Temperature,
                                    color = .data$scenario,
                                    linetype = .data$scenario)) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values = c("black", "black", "red", "red",
                                           "blue", "blue")) +
    ggplot2::scale_linetype_manual(values = c(1, 2, 1, 2, 1, 2))
}
