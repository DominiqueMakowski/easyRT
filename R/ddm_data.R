#' Simulate DDM Data
#'
#' Simulate response time data with specific parameters following a diffusion model.
#' The data are generated using \code{\link[rtdists]{rdiffusion}()}, so that check its documentation.
#'
#' @param n Number of observations.
#' @param drift Drift parameter. Typical range: [-5, 5].
#' @param bs boundary separation parameter. Typical range: [0.5, 2].
#' @param bias Bias parameter. Default to 0.5*bs.
#' @param ndt Non-decision time parameter.  Typical range: [0.1, 0.5] s.
#' @param ndt_var Variability of non-decision time. Typical range: [0, 0.2] s.
#' @param include_parameters Whether to add the parameters to the data.
#' @param ... Other arguments to pass to other functions, such as additional controls to \code{\link{ddm_traces}()}.
#'
#' @examples
#' df <- ddm_data(n = c(100, 100), drift = c(1, 0))
#' summary(df$data)
#'
#' @export
ddm_data <- function(n = 100,
                     drift = 0,
                     bs = 1,
                     bias = 0.5,
                     ndt = 0.2,
                     ndt_var = 0.1,
                     include_parameters = FALSE,
                     ...) {
  # Sanity checks
  n_groups <- max(c(length(n), length(drift), length(bs), length(bias), length(ndt)))

  n <- .sanity_check_parameter_length(n, n_groups, "n")
  drift <- .sanity_check_parameter_length(drift, n_groups, "drift")
  bs <- .sanity_check_parameter_length(bs, n_groups, "bs")
  bias <- .sanity_check_parameter_length(bias, n_groups, "bias")
  ndt <- .sanity_check_parameter_length(ndt, n_groups, "ndt")
  ndt_var <- .sanity_check_parameter_length(ndt_var, n_groups, "ndt_var")

  # Initialize data containers
  data <- data.frame()
  density <- data.frame()
  traces <- data.frame()

  for (i in 1:n_groups) {
    # Empirical data
    dat <- rtdists::rdiffusion(n[i],
      v = drift[i],
      a = bs[i],
      z = bias[i],
      t0 = ndt[i],
      st0 = ndt_var[i]
    )
    dat$condition <- i

    # Add parameters
    if (include_parameters) {
      dat$n <- n[i]
      dat$drift <- drift[i]
      dat$bs <- bs[i]
      dat$bias <- bias[i]
      dat$ndt <- ndt[i]
    }
    data <- rbind(data, dat)

    # Density
    x <- seq(min(dat$rt), max(dat$rt), length.out = 1000)
    tmp <- .get_density_lines(x, n[i],
                              v = drift[i],
                              a = bs[i],
                              z = bias[i],
                              t0 = ndt[i],
                              st0 = ndt_var[i]
    )
    tmp$condition <- i
    density <- rbind(density, tmp)

    # Traces
    tmp <- ddm_traces(drift = drift[i], bs = bs[i], bias = bias[i], ndt = ndt[i], ndt_var = ndt_var[i], ...)
    tmp$condition <- i
    traces <- rbind(traces, tmp)

  }

  # Conditions to factor
  data$condition <- as.factor(data$condition)
  density$condition <- as.factor(density$condition)
  traces$condition <- as.factor(traces$condition)

  out <-list(data = data, density = density, traces = traces)
  class(out) <- c("ddm_data", class(out))
  out
}


# Utilities ---------------------------------------------------------------
#' @keywords internal
.get_density_lines <- function(x, n, ...) {
  upper <- rtdists::ddiffusion(x, response = "upper", ...)
  lower <- rtdists::ddiffusion(x, response = "lower", ...)

  rbind(
    data.frame(x = x, y = upper / max(upper), response = "upper"),
    data.frame(x = x, y = lower / max(lower), response = "lower")
  )
}


#' @keywords internal
.sanity_check_parameter_length <- function(n, n_groups = 2, name = "n") {
  if (length(n) != n_groups) {
    if (length(n) == 1) {
      n <- rep(n, n_groups)
    } else {
      stop(paste("Each parameter should have the same length to represent different conditions. Check ", name))
    }
  }
  n
}
