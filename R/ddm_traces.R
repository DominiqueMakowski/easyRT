#' Plot DDM Data
#'
#' Plot data.
#'
#' @param n_traces Number of traces.
#' @param time_max Span of the x-axis in seconds.
#' @param step Step size in seconds.
#' @param ... Not used for now.
#' @inheritParams ddm_data
#'
#' @examples
#'
#' data_traces <- ddm_traces(n_traces = 5)
#' data_traces
#'
#' @importFrom rlang .data
#' @importFrom stats rnorm runif
#' @importFrom utils head
#' @export
ddm_traces <- function(n_traces=5, time_max=2, step=0.01, drift = 0, bs = 1, bias = 0.5, ndt = 0.2, ndt_var = 0.1, ...) {

  # Thanks to https://github.com/rtdists/rtdists/issues/18
  traces <- data.frame()
  for(i in 1:n_traces) {

    # Compute variable NDT
    current_ndt <- runif(1, ndt, ndt + ndt_var)

    tmp <- data.frame(
      y = cumsum(c(0, rnorm(
        n = time_max * 1/step,
        mean = drift * step,  # mean drift for 1 second
        sd = 1 * sqrt(step) # variance grows by sqrt(stepsize)
      ))) + bias * bs,  # need to add bias * boundary to start at start point
      x = seq(0, time_max, by = step) + current_ndt,  # Add ndt
      iter = as.character(i)
    )

    # Pad at boundaries
    if (any(tmp$y >= bs) & any(tmp$y <= 0)) {
      if (which(tmp$y >= bs)[1] < which(tmp$y <= 0)[1]) {
        tmp$y[which(tmp$y >= bs)[1]:nrow(tmp)] <- bs
      } else {
        tmp$y[which(tmp$y <= 0)[1]:nrow(tmp)] <- 0
      }
    } else if (any(tmp$y >= bs)) {
      tmp$y[which(tmp$y >= bs)[1]:nrow(tmp)] <- bs
    } else if (any(tmp$y <= 0)) {
      tmp$y[which(tmp$y <= 0)[1]:nrow(tmp)] <- 0
    }

    traces <- rbind(traces, tmp)
  }

  traces
}

