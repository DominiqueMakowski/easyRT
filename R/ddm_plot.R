#' Plot DDM Data
#'
#' Plot data.
#'
#' @param data Either object from \code{\link{ddm_data}()}) or dataframe containing
#'   columns "rt", "response" (lower or upper) and "condition".
#' @param density Data for density lines (typically obtained from \code{\link{ddm_data}()}).
#' @param traces Data for trace lines (typically obtained from \code{\link{ddm_data}()}).
#' @param xlim X-axis limits.
#' @param theme Can be \code{NULL} (useful for customizing) or "default".
#' @param trace_alpha,hist_alpha Alpha of trace lines or histograms.
#' @param trace_linewidth,density_linewidth Alpha of trace lines or histograms.
#' @param ... Other arguments to pass to other functions, such as additional controls to \code{\link[graphics]{hist}()}.
#'
#' @examples
#' library(ggplot2)
#' library(patchwork)
#'
#' # Simulate data
#' sim <- ddm_data(n=1000, drift = c(1, 0), n_traces = 20)
#'
#' ddm_plot(sim, xlim = c(0, 1))
#'
#' # Customize
#' data <- sim$data
#' density <- sim$density
#' traces <- sim$traces
#'
#' ddm_plot_upper(data, density = density, theme = NULL) /
#'   ddm_plot_traces(traces, theme = NULL) /
#'   ddm_plot_lower(data, density = density, theme = NULL)
#'
#' @export
ddm_plot <- function(data, ...) {
  UseMethod("ddm_plot")
}


# Definitions -------------------------------------------------------------

#' @export
ddm_plot.ddm_data <- function(data, ...) {
  ddm_plot(data$data, data$density, data$traces, ...)
}

#' @export
ddm_plot.data.frame <- function(data, density = NULL, traces = NULL, xlim = c(0, 2), ...) {

  data_hist <- .ddm_histdata(data, ...)

  upper <-  ddm_plot_upper(data_hist, density = density, xlim = xlim, ...)
  lower <-  ddm_plot_lower(data_hist, density = density, xlim = xlim, ...)

  if(is.null(traces)) {
    p <- patchwork::wrap_plots(upper, lower, nrow = 2) + patchwork::plot_layout(guides = "collect")
  } else {
    p_traces <- ddm_plot_traces(traces, xlim = xlim, ...)
    p <- patchwork::wrap_plots(upper, p_traces, lower, nrow = 3) + patchwork::plot_layout(guides = "collect")
  }
  p
}

# ddm_plot_distributions -------------------------------------------------------

#' @rdname ddm_plot
#' @export
ddm_plot_upper <- function(data, density = NULL, xlim = c(0, 2), hist_alpha = 1/3, density_linewidth = 1, theme = "default", ...) {
  .ddm_plot_distributions(data, side = "upper", density = density, xlim = xlim, hist_alpha = hist_alpha, density_linewidth = density_linewidth, theme = theme, ...)
}

#' @rdname ddm_plot
#' @export
ddm_plot_lower <- function(data, density = NULL, xlim = c(0, 2), hist_alpha = 1/3, density_linewidth = 1, theme = "default", ...) {
  .ddm_plot_distributions(data, side = "lower", density = density,  xlim = xlim, hist_alpha = hist_alpha, density_linewidth = density_linewidth, theme = theme, ...)
}

#' @importFrom ggplot2 ggplot geom_rect aes scale_y_continuous labs coord_cartesian geom_line theme_minimal element_blank theme
#' @keywords internal
.ddm_plot_distributions <- function(data, side = "upper", density = NULL, xlim = c(0, 2), hist_alpha = 1/3, density_linewidth = 1, theme = "default", ...) {

  # Histograms
  if(!all(c("breaks", "xmin", "counts") %in% names(data))) {
    data <- .ddm_histdata(data, ...)
  }

  p <- data[data$response == side, ] |>
    ggplot() +
    geom_rect(aes(
      xmin = .data$xmin,
      xmax =  .data$breaks,
      ymin = 0, ymax =  .data$counts,
      fill =  .data$condition
    ), alpha = hist_alpha) +
    scale_y_continuous(expand = c(0.01, 0.01))

  if(side == "upper") {
    p <- p +
      labs(y = "Upper Bound", x = "RT (s)") +
      coord_cartesian(xlim = xlim)
  } else {
    p <- p +
      labs(y = "Lower Bound", x = "RT (s)") +
      coord_cartesian(xlim = xlim, ylim = c(max(data$counts), 0))
  }

  # Density plots

  # Normalize the density line to the histogram height
  if (!is.null(density)) {
    for (c in unique(data$condition)) {
      for (r in unique(data$response)) {
        y <- density[density$condition == c & density$response == r, "y"]
        density[density$condition == c & density$response == r, "y"] <- y * max(data[data$condition == c & data$response == r, "counts"])
      }
    }
    p <- p +
      geom_line(data = density[density$response == side, ],
                aes(x = .data$x, y = .data$y, color = .data$condition),
                linewidth = density_linewidth)
  }

  # Add theme
  if(!is.null(theme) && theme == "default") {
    p <- p +
      theme_minimal()
    if(side == "upper") {
      p <- p +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none"
      )
    } else {
      p <- p +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none"
        )
    }
  }

  p
}






#' @importFrom graphics hist
#' @keywords internal
.ddm_histdata <- function(data, rt = "rt", response = "response", condition = "condition", breaks = 50, ...) {
  data$response <- as.factor(data[[response]])
  levels(data$response) <- rev(levels(data$response))
  data$condition <- as.factor(data[[condition]])

  breaks <- hist(data[[rt]], plot = FALSE, breaks = breaks)$breaks

  data_hist <- data.frame()
  for (c in levels(data$condition)) {
    for (r in unique(data$response)) {
      dat <- data[data$condition == c & data$response == r, ]
      z <- graphics::hist(dat[[rt]], plot = FALSE, breaks = breaks)
      data_hist <- rbind(
        data_hist,
        data.frame(
          breaks = z$breaks[2:length(z$breaks)],
          counts = z$counts,
          xmin = z$breaks[1:length(z$breaks) - 1],
          condition = c,
          response = r
        )
      )
    }
  }

  data_hist$response <- as.factor(data_hist[[response]])
  data_hist$xmax <- max(data[[rt]])
  levels(data_hist$response) <- rev(levels(data_hist$response))
  # Reorder levels to same as original
  data_hist$condition <- factor(data_hist$condition, levels=levels(data$condition))
  data_hist
}



# ddm_plot_traces --------------------------------------------------------

#' @importFrom rlang .data
#' @importFrom ggplot2 geom_path
#' @rdname ddm_plot
#' @export
ddm_plot_traces <- function(traces, trace_alpha = 0.8, trace_linewidth = 0.5, xlim = c(0, 2), theme = "default", ...) {

  # Normalize boundaries for each condition
  for(c in unique(traces$condition)) {
    traces[traces$condition == c, "y"] <- traces[traces$condition == c, "y"] / max(traces[traces$condition == c, "y"])
  }

  # Cut duplicates at boundaries
  data <- data.frame()
  for(c in unique(traces$condition)) {
    tmp <-  traces[traces$condition == c, ]
    data <- rbind(data, .ddm_traces_cut(tmp, bs = max(tmp$y)))
  }

  data$group <- paste(data$condition, data$iter)

  p <- data |>
    ggplot(aes(x = .data$x, y = .data$y, color = .data$condition, group = .data$group)) +
    geom_path(alpha = trace_alpha, linewidth = trace_linewidth) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(y = "Evidence", x = "rt") +
    coord_cartesian(xlim = xlim)

  # Add theme
  if(!is.null(theme) && theme == "default") {
    p <- p +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()
      )
  }
  p
}


#' @keywords internal
.ddm_traces_cut <- function(x, bs = 1) {
  mask <- (x$y[-1] == head(x$y, -1)) & x$y[-1] %in% c(0, bs)
  x[!c(FALSE, mask), ]
}

