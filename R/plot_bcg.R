#' Plot BCG
#'
#' Creates an interactive plotly object with depth and jerk profiles.
#'
#' @param prh PRH in tibble format (see read_nc())
#' @param beats Subset of PRH rows corresponding to heart beats
#'
#' @return plotly object
plot_bcg <- function(prh, beats) {
  depth_plot <- prh %>%
    plotly::plot_ly(x = ~dt,
                    y = ~p,
                    type = "scatter",
                    mode = "lines",
                    color = I("dark blue"),
                    size = I(0.75),
                    source = "bcg") %>%
    plotly::layout(yaxis = list(autorange = "reversed",
                                fixedrange = TRUE))

  jerk_plot <- prh %>%
    plotly::plot_ly(x = ~dt,
                    y = ~jerk2,
                    type = "scatter",
                    mode = "lines",
                    color = I("magenta"),
                    size = I(0.75),
                    source = "bcg") %>%
    plotly::layout(yaxis = list(fixedrange = TRUE))

  if (!is.null(beats)) {
    depth_plot <- depth_plot %>%
      plotly::add_trace(
        data = beats,
        mode = "markers",
        marker = list(
          color = "rgba(0, 0, 0, 0)",
          size = 8,
          line = list(
            color = "dark blue",
            width = 1
          )
        )
      )

    jerk_plot <- jerk_plot %>%
      plotly::add_trace(
        data = beats,
        mode = "markers",
        marker = list(
          color = "rgba(0, 0, 0, 0)",
          size = 8,
          line = list(
            color = "magenta",
            width = 1
          )
        )
      )
  }

  beats <- if (is.null(beats)) mutate(prh, bpm = NA) else beats
  bpm_plot <- beats %>%
    plotly::plot_ly(
      x = ~dt,
      y = ~bpm,
      type = "scatter",
      mode = "lines",
      color = I("dark green"),
      size = I(0.75),
      source = "bcg"
    ) %>%
    plotly::add_trace(
      mode = "markers",
      marker = list(
        color = "rgba(0, 0, 0, 0)",
        size = 8,
        line = list(
          color = "dark green",
          width = 1
        )
      )
    ) %>%
    plotly::layout(yaxis = list(fixedrange = TRUE))

  plotly::subplot(depth_plot, jerk_plot, bpm_plot,
                  nrows = 3,
                  heights = c(0.25, 0.5, 0.25),
                  shareX = TRUE,
                  titleX = FALSE,
                  titleY = TRUE) %>%
    plotly::layout(showlegend = FALSE)
}
