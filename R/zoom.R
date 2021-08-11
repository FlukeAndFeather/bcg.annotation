#' Zoom into PRH
#'
#' @param prh PRH in tibble format (see read_nc())
#' @param range Time range of zoomed region (POSIXct vector length 2)
#' @param maxpts Maximum number of points to decimate to (10,000 by default)
#'
#' @return Zoomed and decimated PRH
zoom_prh <- function(prh, range = NULL, maxpts = 1e4) {
  if (!inherits(prh, "data.frame"))
    return(NULL)

  if (!is.null(range)) {
    prh <- filter(prh, between(dt, range[1], range[2]))
  }

  npts <- nrow(prh)
  if (npts > maxpts) {
    prh <- slice(prh, round(seq(1, npts, length.out = maxpts)))
  }
  prh
}

#' Zoom into heart beats
#'
#' @param beats Heart beats tibble (subset of PRH)
#' @param range Time range of zoomed region (POSIXct vector length 2)
#'
#' @return Zoomed beats tibble
zoom_beats <- function(beats, range = NULL) {
  if (!is.null(beats) && !is.null(range)) {
    beats <- filter(beats, between(dt, range[1], range[2]))
    if (nrow(beats) == 0) {
      return(NULL)
    }
  }

  beats
}
