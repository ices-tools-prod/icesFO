get_map_breaks <- function(value, min_breaks = 9) {
  # calculate breaks based on max value
  lmax <- log(max(value), 10)
  upper_int <- findInterval(lmax %% 1, log(c(1, 2, 5, 10), 10))
  upper <- c(1, 2, 5, 10)[1:(upper_int + 1)]

  breaks <-
    c(0,
      c(outer(c(1, 2, 5), pmin(0, (floor(lmax) - floor(min_breaks/4))):floor(lmax), function(x, y) x*10^y)),
      upper * 10^(ceiling(lmax)))

  # how small is the smallest number (in magnitude)
  decimals <- -1 * min(log(abs(breaks)[breaks != 0], 10))
  # set digits to be the 1st sig fig of the smallest if smallest < 1, otherwise 0
  digits <- pmax(0, decimals)

  # set up breaks - trim to max value
  maxvalue <- max(value, na.rm = TRUE)
  breaks <- c(breaks[breaks < maxvalue], maxvalue)
  #col <- col[1:(length(breaks) - 1)]

  # set up breaks - trim to min value
  minvalue <- min(value, na.rm = TRUE) - 1e-9
  breaks <- c(minvalue, breaks[breaks > minvalue])
  #col <- col[length(col) - length(breaks):1 + 1]

  ncol <- length(breaks) - 1
  fbreaks <- formatC(breaks, format = "f", digits = digits)
  fbreaks[1] <- ">0"

  # cut into bins
  out <- cut(value, rev(breaks))

  # add bin frequency to label
  labels <- paste0(fbreaks[(ncol):1], " - ", fbreaks[(ncol + 1):2])
  labels <- paste0(labels, " (n = ", rev(table(out)), ")")

  levels(out) <- rev(labels)
  out
}
