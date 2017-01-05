# based on ggplot2:::wrap_dims() by @hadleywickham
layout_dims <- function(n, nrow = NULL, ncol = NULL) {
  if (is.null(ncol) && is.null(nrow)) {
    rc <- grDevices::n2mfrow(n)
    nrow <- rc[2]
    ncol <- rc[1]
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }
  stopifnot(nrow * ncol >= n)

  c(nrow, ncol)
}

# reduce nrow/ncol to prevent empty rows/columns in grid layout
trim_dims <- function(n, nrow, ncol) {
  i <- seq_len(n)[1:prod(nrow, ncol)]
  m <- matrix(i, nrow, ncol, byrow = TRUE)

  nas <- apply(m, c(1, 2), function(x) sum(is.na(x)))
  m <- m[!rowSums(nas) == ncol(m), !colSums(nas) == nrow(m), drop = FALSE]
  dim(m)
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# convert numeric array to an array of colors based on the palette function
# equivalent to scales::cscale(array, palette)
scale_colors <- function(x, palette, na.value = "#FFFFFF") {
  stopifnot(is.array(x))
  dims <- dim(x)
  names <- dimnames(x)
  x <- as.numeric(x)
  x <- scales::rescale(x)
  uniq <- unique(x)
  pal <- palette(uniq)
  scaled <- pal[match(x, uniq)]
  scaled <- replace(scaled, is.na(scaled), na.value)
  array(scaled, dim = dims, dimnames = names)
}

# list of parameters required to build legend grob
train_legend <- function(x, colors) {
    rng <- range(x, na.rm = TRUE)
    legend <- scales::cbreaks(rng, labels = scales::format_format())
    legend$palette <- scales::gradient_n_pal(colours = colors)
    legend$fill <- scales::cscale(legend$breaks, legend$palette)
    return(legend)
}

# winsorize values to a specified range or percentile
trim_values <- function(x, trim) {
  trim <- check_trim(trim)

  # convert percentile to a range
  if (length(trim) == 1) {
    percentiles <- c(trim, 1 - trim)
    trim <- stats::quantile(x, percentiles, na.rm = TRUE, names = FALSE)
  }
  scales::squish(x, trim)
}
