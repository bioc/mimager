#' Construct a grid of affyPLM residual heatmaps
#'
#' @param x PLMset object generated with affyPLM
#' @param samples character vector of samples to include
#' @param labels character vector of labels for each heatmap
#' @param sign Used signed residuals
#' @param use.log logical indicating whether residuals should be log2 transformed
#' @param ncol number of columns in grid layout
#' @param colors vector of colors to use
#' @param cex.legend Size of legend relative to grid (0 - 1)
#' @param range optional, if defined, values will be limited to the defined range

image_grid <- function(x, samples, labels, sign = FALSE, use.log = TRUE,
                       ncol, colors, cex.legend = 0.5, range) {

  stopifnot(class(x) %in% c("AffyBatch", "PLMset"))

  if (missing(samples)) samples <- sampleNames(x)
  if (missing(labels))  labels <- samples
  if (missing(ncol))    ncol <- floor(sqrt(length(samples)))
  if (cex.legend < 0 | cex.legend > 1) stop("cex.legend must be >0 and <1")

  data.array <- array_layout(x)

  if (sign)
    data.array <- sign(data.array)
  else if (use.log)
    data.array <- sign(data.array) * log2(abs(data.array) + 1)

  if (!missing(range) & !sign) {
    data.array <- scales::squish(data.array, range)
  }

  # heatmap parameters
  if (missing(colors)) colors <- scales::brewer_pal(palette = "RdBu")(9)

  if (sign) {
    r.breaks <- c(-1, 1)
    r.breaks.ext <- c(-1, 0, 1)
    heat.cols <- colors[c(1, length(colors))]
    legend.lab <- "Signed\nresiduals"
    r.breaks.lab <- c("-", "+")
  } else {
    r.breaks <- scales::pretty_breaks()(data.array)
    r.breaks.ext <- seq(min(r.breaks), max(r.breaks), 0.2)
    heat.cols <- colorRampPalette(colors)(length(r.breaks.ext) - 1)
    legend.lab <- "Residuals"

    # add greater-/less-than labels to indicate values were squished
    if (!missing(range)) {
      r.breaks.lab <- as.character(r.breaks)
      r.breaks.lab[1] <- paste("<=", r.breaks.lab[1])
      r.breaks.lab[length(r.breaks)] <- paste(">=", r.breaks.lab[length(r.breaks)])
    } else {
      r.breaks.lab <- as.character(r.breaks)
    }
  }

  # plot layout
  cells <- ceiling(length(samples) / ncol) * ncol
  lmat <- matrix(seq_len(cells), ncol = ncol, byrow = TRUE) + 1
  lmat <- cbind(lmat, 0)

  lnr <- nrow(lmat)
  lrows <- pmin(lnr, ceiling(lnr * cex.legend))
  lmat[1:lrows, ncol + 1] <- 1

  layout(lmat, width = c(rep(1, ncol(lmat) - 1), 0.075 * ncol(lmat)))

  # legend
  if (missing(range)) {
    par(mar = c(0.2, 1, 2, 1))
  } else {
    # extra space for less-/greater-than symbols
    par(mar = c(0.2, 1.5, 2, 1))
  }


  legend.mat <- matrix(r.breaks, nrow = 1)
  y <- seq_len(ncol(legend.mat))
  image(y = y, z = legend.mat, col = heat.cols, axes = FALSE)
  mtext(r.breaks.lab, 2, line = 0.1, at = y, las = 1, cex = 0.6)
  mtext(legend.lab, 3, line = 0, cex = 0.6)

  # sample images
  par(mar = c(0.25, 0.25, 1, 0.25))

  for (s in samples) {

    # flip the matrix
    plot.mat <- as.matrix(rev(as.data.frame(data.array[,,s])))

    image(plot.mat, useRaster = TRUE,
          col = heat.cols, breaks = r.breaks.ext, axes = FALSE)
    mtext(labels[match(s, samples)], 3, line = 0, cex = 0.6)
  }
}
