# Construct a grid of affyPLM residual heatmaps
#
# x: PLMset object generated with affyPLM
# samples: character vector of samples to include
# labels: character vector of labels for each heatmap
# sign: Used signed residuals
# use.log: logical indicating whether residuals should be log2 transformed
# ncol: number of columns in grid layout
# colors: vector of colors to use 
# cex.legend: Size of legend relative to grid (0 - 1)

image_grid <- function(x, samples, labels, sign = FALSE, use.log = TRUE, 
                       ncol, colors, cex.legend = 0.5) {
  stopifnot(class(x) == "PLMset")

  require(affy)  
  require(affyPLM)
  require(scales)
  
  if (missing(samples)) samples <- sampleNames(x)
  if (missing(labels))  labels <- samples
  if (missing(ncol))    ncol <- floor(sqrt(length(samples)))
  if (cex.legend < 0 | cex.legend > 1) stop("cex.legend must be >0 and <1")
  
  resids <- residuals(x)[["PM.resid"]][, samples]
  if (sign)
    resids <- sign(resids)
  else if (use.log)
    resids <- sign(resids) * log2(abs(resids) + 1)
  
  # heatmap parameters
  if (missing(colors)) colors <- brewer_pal(palette = "RdBu")(9)
  
  if (sign) {
    r.breaks <- c(-1, 1)
    r.breaks.ext <- c(-1, 0, 1)
    heat.cols <- colors[c(1, length(colors))]
    legend.lab <- "Signed\nresiduals"
  } else {
    r.breaks <- pretty_breaks()(resids)
    r.breaks.ext <- seq(min(r.breaks), max(r.breaks), 0.2)  
    heat.cols <- colorRampPalette(colors)(length(r.breaks.ext) - 1)
    legend.lab <- "Residuals"
  }
  
  # image matrix
  nr <-  x@nrow
  nc <-  x@ncol
  rmat <- matrix(nrow = nr, ncol = nc)

  # probe coordinates
  pm.index <- indexProbes(x, which = "pm", row.names(coefs(x)))

  xycoor <- lapply(pm.index, indices2xy, nc = nc)
  xycoor <- do.call("rbind", xycoor)
  xycoor <- rbind(xycoor, cbind(x = xycoor[, "x"], y = xycoor[, "y"] + 1))
  
  # plot layout
  cells <- ceiling(length(samples) / ncol) * ncol
  lmat <- matrix(seq_len(cells), ncol = ncol, byrow = TRUE) + 1
  lmat <- cbind(lmat, 0)
  
  lnr <- nrow(lmat)
  lrows <- pmin(lnr, ceiling(lnr * cex.legend))
  lmat[1:lrows, ncol + 1] <- 1

  layout(lmat, width = c(rep(1, ncol(lmat) - 1), 0.075 * ncol(lmat)))

  # legend
  par(mar = c(0.2, 1, 2, 1))

  legend.mat <- matrix(r.breaks, nrow = 1)
  y <- seq_len(ncol(legend.mat))
  image(y = y, z = legend.mat, col = heat.cols, axes = FALSE)
  mtext(r.breaks, 2, line = 0.1, at = y, las = 1, cex = 0.6)
  mtext(legend.lab, 3, line = 0, cex = 0.6)

  # sample images
  par(mar = c(0.25, 0.25, 1, 0.25))

  for (s in samples) {
    rmat[xycoor] <- resids[, s]
    
    # flip the matrix
    rmat <- as.matrix(rev(as.data.frame(rmat)))
    
    image(rmat, useRaster = TRUE, col = heat.cols, breaks = r.breaks.ext, axes = FALSE)
    mtext(labels[match(s, samples)], 3, line = 0, cex = 0.6)
  }
}
