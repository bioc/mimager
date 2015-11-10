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

setMethod("ma_image", c(object = "AffyBatch"),
  function(object, colors = NULL, legend.label = NULL, nrow = NULL, ncol = NULL, fixed = FALSE, range = NULL, transform = log2, ...) {
    if (is.null(colors)) colors <- scales::brewer_pal(palette = "YlGnBu")(9)
    if (is.null(legend.label)) legend.label <- "Expression"

    object <- ma_layout(object)
    object <- transform(object)
    .image_grid(object, colors, legend.label, nrow, ncol, fixed, range, ...)
})

setMethod("ma_image", c(object = "PLMset"),
  function(object, colors = NULL, legend.label = NULL, nrow = NULL, ncol = NULL, fixed = FALSE, range = NULL, transform = identity, ...) {
    if (is.null(colors)) colors <- scales::brewer_pal(palette = "RdBu")(9)
    if (missing(legend.label)) legend.label <- "Residuals"

    object <- ma_layout(object)
    object <- transform(object)
    .image_grid(object, colors, legend.label, nrow, ncol, fixed, range, ...)
})

.image_grid <- function(object, colors, legend.label, nrow, ncol, fixed, range, ...) {
  stopifnot(class(object) == "array")

  n <- dim(object)[3]
  dims <- layout_dims(n, nrow, ncol)

  if (is.null(dimnames(object)[[3]]))
    dimnames(object)[[3]] <- paste0("sample", seq_len(n))

  labels <- dimnames(object)[[3]]

  if (!is.null(range)) object <- scales::squish(object, range)

  legend <- scales::cbreaks(range(object, na.rm = TRUE))
  legend$fill <- scales::cscale(legend$breaks, scales::div_gradient_pal())

  obj.colors <- scales::cscale(object, scales::div_gradient_pal(), na.value = "white")

  obj.raster <- lapply(labels, function(l) {
    grid::rasterGrob(obj.colors[,,l],
                     interpolate = FALSE,
                     name = paste0("image.", l))
  })

  if (!fixed) {
    x.rng <- range(seq_len(nrow(object)) / nrow(object))
    y.rng <- range(seq_len(ncol(object)) / ncol(object))

    obj.raster <- lapply(obj.raster, grid::editGrob,
           x = grid::unit(mean(x.rng), "native"),
           y = grid::unit(mean(y.rng), "native"),
           width = grid::unit(diff(x.rng), "native"),
           height = grid::unit(diff(y.rng), "native"))
  }

  obj.raster <- matrix(obj.raster, nrow = dims[1], ncol = dims[2], byrow = TRUE)

  obj.labels <- lapply(labels, function(l) {
   grid::textGrob(l, name = paste0("label.", l), just = c(0.5, 0.8))
  })

  obj.labels <- matrix(obj.labels, nrow = dims[1], ncol = dims[2], byrow = TRUE)

  row.order <- order(rep(seq_len(dims[1]), 2))

  img.unit <- grid::unit(1, "null")
  lbl.unit <- grid::unit(2, "strheight", labels[1])

  heights <- rep(grid::unit.c(lbl.unit, img.unit), dims[1])
  widths <- rep(grid::unit(1, "null"), dims[2])

  final.table <- gtable_matrix("image.table",
                               grobs = rbind(obj.labels, obj.raster)[row.order, ],
                               heights = heights, widths = widths)

  # legend
  legend.table <- build_legend(legend$breaks, legend$fill, legend$labels, legend.label)

  final.table <- gtable_add_cols(final.table, gtable_width(legend.table))

  final.table <- gtable_add_grob(final.table, legend.table,
                  t = 2,
                  b = nrow(final.table),
                  l = ncol(final.table),
                  r = ncol(final.table))

  final.table <- gtable_add_col_space(final.table, unit(0.5, "lines"))
  final.table <- gtable_add_padding(final.table, unit(0.5, "lines"))

  grid.newpage()
  grid.draw(final.table)
}
