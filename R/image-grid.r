#' Display a grid of color images representing microarray chips
#'
#' Visualize microarray probe intensities arranged by their physical location on
#' the array. A false color image is produced for each sample in the
#' microarray object and arranged in a grid.
#'
#' @param object \code{AffyBatch} or \code{PLMset} object
#' @param colors a vector of colors used to represent probe values
#' @param select a numeric, character or logical vector indicating samples to include
#' @param legend.label Legend label
#' @param ncol optional, number of columns in grid layout
#' @param nrow optional, number of rows in grid layout
#' @param fixed Force images to assume a fixed aspect ratio corresponding to their physical dimensions
#' @param range optional, if defined, values will be limited to the defined range
#' @param transform a function to be applied to the values prior to visualization
#' @name ma_image
#' @export

setMethod("ma_image", c(object = "AffyBatch"),
  function(object,
           colors,
           select = NULL,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           range = NULL,
           transform,
           probes) {

    if (missing(colors))       colors <- scales::brewer_pal(palette = "YlGnBu")(9)
    if (missing(legend.label)) legend.label <- "Expression"
    if (missing(transform))    transform <- log2
    if (missing(probes))       probes <- "pm"

    object <- ma_layout(object, probes, transpose = TRUE, select)

    .ma_image(object, colors, legend.label, nrow, ncol, fixed, range, transform)
})



#' @rdname ma_image
#' @export

setMethod("ma_image", c(object = "PLMset"),
  function(object,
           colors,
           select = NULL,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           range = NULL,
           transform = identity,
           probes,
           type) {

    if (missing(colors))       colors <- scales::brewer_pal(palette = "RdBu")(9)
    if (missing(legend.label)) legend.label <- "Residuals"
    if (missing(transform))    transform <- identity
    if (missing(probes))       probes <- "pm"
    if (missing(type))         type <- "resid"

    object <- ma_layout(object, probes, transpose = TRUE, select, type)
    .ma_image(object, colors, legend.label, nrow, ncol, fixed, range, transform)
})



#' @rdname ma_image
#' @export

setMethod("ma_image", c(object = "array"),
  function(object,
           colors,
           select = NULL,
           legend.label = "Values",
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           range = NULL,
           transform) {

  if (missing(colors))       colors <- scales::seq_gradient_pal()(seq(0, 1, 0.1))
  if (missing(legend.label)) legend.label <- "Values"
  if (missing(transform))    transform <- identity

  .ma_image(object, colors, legend.label, nrow, ncol, fixed, range, transform)
})



.ma_image <- function(object,
           colors,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           range = NULL,
           transform) {

  object <- transform(object)
  n <- dim(object)[3]

  dims <- layout_dims(n, nrow, ncol)
  dims <- trim_dims(n, dims[1], dims[2])

  # TODO: No reason to label 'sample'
  if (is.null(dimnames(object)[[3]]))
    dimnames(object)[[3]] <- paste0("sample", seq_len(n))

  labels <- dimnames(object)[[3]]

  if (!is.null(range)) object <- scales::squish(object, range)

  legend <- scales::cbreaks(range(object, na.rm = TRUE),
                            labels = scales::format_format())
  legend$palette <- scales::gradient_n_pal(colours = colors)

  legend$fill <- scales::cscale(legend$breaks, legend$palette)
  obj.colors  <- scales::cscale(object, legend$palette, na.value = "white")

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


  obj.raster <- lapply(obj.raster[1:prod(dims)], "%||%", grid::nullGrob())
  obj.raster <- matrix(obj.raster, nrow = dims[1], ncol = dims[2], byrow = TRUE)

  obj.labels <- lapply(labels, function(l) {
   grid::textGrob(l, name = paste0("label.", l), just = c(0.5, 0.8))
  })

  obj.labels <- lapply(obj.labels[1:prod(dims)], "%||%", grid::nullGrob())
  obj.labels <- matrix(obj.labels, nrow = dims[1], ncol = dims[2], byrow = TRUE)

  row.order <- order(rep(seq_len(dims[1]), 2))

  img.unit <- grid::unit(1, "null")
  lbl.unit <- grid::unit(2, "strheight", labels[1])

  heights <- rep(grid::unit.c(lbl.unit, img.unit), dims[1])
  widths  <- rep(grid::unit(1, "null"), dims[2])

  final.table <- gtable_matrix("image.table",
              grobs = rbind(obj.labels, obj.raster)[row.order, , drop = FALSE],
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

  grid::grid.newpage()
  grid::grid.draw(final.table)

  invisible(final.table)
}
