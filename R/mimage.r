#' Microarray image
#'
#' Visualize microarray probe intensities arranged by their physical location on
#' the array. A false color image is produced for each sample in the microarray
#' object and arranged in a grid.
#'
#' @section Trimming:
#'
#' By default, a 98\% winsorization is performed prior to visualization, pulling
#' in values outside of the 1st and 99th percentiles to their respective
#' endpoints. This can modified using the \code{trim} argument to provide either
#' a new percentile, or a range of 2 values defining the min/max of the trimmed
#' endpoints. Set \code{trim = 0} to avoid trimming altogether.
#'
#' @section Empty Rows:
#'
#' By virtue of platform design, unindexed probes or The presence of rows that
#' are comprised entirely (or mostly) of missing values can create undesirable
#' rasterization artifacts in the array images. To avoid this, empty rows are,
#' by default, filled with values from a neighboring row. The threshold for what
#' constitutes an empty row can be tweaked with the \code{empty.thresh}
#' argument. Such empty rows are the result of either platform design, unindexed
#' probes or probe selection (e.g., including only \code{PM} probes).
#'
#' @template probes
#' @inheritParams marray
#' @param colors a vector of colors used to represent probe values
#' @param legend.label Legend label
#' @param ncol optional, number of columns in grid layout
#' @param nrow optional, number of rows in grid layout
#' @param fixed Force images to assume a fixed aspect ratio corresponding to
#'   their physical dimensions
#' @param trim a percentile (default = \code{0.02}) or range or 2 values see
#'   \bold{trimming} section for details
#' @param empty.rows Should empty rows be filled with values from neighboring
#'   rows (the default, \code{"fill"}), should they be dropped
#'   (\code{"drop"}) entirely, or should they be left alone (\code{"ignore"})
#' @param empty.thresh what proportion of features must be missing from a row to
#'   consider that row empty
#' @param transform a function to be applied to the values prior to visualizatio
#'
#' @examples
#' # standard array visualization
#' mimage(iris3)
#'
#' # microarray visualization
#' if (require(affydata, quietly = TRUE)) {
#'   data("Dilution", package = "affydata")
#'   mimage(Dilution, select = c("20A", "10A"))
#' }
#'
#' @name mimage
NULL

#' @rdname mimage
#' @export
setMethod("mimage", c(object = "AffyBatch"),
  function(object,
           colors,
           select = NULL,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           empty.rows = "fill",
           empty.thresh = 0.6,
           transform,
           trim = 0.01,
           probes) {

    if (missing(colors))
      colors <- scales::brewer_pal(palette = "YlGnBu")(9)
    if (missing(legend.label)) legend.label <- "Expression"
    if (missing(transform))    transform <- log2
    if (missing(probes))       probes <- "pm"

    object <- marray(object, probes, transpose = TRUE, select)
    if (empty.rows == "fill") object <- fill_rows(object, empty.thresh)

    .mimage(object, colors, legend.label, nrow, ncol, fixed, transform, trim)
})


#' @rdname mimage
#' @export
setMethod("mimage", c(object = "PLMset"),
  function(object,
           colors,
           select = NULL,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           empty.rows = "fill",
           empty.thresh = 0.6,
           transform = identity,
           trim = 0.01,
           probes,
           type) {

    if (missing(colors))       colors <- scales::brewer_pal(palette = "RdBu")(9)
    if (missing(legend.label)) legend.label <- "Residuals"
    if (missing(transform))    transform <- identity
    if (missing(probes))       probes <- "pm"
    if (missing(type))         type <- "residuals"

    object <- marray(object, probes, transpose = TRUE, select, type)
    if (empty.rows == "fill") object <- fill_rows(object, empty.thresh)

    .mimage(object, colors, legend.label, nrow, ncol, fixed, transform, trim)
})


#' @rdname mimage
#' @export
setMethod("mimage", c(object = "FeatureSet"),
  function(object,
           colors,
           select = NULL,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           empty.rows = "fill",
           empty.thresh = 0.6,
           transform,
           trim = 0.01,
           probes) {

    if (missing(colors))
      colors <- scales::brewer_pal(palette = "YlGnBu")(9)
    if (missing(legend.label)) legend.label <- "Expression"
    if (missing(transform))    transform <- log2
    if (missing(probes))       probes <- "pm"

    object <- marray(object, probes, transpose = TRUE, select)
    if (empty.rows == "fill") object <- fill_rows(object, empty.thresh)

    .mimage(object, colors, legend.label, nrow, ncol, fixed, transform, trim)
})


#' @rdname mimage
#' @export
setMethod("mimage", c(object = "oligoPLM"),
  function(object,
           colors,
           select = NULL,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           empty.rows = "fill",
           empty.thresh = 0.6,
           transform,
           trim = 0.01,
           probes,
           type) {

    if (missing(colors))       colors <- scales::brewer_pal(palette = "RdBu")(9)
    if (missing(legend.label)) legend.label <- "Expression"
    if (missing(transform))    transform <- identity
    if (missing(probes))       probes <- "pm"

    object <- marray(object, probes, select, transpose = TRUE, type)
    if (empty.rows == "fill") object <- fill_rows(object, empty.thresh)

    .mimage(object, colors, legend.label, nrow, ncol, fixed, transform, trim)
})




#' @rdname mimage
#' @export
setMethod("mimage", c(object = "array"),
  function(object,
           colors,
           select = NULL,
           legend.label = "Values",
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           transform,
           trim = 0.01) {

  if (missing(colors))       colors <- scales::brewer_pal(palette = "YlGnBu")(9)
  if (missing(legend.label)) legend.label <- "Values"
  if (missing(transform))    transform <- identity

  .mimage(object, colors, legend.label, nrow, ncol, fixed, transform, trim)
})



.mimage <- function(object,
           colors,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           transform,
           trim) {

  if (is.matrix(object)) {
    object <- array(object,
                    dim = c(dim(object), 1),
                    dimnames = c(dimnames(object), list(NULL)))
  }

  object <- transform(object)
  object <- trim_values(object, trim)

  # layout plot table
  n <- dim(object)[3]
  dims <- layout_dims(n, nrow, ncol)
  dims <- trim_dims(n, dims[1], dims[2])

  if (is.null(dimnames(object))) dimnames(object) <- list(NULL, NULL, NULL)
  # TODO: No reason to label 'sample'
  if (is.null(dimnames(object)[[3]]))
    dimnames(object)[[3]] <- paste0("sample", seq_len(n))

  labels <- dimnames(object)[[3]]
  legend <- train_legend(object, colors)
  obj.colors <- scale_colors(object, legend$palette)

  obj.raster <- lapply(labels, function(l) {
    grid::rasterGrob(obj.colors[,, l],
                     interpolate = FALSE,
                     name = paste0("image.", l))
  })

  if (!fixed) {
    x.rng <- range(seq_len(nrow(object)) / nrow(object))
    y.rng <- range(seq_len(ncol(object)) / ncol(object))

    obj.raster <- lapply(obj.raster, grid::editGrob,
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
              grobs = rbind(obj.labels, obj.raster)[row.order,, drop = FALSE],
            heights = heights, widths = widths, respect = fixed)

  # legend
  legend.table <-
    build_legend(legend$breaks, legend$fill, legend$labels, legend.label)

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
