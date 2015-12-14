# return intensity value from probe's x/y coordinates
xy2value <- function(object, x, y) {
  i <- affy::xy2indices(x, y, abatch = object)
  affy::exprs(object)[i, ]
}