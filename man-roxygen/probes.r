#' @section Probe types:
#'
#' For microarray data structures the \code{type} argument determines the
#' \emph{type} of probe that should be included. The following table provides a
#' list of valid values for each supported microarray class:
#'
#' \tabular{rcccc}{
#' \code{AffyBatch}
#'   \tab \code{"all"} \tab \code{"pm"} \tab \code{"mm"} \tab - \cr
#' \code{ExpressionFeatureSet}
#'   \tab \code{"all"} \tab \code{"pm"} \tab \code{"mm"} \tab - \cr
#' \code{GeneFeatureSet}
#'   \tab \code{"all"} \tab \code{"pm"} \tab - \tab \code{"bg"}\cr
#' \code{ExonFeatureSet}
#'   \tab \code{"all"} \tab \code{"pm"} \tab \code{"mm"} \tab \code{"bg"} \cr
#' \code{SnpFeatureSet}
#'   \tab \code{"all"} \tab \code{"pm"} \tab \code{"mm"} \tab - \cr
#' }
#'
