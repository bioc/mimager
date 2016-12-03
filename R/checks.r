check_trim <- function(x) {
  if (x >= 0 & x <= 1) {
    return(x)
  } else {
    stop("trim must be between 0 and 1", call. = FALSE)
  }
}

check_probe <- function(object, probe) {
  if(is.null(probe)) probe <- "pm"
  choices <- switch(class(object),
    AffyBatch            = c("all", "pm", "mm"),
    PLMset               = c("all", "pm", "mm"),
    ExpressionFeatureSet = c("all", "pm", "mm"),
    GeneFeatureSet       = c("all", "pm", "bg"),
    ExonFeatureSet       = c("all", "pm", "mm", "bg")
  )
  if (all(probe %in% choices)) return(probe)
  stop(
    "Invalid probe for ", class(object), " should be one of ",
    paste(choices, collapse = ", "),
    call. = FALSE
  )
}

# check for annotation packages
check_annotation <- function(x) {
  stopifnot(is.character(x))
  status <-
    suppressPackageStartupMessages(require(x, character.only = TRUE, quietly = TRUE))
	if (status) return(x)
  stop(
    x, "Please install ", x, " from Biocounductor.\n",
    "source(\"https://bioconductor.org/biocLite.R\")\n",
    "biocLite(", x, ")\n",
    call. = FALSE
  )
}