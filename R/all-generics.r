setGeneric("mindex",
  function(object,
           probes = NULL) standardGeneric("mindex"))

setGeneric("ma_values",
  function(object,
           probes = NULL,
           select = NULL, ...) standardGeneric("ma_values"))

setGeneric("ma_layout",
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE, ...) standardGeneric("ma_layout"))

setGeneric("ma_image",
  function(object,
           colors,
           select = NULL,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           range = NULL,
           transform, ...) standardGeneric("ma_image"))

