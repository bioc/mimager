check_probes <- function(x) {
  if(is.null(x)) x <- "pm"
  match.arg(tolower(x), c("pm", "mm", "both"))
}
