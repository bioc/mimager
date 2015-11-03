# Convert AffyBatch or PLMSet to an array of matrixes that correspond to the
# physical layout of their microarray features

array_layout <- function(x) {

  labels <- sampleNames(x)

  # image matrix
  nr <-  x@nrow
  nc <-  x@ncol
  n  <- length(labels)


  # probe coordinates
  probe.index <- indexProbes(x, which = "pm")

  xycoor <- lapply(probe.index, indices2xy, nc = nc)
  xycoor <- do.call("rbind", xycoor)
  xycoor <- rbind(xycoor, cbind(x = xycoor[, "x"], y = xycoor[, "y"] + 1))

  # extract array values
  exp.mat <- switch(class(x),
         PLMset = residuals(x)$PM.resid,
      AffyBatch = pm(x)
  )

  exp.array <- lapply(labels, function(l) {
    xyzcoor <- cbind(xycoor, z = match(l, labels))
    mat <- matrix(nrow = nr, ncol = nc)
    mat[xycoor] <- exp.mat[, l]
    mat
  })

  exp.array <- abind::abind(exp.array, along = 3)
  dimnames(exp.array) <- list(seq_len(nr), seq_len(nc), labels)

  exp.array
}
