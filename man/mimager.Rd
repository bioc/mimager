% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mimager-package.r
\docType{package}
\name{mimager}
\alias{mimager}
\alias{mimager-package}
\title{mimager: The Microarray Imager}
\description{
\pkg{mimager} simplifies the creation of microarray images (sometimes called
"pseudo-images") for the purpose of identifying problematic regional
aberrations. Notable features include support for many of Bioconductor's core
microarray data structures, providng compatibility with a variety of common
microarray platforms, and the ability to visualize multiple microarrays
simultaneously.
}
\details{
The following Bioconductor microarray data structures are currently
supported:

\itemize{
  \item \code{\link[affy]{AffyBatch-class}} for Affymetrix GeneChip
    probe level data
   \item \code{\link[affyPLM]{PLMset-class}} for probe-level
     linear models fitted to Affymetrix GeneChip probe level data
   \item \code{\link[oligoClasses]{FeatureSet-class}} for storing
     Expression/Exon/SNP data from a variety of oligonucleotide platforms
   \item \code{\link[oligo]{oligoPLM-class}} for probe-level linear models
     fitted to any of the \code{FeatureSet}-like classes
}
}

