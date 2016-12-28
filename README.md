# mimager: The Microarray Imager

*mimager* simplifies the process of imaging microarrays and inspecting them for spatial artifacts by providing a consistent visualization interface that supports many of Bioconductor's microarray object classes.

## Installation

You can install the latest release from Bioconductor:

```r
source("http://www.bioconductor.org/biocLite.R")
biocLite("mimager")
```

or the current development version using `devtools`:

```r
# library(devtools)
install_github("aaronwolen/mimager", build_vignettes = TRUE)
```

## Example

```r
library(mimager)
library(affydata)
data("Dilution")

mimage(Dilution, transform = arle, nrows = 1, legend.label = "RLE")
```

![mimager example](http://i.imgur.com/2Wf4y8v.jpg)
