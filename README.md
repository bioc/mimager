# mimager: The Microarray Imager

*mimager* simplifies the process of imaging microarrays and inspecting them for spatial artifacts by providing a single visualization function (`mimage()`) that works consistently with many of the microarray object classes provided by BioConductor.

```r
data("Dilution", package = "affydata")
mimage(Dilution, transform = arle, nrows = 1, legend.label = "RLE")
```

![mimager example](http://i.imgur.com/2Wf4y8v.jpg)
