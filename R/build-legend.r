
legend


legend$text.width <- unit(1, "strwidth", legend$labels[which.max(nchar(legend$labels))])



nbreaks <- length(legend$breaks)

stringWidth(legend$labels)

key.ht <- unit(1.2, "lines")
key.wd <- unit(1.2, "lines")

legend.ht <- key.ht * nbreaks

pushViewport(viewport(width = key.wd, height = legend.ht))
grid.raster(legend$fill, interp = FALSE)

grid.text(legend$labels,
          x = rep(key.wd * 0.5, nbreaks),
          y = unit(seq_len(nbreaks) / nbreaks, "npc") - (key.ht * 0.5))



grid.points(key.wd * 0.5, (key.ht * 0.5) * 6)

grid.text(legend$labels,
          x = key.wd * 0.5,
          y = cumsum(rep(key.ht, nbreaks)) - 0.5)

  grid.rect(x = 0.5, y = seq_len(nbreaks) / nbreaks,
            width = key.wd,
            height = key.ht,
            just = c(0.5, 1.0),
            gp = gpar(fill = legend$fill))



  grid.text(legend$labels,
            x = 0.5,
            y = unit(seq_len(nbreaks) / breaks, "npc") - (key.ht * 0.5),
            just = c(.5, 1))
