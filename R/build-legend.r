
build_legend <- function(breaks, colors, labels, title) {

  key.height <- unit(1.2, "lines")
  key.width  <- unit(1.2, "lines")

  nbreaks <- length(breaks)

  legend.grob <- grid::rasterGrob(colors,
             x = unit(0, "npc"),
        interp = FALSE,
          just = "left",
          name = "legend.key",
         width = grid::convertWidth(key.width, "mm"),
        height = grid::convertHeight(key.height * nbreaks, "mm"))

  legend.text.grob <- grid::textGrob(labels,
            x = unit(0, "npc"),
            y = unit(seq_len(nbreaks) / nbreaks, "npc") - (key.height * 0.5),
         just = "left",
         name = "legend.text")

  legend.table <- gtable(
    widths = grid::unit.c(key.width, grid::grobWidth(legend.text.grob)),
    height = grid::grobHeight(legend.grob))
  legend.table <- gtable_add_grob(legend.table, legend.text.grob,
                                  t = 1, l = 2, r = 2)
  legend.table <- gtable_add_grob(legend.table, legend.grob,
                                  t = 1, l = 1, r = 1)

  return(legend.table)
}

