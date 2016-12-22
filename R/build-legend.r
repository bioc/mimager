
build_legend <- function(breaks, colors, labels, title, fontsize) {

  nbreaks <- length(breaks)

  # provide title *after* sizing keys in case it contains line breaks
  legend.title <- grid::textGrob("",
            x = unit(0, "npc"),
            y = unit(1, "npc"),
         just = c("left", "top"),
         name = "legend.title",
         gp = gpar(fontsize = fontsize * 1.1))

  key.width  <- unit(1.2, "lines")
  key.height <- max(key.width, unit(2, "grobheight", legend.title))
  legend.title$label <- title

  legend.text <- grid::textGrob(labels,
            x = unit(0, "npc"),
            y = unit(seq_len(nbreaks) / nbreaks, "npc") - (key.height * 0.5),
         just = "left",
         gp = gpar(fontsize = fontsize * 0.9),
         name = "legend.text")

  legend <- grid::rasterGrob(rev(colors),
             x = unit(0, "npc"),
        interp = FALSE,
          just = "left",
          name = "legend.key",
         width = grid::convertWidth(key.width, "mm"),
        height = grid::convertHeight(key.height * nbreaks, "mm"))

  legend.table <- gtable(name = "legend",
    widths = grid::unit.c(key.width,
                          max(grid::grobWidth(legend.text),
                              grid::grobWidth(legend.title))),
   heights = grid::unit.c(grid::grobHeight(legend.title) * 1.5,
                          grid::grobHeight(legend)))

  legend.table <- gtable_add_grob(legend.table, legend.title,
                                  t = 1, l = 1, r = 2)
  legend.table <- gtable_add_grob(legend.table, legend.text,
                                  t = 2, l = 2, r = 2)
  legend.table <- gtable_add_grob(legend.table, legend,
                                  t = 2, l = 1, r = 1)

  legend.table <- gtable_add_col_space(legend.table, unit(0.2, "lines"))

  return(legend.table)
}
