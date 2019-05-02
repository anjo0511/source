#
# Written by: Andreé Johnsson,andreejohnsson@outlook.com
#
vennPlot.aj <- function(list.with.names, title) {
    require(RColorBrewer);require(VennDiagram)
    pal <- brewer.pal(8, "Dark2")
    grid.newpage()
    grid.draw(
      venn.diagram(
        list.with.names,
        filename = NULL,
        fill = pal[1:length(list.with.names)],
        cat.pos = c(335, 30, 180), 
        main = title
      )
    )
  }
