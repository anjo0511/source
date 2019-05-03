#
# Written by: Andreé Johnsson,andreejohnsson@outlook.com
#
vennPlot.aj <- function(list.with.names, title="") {
    futile.logger::flog.threshold(
        futile.logger::ERROR,name = "VennDiagramLogger") # Supresses Logfiles
    require(RColorBrewer);require(VennDiagram)
    pal <- brewer.pal(8, "Dark2")
    grid.newpage()
    grid.draw(
        venn.diagram(
            list.with.names,
            filename = NULL,
            fill = pal[1:length(list.with.names)],
            # cat.pos = c(335, 30, 180), 
            main = title,
            euler.d  = TRUE,
            scaled = TRUE,
            cex = 1.25,
            cat.cex = 1.25,
            main.cex = 1.34,
            cat.dist = 0.05,
            na = "remove",
            force.unique = TRUE
        )
    )
    
}
#
# TEST
# l.1 = list("First"=sample(seq(3200),1),
#            "Second"=sample(seq(3200),1110),
#            "Third"=sample(seq(3200),212))
# 
# vennPlot.aj(l.1,title = "Venn Diagram Title")

