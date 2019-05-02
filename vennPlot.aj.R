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
#
#
#
indiv.inter.aj  <- function(list,name) {
    # Given a list of three elements with names
    # this finds the individual elements of the spec name.
    require(rlist)
    name.inx <- match(name,names(list))
    new.list = list.remove(list,name.inx)
    first = list[[name.inx]]
    second = new.list[[1]]
    third = new.list[[2]]
    uniq.for.first <- setdiff(first,union(second,third))
    return(uniq.for.first)
}
