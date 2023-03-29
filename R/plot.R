


# networks ####

#' @export
setMethod('plotGO', signature('prbList'), function(x) {

  plot(x@igraph)

})




#' @export
setMethod('plotGOInteractive', signature('prbList'),
          function(x, bounded = FALSE, nodeSize = FALSE) {
  g = x@igraph
  v_names = igraph::V(g)$name
  igraph::V(g)$ID = v_names
  igraph::V(g)$name = seq(length(g)) - 1L # zero indexed for javascript

  linkDT_for_plot = igraph::get.data.frame(x = g, what = 'edges') |>
    data.table::setDT()
  nodeDT_for_plot = igraph::get.data.frame(x = g, what = 'vertices') |>
    data.table::setDT()

  linkDT_for_plot[, value := 1L]

  nodeDT_for_plot[, term_counts := sapply(ID, function(term)
    length(getFeats(x = x, query = term, by = 'GO')))]
  nodeDT_for_plot[, group := 1L]


  networkD3::forceNetwork(
    Links = linkDT_for_plot,
    Nodes = nodeDT_for_plot,
    Source = 'from',
    Target = 'to',
    Value = 'value',
    NodeID = 'ID',
    Group = 'group',
    opacity = 1L,
    opacityNoHover = 0.6,
    zoom = TRUE,
    arrows = TRUE,
    charge = -60,
    bounded = bounded,
    fontSize = 14L,
    fontFamily = 'serif',
    Nodesize = if(isTRUE(nodeSize)) 'term_counts' else 'group'
  )

})









# barplots ####

#' @title Histogram of categories
#' @name histGO
#' @param x prbList
#' @param query vector of term(s) to plot
#' @param by category of term (column to use)
#' @param orientation 'h' for horizontal (default) or 'v' for vertical
#' @export
histGO = function(x, query, by = 'GO', orientation = 'h') {

  Counts = sapply(query, function(term) {
    getFeats(x = x, query = term, by = by) |>
      length()
  })


  countsDT = data.table::data.table(
    Category = query,
    Counts = Counts
  )

  p = ggplot2::ggplot()

  if(orientation == 'h') {
  p = p + geom_bar(data = countsDT,
                   mapping = aes(x = Counts, y = Category),
                   stat = 'identity')

  } else if(orientation == 'v') {
    p = p + geom_bar(data = countsDT,
                     mapping = aes(x = Category, y = Counts),
                     stat = 'identity')
  }

  plot(p)

}













# venn diagrams ####

# https://stackoverflow.com/questions/50430225/euler-diagram-with-eulerr-in-r

#' Contingency table from table(prbs[]$GO)



plot_nVennR = function() {

  if(!requireNamespace('nVennR', quietly = TRUE)) {
    stop(wrap_txt('nVennR not installed.
                  Please install with "devtools::install_github("vqf/nVennR") to continue',
                  errWidth = TRUE))
  }




}




plot_GO_intersect = function(...) {

}






