# Exported creation function ####
#' @name createPrbList
#' @title Create a prbList object
#' @param probe_data dataframe-like object containing probe information.
#' See details
#' @param GO_cols vector of column(s) in probe_data that contain user-
#' defined terms that organize the information.
#' @param comm_cols (optional) vector of column(s) to use for the comments info.
#' If left as NULL, all columns other than GO and feat_ID will be used.
#' @param GO_hierarchy (optional) user defined hierarchy can be passed here.
#' Should be provided as a named list of vectors, where the list names are the
#' parent terms and the items in the vectors are the child terms
#' @param read_separator (default = ":") separator to use on data to be read in
#' @param obj_separator (optional. default = ":") separator symbol to use in the
#' final prbList object
#' @export
createPrbList = function(probe_data,
                         GO_cols,
                         comm_cols = NULL,
                         GO_hierarchy = NULL,
                         read_separator = ':',
                         obj_separator = ':') {

  if(inherits(probe_data, 'data.table')) probe_data = data.table::copy(probe_data)

  x = create_prbList(infoDT = probe_data,
                     separator = obj_separator)

  x = setGOCols(x = x,
                GO_cols = GO_cols,
                read_sep = read_separator,
                obj_sep = obj_separator)

  x = setCommentCols(x = x,
                     comm_cols = comm_cols,
                     read_sep = read_separator,
                     obj_sep = obj_separator)

  data.table::setcolorder(x[], neworder = c('feat_ID', 'GO'))

  x = initialize(x)

  return(x)
}



# Comments ####




# Feature Editing ####

#' @name setFeatID
#' @title Change a feature ID symbol for another
#' @param x prbList
#' @param old_ID feature ID to change
#' @param new_ID feature ID to change to
#' @return prbList
#' @export
setFeatID = function(x, old_ID, new_ID) {

  if(length(old_ID) != length(new_ID)) {
    stop(wrap_txt('Inputs for the old ID vector and new ID vector',
                  'must be in the same order and the same length',
                  errWidth = TRUE))
  }

  x = copy(x)

  lapply(seq_along(old_ID), function(i) {
    x[][, feat_ID := gsub(old_ID[[i]], new_ID[[i]], feat_ID)]
  })

  return(x)
}



# igraph relationships ####

#' @name create_igraph_net
#' @param x prbList
#' @keywords internal
create_igraph_net = function(x) {
  # if hierarchy info exists
  if(length(GOHierarchy(x)) > 0) {
    parent_entries = lengths(GOHierarchy(x))

    relations = data.table::data.table(from = rep(names(parent_entries), parent_entries))
    for(term in names(parent_entries)) {
      relations[from == term, to := GOHierarchy(x)[[term]]]
      relations[, weights := 1]
    }

    g = igraph::graph.data.frame(relations, directed = TRUE)
    x@igraph = g
  }

  return(x)
}








