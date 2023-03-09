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



# Basic Class Methods ####
#' @include generics.R
#' @export
setMethod('nrow', signature('prbList'), function(x) nrow(x@infoDT))
#' @export
setMethod('ncol', signature('prbList'), function(x) ncol(x@infoDT))
#' @export
setMethod('colnames', signature('prbList'), function(x) colnames(x@infoDT))
#' @export
setMethod('featIDs', signature('prbList'), function(x) unique(x@infoDT$feat_ID))
#' @export
setMethod('[', signature(x = 'prbList', i = 'missing', j = 'missing', drop = 'missing'),
          function(x, i, j) return(x@infoDT))
#' @export
setMethod('[<-', signature(x = 'prbList', i = 'missing', j = 'missing', value = 'ANY'),
          function(x, i, j, value) {
            x@infoDT = value
            x
          })





# Add/Remove Feats ####

#' @include generics.R
#' @param x prbList object
#' @param feat_to_add character vector of feature IDs to add
#' @param GO_terms character vector of GO terms to apply to all feats in feat_to_add
#' @param comments_to_add character vector of comments to apply to all feats in feat_to_add
#' @export
setMethod('addFeat', signature('prbList'),
          function(x, feat_to_add, GO_terms = NA_character_,
                   comments_to_add = NA_character_) {
            separator = sep(x)

            in_bool = feat_to_add %in% featIDs(x)

            out_feats = feat_to_add[!in_bool]


            # In list
            x[][feat_ID %in% feat_to_add, GO :=
                unique_collapse(c(GO, GO_terms),
                                separator = separator)]
            x[][feat_ID %in% feat_to_add, comments :=
                unique_collapse(c(comments, comments_to_add),
                                separator = separator)]

            # Not in list
            if(length(out_feats) > 0L) {
              new_row = data.table::data.table(
                feat_ID = out_feats,
                GO = if(any(is.na(GO_terms))) NA else unique_collapse(GO_terms,
                                                                 separator = separator),
                comments = if(any(is.na(comments_to_add))) NA else unique_collapse(comments_to_add,
                                                                       separator = separator)
              )
              x@infoDT = rbind(x@infoDT, new_row, fill = TRUE)
            }


            return(initialize(x))
          })







#' @include generics.R
#' @export
setMethod('rmFeat', signature('prbList'), function(x, feat_to_remove) {
  x@infoDT = x@infoDT[!feat_to_remove == feat_ID, ]
  x
})



# Feature Editing ####

#' @title Change a feature ID symbol for another
#' @name changeFeatID
#' @param x prbList
#' @param old_ID vector of feature IDs to change
#' @param new_ID vector of feature IDs to change to
#' @return prbList
#' @export
changeFeatID = function(x, old_ID, new_ID) {

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





# Getting Features ####


#' @title Examine feature
#' @name feature
#' @param x prbList object
#' @param feat feature to examine
#' @param info annotation columns to pull from
#' @export
feature = function(x, feat, info = c('GO', 'comments')) {

  separator = sep(x)
  return_list = lapply(info, function(col_to_use) {
    x[][feat_ID == feat, col_to_use, with = FALSE] |>
    unlist() |>
    unique_split(separator = separator)
  })
  names(return_list) = info
  return(return_list)
}





#' @export
setMethod('getFeats', signature('prbList'),
          function(x, query = NULL, by = 'GO', to_clip = FALSE) {

  if(by == 'GO') {
    GO_query = sapply(query, function(term) {
      if(term %in% names(GOHierarchy(x))) getGOHierarchical(x = x, query = term)
    }) |>
      unlist() |>
      unique()

    if(!is.null(GO_query)) query = GO_query
  }

  query = paste0(query, collapse = '|')

  res = x@infoDT[eval(call('grepl', query, as.name(by))), feat_ID]


  if(isTRUE(to_clip)) {
    clipr::write_clip(res)
  } else {
    return(res)
  }

})








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
      relations[, weights := 1L]
    }

    g = igraph::graph.data.frame(relations, directed = TRUE)

    x@igraph = g
  }

  return(x)
}








