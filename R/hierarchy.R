

# GOHierarchy ####

#' @title Gene ontology hierarchy
#' @name GOHierarchy-generic
#' @description Get and set gene ontology hierarchy
#' @param x prbList object
#' @param parent parent GO term
#' @param value children GO terms to set
#' @aliases GOHierarchy, GOHierarchy<-
NULL

#' @rdname GOHierarchy-generic
#' @export
setMethod('GOHierarchy',
          signature(x = 'prbList',
                    parent = 'character'),
          function(x, parent) {
            (x@GO_hierarchy[[parent]])
          })



#' @rdname GOHierarchy-generic
#' @export
setMethod('GOHierarchy',
          signature(x = 'prbList',
                    parent = 'missing'),
          function(x) x@GO_hierarchy
          )



#' @rdname GOHierarchy-generic
#' @export
setMethod('GOHierarchy<-',
          signature(x = 'prbList',
                    parent = 'missing'),
          function(x, value) {
            initialize(x, GO_hierarchy = value)
          })



#' @rdname GOHierarchy-generic
#' @export
setMethod('GOHierarchy<-',
          signature(x = 'prbList',
                    parent = 'character'),
          function(x, parent, value) {
          x@GO_hierarchy[[parent]] = value
          return(initialize(x))
         })








# GO searches ####

#' @name getGOHierarchical
#' @title Get GO terms in a hierarchical manner
#' @param x prbList
#' @param query GO term to search for child terms with
#' @param return_self include initial GO term query in return
#' @export
getGOHierarchical = function(x, query, return_self = TRUE) {

  GO_H = get_GO_Hierarchical(x = x, query = query)

  if(!isTRUE(return_self)) {
    GO_H = GO_H[!GO_H == query]
  }

  return(GO_H)

}




#' @noRd
#' @param x prbList
#' @param query GO term to search for child terms with
#' @param visited_terms do not use. Used in recursion
#' @keywords internal
get_GO_Hierarchical = function(x, query, visited_terms = c()) {

  # visited_terms gets added to with each iteration and keeps a record
  # of ALL terms to detect circular hierarchies

  # skip if query has been used before
  if(query %in% visited_terms) {
    warning('Circular hierarchy detected for', query, '\n')
    return(NULL)
  }

  # increment visited terms
  visited_terms = c(visited_terms, query)

  # if no hierarchical info, return directly
  if(all(is.na(x@GO_terms))) {
    return(query)
  }

  sub_terms = x@GO_hierarchy[[query]]
  terms = query
  for(sub_term in sub_terms) {
    sub_terms_recursive = get_GO_Hierarchical(x = x,
                                              query = sub_term,
                                              visited_terms = visited_terms)
    if(!is.null(sub_terms_recursive)) {
      terms = c(terms, sub_terms_recursive)
    }
  }

  # debug
  u_terms = unique(terms)
  if(!identical(u_terms, terms)) {
    message('non-unique terms returning from GO hierarchy')
  }

  return(u_terms)
}








