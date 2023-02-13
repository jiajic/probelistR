
# separator ####

#' @name unique_collapse
#' @title Collapse unique entries
#' @param ... character objects to collapse
#' @param split_separator symbol to strsplit by (defaults to collapse_separator)
#' @param collapse_separator symbol to collapse by
#' @keywords internal
#' @noRd
unique_collapse = function(...,
                           collapse_separator = ':',
                           split_separator = collapse_separator) {
  collapsed = paste(..., sep = split_separator) |>
    strsplit(split_separator) |>
    unlist() |>
    unique() |>
    sort() |>
    paste0(collapse = collapse_separator)

  return(collapsed)
}



unique_split = function(x, separator = ':') {
  return(x |> strsplit(separator) |> unlist() |> unique() |> sort())
}






# GO ####


#' @name getGOHierarchical
#' @title Get GO terms in a hierarchical manner
#' @param x prbList
#' @param query GO term to search for child terms with
#' @param visited_terms do not use. Used in recursion
#' @export
getGOHierarchical = function(x, query, visited_terms = c()) {

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
    sub_terms_recursive = getGOHierarchical(x = x,
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








# text formatting ####

#' @title Wrap text
#' @name wrap_txt
#' @param ... additional params to pass
#' @param sep how to join elements of string (default is one space)
#' @param strWidth externally set wrapping width. (default value of 100 is not effected)
#' @param errWidth default = FALSE. Set strWidth to be compatible with error printout
#' @keywords internal
wrap_txt = function(..., sep = ' ', strWidth = 100, errWidth = FALSE) {
  custom_width = ifelse(is.null(match.call()$strWidth), yes = FALSE, no = TRUE)
  if(!isTRUE(custom_width)) {
    if(isTRUE(errWidth)) strWidth = getOption('width') - 6
  }

  cat(..., sep = sep) %>%
    capture.output() %>%
    strwrap(., prefix =  ' ', initial = '', # indent later lines, no indent first line
            width = min(80, getOption("width"), strWidth)) %>%
    paste(., collapse = '\n')
}

#' @title Wrap message
#' @name wrap_msg
#' @param ... additional strings and/or elements to pass to wrap_txt
#' @param sep how to join elements of string (default is one space)
#' @keywords internal
wrap_msg = function(..., sep = ' ') {
  message(wrap_txt(..., sep = sep))
}

