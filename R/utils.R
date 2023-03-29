
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

