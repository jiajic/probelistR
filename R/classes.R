
# Validity Function ####
# prbList_validity = function(object) {
#   if(!all(c('feat_ID', 'GO', 'comments') %in% colnames(object@infoDT))) {
#     stop('Missing column info in data.table input\n')
#   }
# }




# definition ####
#' @name prbList-class
#' @title Container for probelist information
#' @slot infoDT data.table of gene information and annotations
#' @slot GO_terms annotation terms
#' @slot GO_hierarchy hierarchy information between annotation terms
#' @slot igraph igraph of hierarchy
#' @slot separator separator symbol used within object
#' @export
prbList = setClass('prbList',
         slots = list(
           infoDT = 'ANY',
           GO_terms = 'character',
           GO_hierarchy = 'list',
           GO_initialized = 'logical',
           igraph = 'ANY',
           separator = 'character',
           misc = 'ANY'
         ),
         prototype = list(
           infoDT = NULL,
           GO_terms = NA_character_,
           GO_hierarchy = list(),
           GO_initialized = FALSE,
           igraph = NULL,
           separator = NA_character_,
           misc = NULL
         ))


setMethod('show', signature('prbList'), function(object) {

  nfeats = nrow(object@infoDT)
  if(is.null(nfeats)) nfeats = 0L

  n_in_panel = sum(object@infoDT$include)

  if(any(is.na(GOTerms(object)))) {
    nterms = 0L
  } else {
    nterms = length(GOTerms(object))
  }




  # prints
  cat(class(object), ': ', sep = '')
  cat('Panel of ', n_in_panel, ' (out of ', nfeats,' features)\n', sep = '')
  cat('+ ', nterms, ' GO terms', sep = '')
  cat(', separator: "', sep(object), '"', sep = '')


})


# initialize method ####
setMethod('initialize', signature('prbList'), function(.Object, ...) {
  .Object = callNextMethod()
  if(!is.null(.Object@infoDT)) {

    # convert to data.table and setup feat_ID col
    .Object@infoDT = evaluate_DF(probe_data = .Object@infoDT)


      if(isTRUE(.Object@GO_initialized)) {

        # validity
        if(!all(c('feat_ID', 'GO') %in% colnames(.Object@infoDT))) {
          stop('Missing column info in data.table input\n')
        }

        separator = sep(.Object)

        # unique GO and comments
        .Object@infoDT[, GO := sapply(GO, function(x) unique_collapse(x, split_separator = separator,
                                                                      collapse_separator = separator))]
        .Object@infoDT[, comments := sapply(comments, function(x) unique_collapse(x, split_separator = separator,
                                                                      collapse_separator = separator))]

        # cleanup separator
        .Object@infoDT = run_cleanups(.Object@infoDT, cols = 'GO', separator = separator)
        if('comments' %in% colnames(.Object@infoDT)) {
          .Object@infoDT = run_cleanups(.Object@infoDT, cols = 'comments', separator = separator)
        }

        # panelInclude info
        if(!'include' %in% colnames(.Object@infoDT)) .Object@infoDT[, include := TRUE]
        else .Object@infoDT[is.na(include), include := TRUE]

        # feature locking
        if(!'lock' %in% colnames(.Object@infoDT)) .Object@infoDT[, lock := FALSE]

        # Generate GO terms info
        .Object@GO_terms = unique(unlist(strsplit(.Object@infoDT$GO, separator)))
      }

    .Object@infoDT = unique(.Object@infoDT)
    data.table::setkey(.Object@infoDT, 'feat_ID')
  }

  if(length(GOHierarchy(.Object)) > 0) {
    # Generate igraph hierarchy representation
    .Object = create_igraph_net(.Object)
  }

  .Object
})


# internal constructor function ####
create_prbList = function(infoDT = NULL,
                          GO_terms = NA_character_,
                          GO_hierarchy = list(),
                          igraph = NULL,
                          separator = ':') {
  prbList = new('prbList',
                infoDT = infoDT,
                GO_terms = GO_terms,
                GO_hierarchy = GO_hierarchy,
                igraph = igraph,
                separator = separator)

  return(prbList)
}


