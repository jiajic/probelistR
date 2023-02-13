
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
setClass('prbList',
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




# initialize method ####
setMethod('initialize', signature('prbList'), function(.Object, ...) {
  .Object = callNextMethod()
  if(!is.null(.Object@infoDT)) {

    # convert to data.table and setup feat_ID col
    .Object@infoDT = evaluate_DF(probe_data = .Object@infoDT)


      if(isTRUE(.Object@GO_initialized)) {

        # validity
        if(!all(c('feat_ID', 'GO') %in% colnames(object@infoDT))) {
          stop('Missing column info in data.table input\n')
        }

        separator = sep(.Object)

        # cleanup separator
        .Object@infoDT = separator_cleanup(.Object@infoDT, cols = 'GO', separator = separator)
        if('comments' %in% colnames(.Object@infoDT)) {
          .Object@infoDT = separator_cleanup(.Object@infoDT, cols = 'comments', separator = separator)
        }


        # Generate GO terms info
        .Object@GO_terms = unique(unlist(strsplit(.Object@infoDT$GO, separator)))
      }

    .Object@infoDT = unique(.Object@infoDT)
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


