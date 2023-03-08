
#' @include classes.R
NULL

# setGeneric('addList', function(x, ...) standardGeneric('addList'))


# generics ####
setGeneric('addFeat', function(x, ...) standardGeneric('addFeat'))
setGeneric('rmFeat', function(x, ...) standardGeneric('rmFeat'))

if(!isGeneric('copy')) setGeneric('copy', function(x) standardGeneric('copy'))


setGeneric('getFeats', function(x, query, ...) standardGeneric('getFeats'))

if(!isGeneric('featIDs')) setGeneric('featIDs', function(x, ...) standardGeneric('featIDs'))

setGeneric('GOTerms', function(x, ...) standardGeneric('GOTerms'))
setGeneric('removeGOTerm', function(x, term, ...) standardGeneric('removeGOTerm'))
setGeneric('GOHierarchy', function(x, parent, ...) standardGeneric('GOHierarchy'))
setGeneric('GOHierarchy<-', function(x, parent, value, ...) standardGeneric('GOHierarchy<-'))

setGeneric('GO', function(x, feats, ...) standardGeneric('GO'))
setGeneric('GO<-', function(x, feats, value, ...) standardGeneric('GO<-'))
setGeneric('comments', function(x, feats, ...) standardGeneric('comments'))
setGeneric('comments<-', function(x, feats, value, ...) standardGeneric('comments<-'))

setGeneric('sep', function(x, ...) standardGeneric('sep'))
setGeneric('sep<-', function(x, value, ...) standardGeneric('sep<-'))


## obj setup ####
setGeneric('setGOCols', function(x, GO_cols, read_sep, obj_sep, ...) standardGeneric('setGOCols'))
setGeneric('setCommentCols', function(x, comm_cols, read_sep, obj_sep, ...) standardGeneric('setCommentCols'))

## plotting ####
setGeneric('plotGOInteractive', function(x, ...) standardGeneric('plotGOInteractive'))
setGeneric('plotGO', function(x, ...) standardGeneric('plotGO'))

setGeneric('validFeatIDs', function(x, ...) standardGeneric('validFeatIDs'))

# methods ####
#' @export
setMethod('setGOCols', signature('prbList'), function(x, GO_cols, read_sep = ':', obj_sep = ':') {
  x[] = initialize_GO_col(probe_data = x[], GO_cols = GO_cols, obj_separator = obj_sep, read_separator = read_sep)
  return(x)
})
#' @export
setMethod('setCommentCols', signature('prbList'), function(x, comm_cols = NULL, read_sep = ':', obj_sep = ':') {
  x[] = initialize_comments_col(probe_data = x[], comm_cols = comm_cols, obj_separator = obj_sep, read_separator = read_sep)
  return(x)
})


#' @export
setMethod('sep', signature('prbList'), function(x) x@separator)
#' @export
setMethod('sep<-', signature('prbList'), function(x, value = ':') initialize(x, separator = value))



























#' @export
setMethod('getFeats', signature('prbList'), function(x, query = NULL, by = 'GO') {

  if(by == 'GO') {
    GO_query = sapply(query, function(term) {
      if(term %in% names(GOHierarchy(x))) getGOHierarchical(x = x, query = term)
    }) |>
      unlist() |>
      unique()

    if(!is.null(GO_query)) query = GO_query
  }

  query = paste0(query, collapse = '|')

  return(x@infoDT[eval(call('grepl', query, as.name(by))), feat_ID])
})



#' @export
setMethod('validFeatIDs', signature('prbList'), function(x, species = 'Hs') {
  validate_gene_names(x = x, species = species)
})

