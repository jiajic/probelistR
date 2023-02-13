




# generics ####
setGeneric('addFeat', function(x, ...) standardGeneric('addFeat'))
setGeneric('addList', function(x, ...) standardGeneric('addList'))






setGeneric('rmFeat', function(x, ...) standardGeneric('rmFeat'))
setGeneric('GOTerms', function(x, ...) standardGeneric('GOTerms'))
setGeneric('GOHierarchy', function(x, parent, ...) standardGeneric('GOHierarchy'))
setGeneric('GOHierarchy<-', function(x, parent, value, ...) standardGeneric('GOHierarchy<-'))
setGeneric('getFeats', function(x, by, query, ...) standardGeneric('getFeats'))
setGeneric('validFeatIDs', function(x, ...) standardGeneric('validFeatIDs'))
if(!isGeneric('featIDs')) setGeneric('featIDs', function(x, ...) standardGeneric('featIDs'))
setGeneric('sep', function(x, ...) standardGeneric('sep'))
setGeneric('sep<-', function(x, value, ...) standardGeneric('sep<-'))
setGeneric('annotatePrbList', function(x, term, feats, ...) standardGeneric('annotatePrbList'))
setGeneric('removeGOTerm', function(x, term, ...) standardGeneric('removeGOTerm'))
if(!isGeneric('copy')) setGeneric('copy', function(x) standardGeneric('copy'))
setGeneric('comments', function(x, feats, ...) standardGeneric('comments'))
setGeneric('comments<-', function(x, feats, values, ...) standardGeneric('comments<-'))
setGeneric('plotGO', function(x, ...) standardGeneric('plotGO'))
setGeneric('setGOCols', function(x, GO_cols, read_sep, obj_sep, ...) standardGeneric('setGOCols'))
setGeneric('setCommentCols', function(x, comm_cols, read_sep, obj_sep, ...) standardGeneric('setCommentCols'))







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
setMethod('plotGO', signature('prbList'), function(x) plot(x@igraph))

#' @export
setMethod('copy', signature('prbList'), function(x) {
  x[] = data.table::copy(x[])
  x
})

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

#' @export
setMethod('addFeat', signature('prbList'), function(x,
                                                    feat_to_add,
                                                    GO_terms = NA_character_,
                                                    comments_to_add = NA_character_) {
  separator = sep(x)

  if(!is.na(GO_terms)) GO_terms = paste0(GO_terms, collapse = separator)
  if(!is.na(comments_to_add)) comments_to_add = paste0(comments_to_add, collapse = separator)

  if(feat_to_add %in% x@infoDT$feat_ID) {
    x[feat_ID == feat_to_add, GO := unique_collapse(GO, GO_terms, separator = separator)]
    x[feat_ID == feat_to_add, comments := unique_collapse(comments, comments_to_add, separator = separator)]
  } else {
    new_row = data.table::data.table(
      feat_ID = feat_to_add,
      GO = GO_terms,
      comments = comments_to_add
    )
    x@infoDT = rbind(x@infoDT, new_row, fill = TRUE)
    return(x)
  }
})
#' @export
setMethod('rmFeat', signature('prbList'), function(x, feat_to_remove) {
  x@infoDT = x@infoDT[!feat_to_remove == feat_ID, ]
  x
})



#' @export
setMethod('comments', signature('prbList'), function(x, feats) {
  x[feat_ID %in% feats, comments, by = feat_ID]
})




#' @export
setMethod('GOTerms', signature('prbList'), function(x)  x@GO_terms)


#' @export
setMethod('GOHierarchy', signature(x = 'prbList', parent = 'character'), function(x, parent) {
  (x@GO_hierarchy[[parent]])
})
#' @export
setMethod('GOHierarchy', signature(x = 'prbList', parent = 'missing'), function(x) x@GO_hierarchy)
#' @export
setMethod('GOHierarchy<-', signature(x = 'prbList', parent = 'missing'), function(x, value) {
  initialize(x, GO_hierarchy = value)
})
#' @export
setMethod('GOHierarchy<-', signature(x = 'prbList', parent = 'character'), function(x, parent, value) {
  x@GO_hierarchy[[parent]] = value
  return(initialize(x))
})


#' @export
setMethod('annotatePrbList', signature(x = 'prbList', term = 'character', feats = 'character'),
          function(x, term, feats) {
            x = annotate_feats(x = x, term = term, feats = feats)
            return(x)
          })


#' @export
setMethod('getFeats', signature('prbList'), function(x, by = NULL, query = NULL) {

  if(by == 'GO') {
    query = sapply(query, function(term) {
      if(term %in% names(GOHierarchy(x))) getGOHierarchical(x = x, query = term)
    }) |>
      unlist() |>
      unique()
  }

  query = paste0(query, collapse = '|')

  return(x@infoDT[eval(call('grepl', query, as.name(by))), feat_ID])
})



#' @export
setMethod('validFeatIDs', signature('prbList'), function(x, species = 'Hs') {
  validate_gene_names(x = x, species = species)
})

