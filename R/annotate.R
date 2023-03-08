
# Annotation functions
# Functions for assigning GO terms and comments


#' @export
setMethod('GOTerms', signature('prbList'), function(x)  x@GO_terms)


#' @param x prbList
#' @keywords internal
#' @noRd
annotate_feats = function(x, term, feats, type = c('GO', 'comment')) {

  if(!inherits(x, 'prbList')) stop(substitute(x), ' is not a prbList')
  # data.table vars
  GO = NULL

  type = match.arg(type, choices = c('GO', 'comment'))

  x = copy(x)

  switch(
    EXPR = type,
    'GO' = x[][feat_ID %in% feats, GO := paste(GO, term, sep = sep(x))],
    'comment' = x[][feat_ID %in% feats, comments := paste(comments, term, sep = sep(x))],
  )

  x = initialize(x)
  return(x)
}





# GO ####

## Set ####

#' @include generics.R
#' @export
setMethod('GO<-', signature(x = 'prbList', feats = 'character', value = 'character'),
          function(x, feats, value, mode = c('append', 'replace')) {

            mode = match.arg(mode, choices = c('append', 'replace'))

            if(mode == 'append') {
              x = annotate_feats(x = x, term = value, feats = feats, type = 'GO')
            }
            if(mode == 'replace') {
              x[][feat_ID %in% feats, 'GO'] = unique_collapse(value,
                                                              collapse_separator = sep(x))
              x = initialize(x)
            }

            return(x)
          })








## Extract ####

#' @export
setMethod('GO', signature(x = 'prbList', feats = 'character'), function(x, feats) {
  tbl = x[][feat_ID %in% feats, GO, by = feat_ID]

  out_list = lapply(tbl[, feat_ID], function(feat_i) {
    tbl[feat_ID == feat_i, unique_split(GO)]
  })

  names(out_list) = tbl[, feat_ID]

  return(out_list)

})






# Comments ####

## Set ####

#' @export
setMethod('comments<-', signature(x = 'prbList', feats = 'character', value = 'character'),
          function(x, feats, value, mode = c('append', 'replace')) {

            mode = match.arg(mode, choices = c('append', 'replace'))

            if(mode == 'append') {
              x = annotate_feats(x = x, term = value, feats = feats, type = 'comment')
            }
            if(mode == 'replace') {
              x[][feat_ID %in% feats, 'comments'] = unique_collapse(value,
                                                                    collapse_separator = sep(x))
              x = initialize(x)
            }

            return(x)
          })



## Extract ####
#' @export
setMethod('comments', signature('prbList'), function(x, feats) {
  comm_tbl = x[][feat_ID %in% feats, comments, by = feat_ID]

  out_list = lapply(comm_tbl[, feat_ID], function(feat_i) {
    comm_tbl[feat_ID == feat_i, unique_split(comments)]
  })

  names(out_list) = comm_tbl[, feat_ID]

  return(out_list)

})




