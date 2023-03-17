
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





# Additional Info ####

setMethod('attrValues', signature(x = 'prbList', attr = 'character', feats = 'character'),
          function(x, attr, feats) {
            getAttrValues(x = x, attr = attr, feats = feats)
          })

setMethod('attrValues', signature(x = 'prbList', attr = 'character', feats = 'missing'),
          function(x, attr, panel_only = TRUE) {
            feats = featIDs(x, panel_only = panel_only)
            getAttrValues(x = x, attr = attr, feats = feats)
          })

getAttrValues = function(x, attr, feats) {
  attr_vals = x[][feats, attr, with = FALSE]
  res_tbl = data.table::data.table(feat_ID = feats)
  res_tbl = cbind(res_tbl, attr_vals)
  return(res_tbl)
}



setMethod('attrValues<-', signature(x = 'prbList', attr = 'character', feats = 'character', value = 'ANY'),
          function(x, attr, feats, value) {
            x[][feats, attr] = value
            x
          })



#' @param x prbList
#' @param value_tbl dataframe of values with a feature ID column
#' @param ID_col colname of the ID column
setAttrValues = function(x, attr, value_tbl, ID_col = 'feat_ID') {

  if(!ID_col %in% colnames(value_tbl)) stop('ID_col not found in values table')

  if(ncol(value_tbl) != 2L) stop(wrap_txt('values table required to be 2 cols.
                                         col1 for IDs and col2 for values',
                                         errWidth = TRUE))

  ab_name = colnames(value_tbl)[colnames(value_tbl) != ID_col]

  data.table::setDT(value_tbl)
  data.table::setkeyv(value_tbl, ID_col)

  intersect_names = intersect(value_tbl[[ID_col]], featIDs(x, panel_only = FALSE))

  if(!all(intersect_names %in% featIDs(x, panel_only = FALSE))) {
    warning(wrap_txt('Not all features found in prbList object.
                     Missing features ignored:'))
    print(intersect_names[!intersect_names %in% featIDs(x, panel_only = FALSE)])
  }

  intersect_ab = value_tbl[intersect_names, ab_name, with = FALSE]

  x[][intersect_names, (attr) := intersect_ab]

  x
}




