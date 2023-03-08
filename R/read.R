
# Data Import ####

#' #' @name fread
#' #' @title Data.table fread
#' #' @details See \code{\link(data.table){fread}}
#' #' @inheritDotParams data.table::fread
#' #' @export
#' fread = function(...) {
#'   data.table::fread(...)
#' }


#' #' @name importData
#' #' @title Import data.frame type input into prbList object
#' #' @param x prbList
#' #' @param y data.frame type input to merge
#' #' @details Merges in data and also
#' #' @export
#' importData = function(x, y) {
#'
#' }

#' @title Read clipboard as vector
#' @name readClip
readClip = function() {
  clipr::read_clip()
}


#' @title Read clipboard as table
#' @name readClipTbl
readClipTbl = function(header = TRUE, sep = '\t', stringsAsFactors = FALSE, ...) {


  clipr::read_clip_tbl(header = header,
                       sep = sep,
                       stringsAsFactors = stringsAsFactors,
                       ...) |> data.table::setDT()
}


# Data Cleaning and Initialization ####


#' @name evaluate_DF
#' @title Evaluate input dataframe and convert to correct formatting
#' @name probe_data
#' @keywords internal
#' @noRd
evaluate_DF = function(probe_data) {
  if(!inherits(probe_data, 'data.frame')) {
    stop(wrap_txt(substitute(probe_data, 'is not a data.frame-like object')))
  }

  # Convert to data.table
  probe_data = data.table::setDT(probe_data)

  # assign feat_ID col - assume first character col
  if(!'feat_ID' %in% colnames(probe_data)) {
    fid_index = which(sapply(probe_data, class) == 'character')[1]
    if(!is.na(fid_index)) {
      data.table::setnames(probe_data, old = names(fid_index), new = 'feat_ID')
    } else {
      stop('No character columns detected.\n feat_ID column cannot be automatically set.')
    }
  }

  return(probe_data)
}




# run after feat_ID has been initialized
#' @keywords internal
initialize_GO_col = function(probe_data,
                             GO_cols = NULL,
                             obj_separator = ':',
                             read_separator = obj_separator) {

  # create GO column
  probe_data = combine_cols(x = probe_data,
                            new_col = 'GO',
                            cols_to_combine = GO_cols,
                            collapse_separator = obj_separator,
                            split_separator = read_separator)

  GO_cols = GO_cols[GO_cols != 'GO']

  # remove columns used
  remove_cols(x = probe_data,
              cols_to_remove = GO_cols)

  return(probe_data)
}




# run after GO column has been initialized
#' @keywords internal
initialize_comments_col = function(probe_data,
                                   comm_cols = NULL,
                                   obj_separator = ':',
                                   read_separator = obj_separator) {
  if(!'GO' %in% colnames(probe_data)) stop('Initialize GO column first\n')

  # combine remaining columns into comments column if not specified
  if(is.null(comm_cols)) {
    data_cols = colnames(probe_data)
    comm_cols = data_cols[which(!data_cols %in% c('feat_ID', 'GO'))]
  }

  probe_data = combine_cols(x = probe_data,
                            new_col = 'comments',
                            cols_to_combine = comm_cols,
                            collapse_separator = obj_separator,
                            split_separator = read_separator)

  # remove 'comments' from comm_cols if it was used
  comm_cols = comm_cols[comm_cols != 'comments']

  # remove columns used
  remove_cols(x = probe_data,
              cols_to_remove = comm_cols)

  return(probe_data)
}




# updates by reference
#' @param x data.table
#' @param new_col new column to assign as container of the combined column info
#' @param cols_to_combine columns to combine in to the new_col
#' @param split_separator separator symbol used to read the data
#' @param collapse_separator separator symbol to use downstream
#' @keywords internal
combine_cols = function(x,
                        new_col,
                        cols_to_combine,
                        collapse_separator = ':',
                        split_separator = collapse_separator) {
  x[, eval(quote(new_col)) := unique_collapse(unlist(.SD),
                                              collapse_separator = collapse_separator,
                                              split_separator = split_separator),
    by = feat_ID,
    .SDcols = cols_to_combine]
}


# updates by reference
#' @param x data.table
#' @param cols_to_remove columns to remove from the object
remove_cols = function(x, cols_to_remove) {
  x[, eval(call(':=', cols_to_remove, NULL))]
}


# GO term manipulations ####


# This function works on the raw data input. The finalized data to be set into
# the @infoDT slot only has one GO column so it can only be collapsed down to
# that one column after this step.
#' @name evaluate_GO_cols
#' @title Determine GO terms and hierarchy based on probe_data input
#' @param probe_data dataframe type input
#' @keywords internal
#' @return vector of gene ontology terms
#' @noRd
evaluate_GO_cols = function(probe_data, GO_cols, separator = ':') {

  if(!inherits(probe_data, 'data.table')) stop('Input should be data.table\n')

  if(length(GO_cols) == 1) {
    wrap_msg('Only one GO column provided.\n',
             'Skipping hierarchy detection')
  } else {

  }


}








#' @param x prbList
#' @keywords internal
remove_term = function(x, term_to_remove) {
  GO = NULL

  # if GO_hierarchy exists
  if(length(GOHierarchy(x)) > 0) {
    # remove any associated parent
    GOHierarchy(x, term_to_remove) = NULL
    # remove any associated children
    GOHierarchy(x) = lapply(GOHierarchy(x), function(p_i) {
      p_i[p_i != term_to_remove]
    })
  }

  # if data.table exists
  if(!is.null(x[])) {
    s = sep(x)
    # remove references to term to remove
    x = copy(x)
    x[][, GO := gsub(pattern = term_to_remove, replacement = s, x = GO)]
    # cleanup stray separators
    x[] = separator_cleanup(x[], cols = 'GO', separator = s)
  }

  # regenerate GO_terms
  x = initialize(x)

  return(x)
}



#' @param x data.table
separator_cleanup = function(x, cols, separator = ':') {
  pattern = paste0('^', separator, '|', separator, '$', '|', separator, separator)

  for(j in cols) set(x, j = j, value = gsub(pattern, '', x[[j]]))

  return(x)
}

