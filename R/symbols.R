
# For work with gene symbols

#' @name validate_gene_names
#' @title Test for non-official gene names
#' @description Test feature IDs to see if they are the official gene names using
#' limma::alias2Symbol. If they are, TRUE is returned. Otherwise, elements that
#' are only in the non-corrected list and those only in the corrected list will
#' be printed.
#' @param x a prbList
#' @param species species to draw gene reference from
validate_gene_names = function(x, species = 'Hs') {
  old_IDs = featIDs(x)
  official_IDs = limma::alias2Symbol(alias = old_IDs,
                                     species = species)
  if(!all(old_IDs %in% official_IDs)) {
    message('Not all gene IDs are official')
    old_only = old_IDs[!old_IDs %in% official_IDs]
    off_only = official_IDs[!official_IDs %in% old_IDs]
    message('> old IDs only')
    print(old_only)
    message('> official IDs only')
    print(off_only)
  }
  else return(TRUE)
}
