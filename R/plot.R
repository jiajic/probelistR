


# https://stackoverflow.com/questions/50430225/euler-diagram-with-eulerr-in-r

#' 1. pull all rows that grep match to any of the search terms
#' 2. see which GO terms match the search terms, order them, then assign them in new col
#' 3. sum counts in the new column to get values



plot_nVennR = function() {

  if(!requireNamespace('nVennR', quietly = TRUE)) {
    stop(wrap_txt('nVennR not installed.
                  Please install with "devtools::install_github("vqf/nVennR") to continue',
                  errWidth = TRUE))
  }




}




