#' @include generics.R
#' @export
setMethod('copy', signature('prbList'), function(x) {
  x[] = data.table::copy(x[])
  x
})
