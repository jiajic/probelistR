

# GOHierarchy ####

#' @title Gene ontology hierarchy
#' @name GOHierarchy-generic
#' @description Get and set gene ontology hierarchy
#' @param x prbList object
#' @param parent parent GO term
#' @param value children GO terms to set
#' @aliases GOHierarchy, GOHierarchy<-
NULL

#' @rdname GOHierarchy-generic
#' @export
setMethod('GOHierarchy',
          signature(x = 'prbList',
                    parent = 'character'),
          function(x, parent) {
            (x@GO_hierarchy[[parent]])
          })



#' @rdname GOHierarchy-generic
#' @export
setMethod('GOHierarchy',
          signature(x = 'prbList',
                    parent = 'missing'),
          function(x) x@GO_hierarchy
          )



#' @rdname GOHierarchy-generic
#' @export
setMethod('GOHierarchy<-',
          signature(x = 'prbList',
                    parent = 'missing'),
          function(x, value) {
            initialize(x, GO_hierarchy = value)
          })



#' @rdname GOHierarchy-generic
#' @export
setMethod('GOHierarchy<-',
          signature(x = 'prbList',
                    parent = 'character'),
          function(x, parent, value) {
          x@GO_hierarchy[[parent]] = value
          return(initialize(x))
         })






