#' Delete a tag/s from phenotype.
#' 
#' This method is depricated and will be removed in future versions. use \code{\link{phenotype_delete_tags}} instead.
#'
#' @param tag string of tag/s to be added seperated by comma eg. "height,weight"  or character vector "c("height", "weight")
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn Deprecated : gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return an updated list with the phenotype object
#' @export
phenotype_delete_tag <- function(tag, phenotype, conn=NULL) {
    .Deprecated(new = "phenotype_delete_tags")
    if (!missing(conn)) {
        deprecated_argument_msg(conn)
    }
    phenotype_delete_tags(tags = tag, phenotype = phenotype)
}

#' Add a new tag/s to this phenotype.
#'
#' This method is depricated and will be removed in future versions. use \code{\link{phenotype_add_tags}} instead.
#'
#' @param tag string or character vector of tag/s to be added. Tags should be seperated by comma e.g. "height,weight" or as vector "c("height", "weight")
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn Deprecated : gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return an updated phenotype object
#' @export
phenotype_add_tag <- function(tag, phenotype, conn=NULL) {
    .Deprecated(new = "phenotype_add_tags")
    if (!missing(conn)) {
        deprecated_argument_msg(conn)
    }
    phenotype_add_tags(tags = tag, phenotype = phenotype)
}