#' Update phenotype attributes
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{get_phenotype}}
#' @param description phenotype description
#' @param tags comma separated string of tags eg. "height,weight", list or character vector "c("height", "weight")
#' @param query NOR query that defines this phenotype
#' @param category category for the phenotype (must be defined in the project - see get_categories)
#' @param url reference URL for the phenotype (to dataset or other reference)
#'
#' @return phenotype object
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' name <- "height"
#' phenotype <- get_phenotype(name, conn)
#' description <- "individual height"
#' phenotype <- update_phenotype(phenotype, description=decription)
#' }
phenotype_update <-
    function(phenotype, description=NULL, tags=list(), query=NULL, category=NULL, url=NULL) {
        assertthat::assert_that(class(phenotype) == "phenotype")
        assertthat::assert_that(is.null(description) | is.character(description))
        assertthat::assert_that(is.list(tags) | is.character(tags))
        assertthat::assert_that(is.null(query) | is.character(query))
        assertthat::assert_that(is.null(category) | is.character(category))
        assertthat::assert_that(is.null(url) | is.character(url))

        if (length(tags) > 0) {
            tags <- parse__tags(tags)
        }

        url <- get__link(phenotype, "self")
        content <- list(description = description,
                        query = query,
                        tag_list = as.list(tags),
                        category = category,
                        url = url)
        resp <-
            gorr__api_request("PATCH",
                              url = url,
                              body = content,
                              conn = attr(phenotype, which = "conn"))

        phenotype(resp$phenotype, conn = attr(phenotype, which = "conn"))
    }

#' Update the phenotype with a new description
#'
#' @param description phenotype description
#' @param phenotype phenotype structure, create or get it using \code{\link{get_phenotype}}
#' @param conn Deprecated : gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{platform_connect}}
#'
#' @return phenotype structure
#' @export
phenotype_update_description <- function(description, phenotype, conn=NULL) {
    if (!missing(conn)) {
        deprecated_argument_msg(conn)
    }

    phenotype_update(phenotype, description = description)
}


#' Update the phenotype with new query
#'
#' @param query NOR query that defines this phenotype
#' @param phenotype phenotype structure, create or get it using \code{\link{get_phenotype}}
#'
#' @return phenotype structure
#' @export
phenotype_update_query <- function(query, phenotype) {
    phenotype_update(phenotype, query = query)
}


#' Add new tag/s phenotype.
#'
#' @param tags string or character vector of tag/s to be added. Tags should be seperated by comma e.g. "height,weight" or as vector "c("height", "weight")
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#'
#' @return phenotype structure
#' @export
phenotype_add_tags <- function(tags, phenotype) {
    assertthat::assert_that(is.list(tags) | is.character(tags))
    assertthat::assert_that(class(phenotype) == "phenotype")

    pheno_tags <- phenotype_get_tags(phenotype)
    tags <- parse__tags(tags)

    if (all(tags %in% pheno_tags)) {
        warning(paste("Tags:", tags, "already in phenotype tag list:", pheno_tags))
        return(phenotype)
    }

    tag_list <- base::union(pheno_tags, tags)
    phenotype_update(phenotype, tags = tag_list)
}


#' Set the tag/s for this phenotype, overriding previous tags
#'
#' @param conn Deprecated : gor connection structure, create it using \code{\link{platform_connect}}
#' @inheritParams phenotype_add_tags
#'
#' @return phenotype structure
#' @export
phenotype_set_tags <- function(tags, phenotype, conn=NULL) {
    if (!missing(conn)) {
        deprecated_argument_msg(conn)
    }
    assertthat::assert_that(is.list(tags) | is.character(tags))
    assertthat::assert_that(class(phenotype) == "phenotype")

    phenotype_update(phenotype, tags = tags)
}


#' Delete tag/s from phenotype.
#'
#' @inheritParams phenotype_add_tags
#'
#' @return phenotype structure
#' @export
phenotype_delete_tags <- function(tags, phenotype) {
    assertthat::assert_that(is.list(tags) | is.character(tags))
    assertthat::assert_that(class(phenotype) == "phenotype")

    pheno_tags <- phenotype_get_tags(phenotype)
    tags <- parse__tags(tags)
    tags_list <- pheno_tags[!(pheno_tags %in% tags)]

    phenotype_update(phenotype, tags = tags_list)
}
