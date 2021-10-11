validate_phenotype <- function(phenotype) {
    values <- unclass(phenotype)
    req_names <- c(
        "project_key",
        "name",
        "description",
        "result_type",
        "links",
        "tag_list"
    )

    req_links <- c("self",
                   "upload")

    link_names <- names(get("links", values))
    links <- get("links", values) %>% unlist()

    if (!all(req_names %in% names(values))) {
        stop("Phenotype must include the following property: ",
                paste0(req_names, collapse = ", "),
                call. = FALSE)
    }

    if (!all(all(req_links %in% link_names) &
             (!is.null(values$links[["self"]]) &
              !is.null(values$links[["self"]])))) {
        stop("Phenotype links must include a valid 'self' and 'upload' links",
                call. = FALSE)
    }

    phenotype
}

new_phenotype <- function(phenotype, conn) {
    structure(phenotype, class = "phenotype", conn = conn)
}


# Phenotype class constructor
# A local object representing a phenotype response from the phenotype
# catalog service.
#
# Note that most of the attributes come directly from the phenotype
# serverside response and are therefore not documented directly here.
# Please refer to the API Documentation for the phenotype catalog service.
#
# In addition to the ones documented here, this object has at least these attributes:
#
# * name - Phenotype name
# * description - Textual description of this phenotype
# * result_type - Type of result. Cannot be changed. One of SET, QT, CATEGORY
# * created_at - Timestamp when the phenotype was first created
# * updated_at - Timestamp when the phenotype was last updated
# * created_by - Username who created the phenotype
# * versions - List of data versions available in this phenotype
phenotype <- function(phenotype, conn) {
    new_phenotype(phenotype, conn) %>%
        validate_phenotype()
}

#' @export
print.phenotype <- function(x, ...) {

    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    cli::cat_rule(left = ("Phenotype"))

    bullet("$name: ", x$name)
    bullet("$description: ", x$description)
    bullet("$result_type: ", x$result_type)
    bullet("$tag_list: ", paste(x$tag_list, collapse = ", "))
    bullet("$pn_count: ", x$pn_count)
    bullet("$query: ", x$query)
}

# Internal method to be called by `get_phenotypes`, `get_phenotypes_matrix` and `get_phenotypes_dataframe`
# See documentation of those methods for more details
query__phenotypes <- function(conn, all_tags, any_tags, pn_count, categories, limit, states, search, updated_at, result_types, names) {
    assertthat::assert_that(class(conn) == "platform_connection")
    assertthat::assert_that(is.numeric(limit))
    assertthat::assert_that(is.list(all_tags) | is.character(all_tags))
    assertthat::assert_that(is.list(any_tags) | is.character(any_tags))
    assertthat::assert_that(is.null(pn_count) | is.character(pn_count))
    assertthat::assert_that(is.list(categories) | is.character(categories))
    assertthat::assert_that(is.list(states) | is.character(states))
    assertthat::assert_that(is.null(search) | is.character(search))
    assertthat::assert_that(is.null(updated_at) | is.character(updated_at))
    assertthat::assert_that(is.list(result_types) | is.character(result_types))
    assertthat::assert_that(is.list(names) | is.character(names))

    url <- gorr__get_endpoint(conn, "phenotype-catalog", "phenotypes")

    content <- list(with_all_tags = paste(all_tags, collapse = ","),
                    with_any_tags = paste(any_tags, collapse = ","),
                    pn_count = pn_count,
                    category = paste(categories, collapse = ","),
                    limit = limit,
                    state = paste(states, collapse = ","),
                    search = search,
                    updated_at = updated_at,
                    result_type = paste(result_types, collapse = ","),
                    names = paste(names, collapse = ","))

    resp <- gorr__api_request("GET",
                              url = url,
                              query = content,
                              conn = conn)
    resp$phenotypes
}


#' A list of all/subset of phenotypes in the current project.
#'
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#' @param tags Deprecated : Optional character, character vector or list of tags to filter for.
#' @param limit Maximum number of results fetched (default: 100)
#' @param pheno_names Only fetch phenotypes that are in a given list of phenotype names
#' @param all_tags Only fetch phenotypes that have all tags in the given list of tags
#' @param any_tags Fetch phenotypes that have any of the tags in the given list of tags
#' @param pn_count Only list phenotypes that match the given pn counts. (include greater or less symbol in string eg ">10")
#' @param categories Only fetch phenotypes in the given list of categories
#' @param states Only fetch phenotypes in the given list of states
#' @param search String of keywords to search for in phenotypes, such as name, categories and tags
#' @param playlist Fetch a specific playlist of phenotypes by the playlist id
#' @param updated_at Only fetch phenotypes that match the given dates. Example: >=2017-04-01 ┃ <=2012-07-04 ┃ 2016-04-30..2016-07-04
#' @param result_types Only fetch phenotypes in the given list of result types
#'
#' @return List of phenotypes
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' phenotypes <- get_phenotypes(conn, limit=5)
#' }
get_phenotypes <- function(conn,
                           tags = list(),
                           limit = 100,
                           pheno_names=list(),
                           all_tags=list(),
                           any_tags=list(),
                           pn_count=NULL,
                           categories=list(),
                           states=list(),
                           search=NULL,
                           playlist=NULL,
                           updated_at=NULL,
                           result_types=list()) {
    if (!missing(tags)) {
        deprecated_argument_msg(tags, custom = "'any_tags' arguments used instead") %>%
        warning()
        any_tags <- append(any_tags, tags)
    }
    assertthat::assert_that(class(conn) == "platform_connection")

    if (!is.null(playlist)) {
        assertthat::assert_that(class(playlist) == "playlist")
        pheno_names <- names(playlist$phenotypes)
    }

    phenotypes <- query__phenotypes(conn = conn,
                                    all_tags = all_tags,
                                    any_tags = any_tags,
                                    pn_count = pn_count,
                                    categories = categories,
                                    limit = limit,
                                    states = states,
                                    search = search,
                                    updated_at = updated_at,
                                    result_types = result_types,
                                    names = pheno_names) %>%
        purrr::map(phenotype, conn = conn)

    attr(phenotypes, "names") <- phenotypes %>% purrr::map_chr(~.x$name)

    structure(phenotypes, class = "phenotype_list", conn = conn)
}


#' @export
print.phenotype_list <- function(x, ...) {

    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    cli::cat_rule(left = ("Phenotypes"))

    if (length(attr(x, "names")) == 0) {
        cli::cat_line("  None")
    } else {
        for (name in attr(x, "names")) {
            bullet("$", name)
        }
    }
}

#' A dataframe of all/subset of phenotypes in the current project.
#'
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#' @param pheno_names Only fetch phenotypes that are in a given list of phenotype names
#' @param all_tags Only fetch phenotypes that have all tags in the given list of tags
#' @param any_tags Fetch phenotypes that have any of the tags in the given list of tags
#' @param pn_count Only list phenotypes that match the given pn counts. (include greater or less symbol in string eg ">10")
#' @param categories Only fetch phenotypes in the given list of categories
#' @param limit Maximum number of results (default: 100)
#' @param states Only fetch phenotypes in the given list of states
#' @param search String of keywords to search for in phenotypes, such as name, categories and tags
#' @param playlist Fetch a specific playlist of phenotypes by the playlist id
#' @param updated_at Only fetch phenotypes that match the given dates. Example: >=2017-04-01 ┃ <=2012-07-04 ┃ 2016-04-30..2016-07-04
#' @param result_types Only fetch phenotypes in the given list of result types
#' @param filtered boolean - Return subset of phenotype info columns. filtered=FALSE returns everything. Default: TRUE
#'
#' @return Dataframe of phenotype/s info
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' phenotypes <- get_phenotypes_dataframe(conn, limit=5)
#' }
get_phenotypes_dataframe <- function(conn = conn,
                                     pheno_names=list(),
                                     all_tags = list(),
                                     any_tags = list(),
                                     pn_count = NULL,
                                     categories = list(),
                                     limit = 100,
                                     states = list(),
                                     search = NULL,
                                     playlist = NULL,
                                     updated_at = NULL,
                                     result_types = list(),
                                     filtered=TRUE) {
    assertthat::assert_that(is.list(pheno_names) | is.character(pheno_names))
    assertthat::assert_that(is.logical(filtered))


    if (!is.null(playlist)) {
        assertthat::assert_that(class(playlist) == "playlist")
        pheno_names <- names(playlist$phenotypes)
    }

    phenotypes <- query__phenotypes(conn = conn,
                                    all_tags = all_tags,
                                    any_tags = any_tags,
                                    pn_count = pn_count,
                                    categories = categories,
                                    limit = limit,
                                    states = states,
                                    search = search,
                                    updated_at = updated_at,
                                    result_types = result_types,
                                    names = pheno_names)


    phenotypes_dataframe <- phenotypes %>%
        do.call(rbind, .)  %>%
        as.data.frame()

    if (nrow(phenotypes_dataframe) == 0) {
        return(phenotypes_dataframe)
    }

    if (filtered) {
        cols <- c("name", "description", "result_type", "tag_list", "pn_count")
        phenotypes_dataframe <- phenotypes_dataframe %>% dplyr::select(tidyselect::all_of(cols))
    }

    phenotypes_dataframe
}

#' Get a specific phenotype in the current project.
#'
#' @param name Unique (lowercase) phenotype name in the project
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return a list with the phenotype object
#'
#' @importFrom assertthat is.string
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' name <- "height"
#' phenotype <- get_phenotype(name, conn)
#' }
get_phenotype <- function(name, conn) {
    assertthat::assert_that(is.character(name))
    assertthat::assert_that(class(conn) == "platform_connection")

    url <-
        paste(gorr__get_endpoint(conn, "phenotype-catalog", "phenotypes"), name, sep = "/")

    resp <- gorr__api_request("GET", url = url, conn = conn)

    phenotype(resp$phenotype, conn = conn)
}



#' Create a new phenotype in the current project.
#'
#' @param name Unique (lowercase) phenotype name in the project
#' @param result_type Type of phenotype (supported types: "SET", "QT" and "CATEGORY")
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#' @param description Optional Phenotype description
#' @param url Reference URL for the phenotype (to dataset or other reference)
#' @param category Enter the category for the phenotype (must be defined in the project - see get_categories) (optional)
#' @param query NOR query that defines this phenotype (optional)
#' @param tags comma separated string of tags eg. "height,weight" or character vector "c("height", "weight") (optional)
#'
#' @return a list with the phenotype object
#'
#' @importFrom assertthat is.string
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' name <- "height"
#' result_type <- "QT"
#' description <- "Height of individuals"
#' phenotype <- create_phenotype(name, result_type, conn, description)
#' }
create_phenotype <-
    function(name, result_type, conn, description = NULL, url=NULL, category = NULL, query = NULL, tags = NULL) {
        assertthat::assert_that(is.string(name))
        assertthat::assert_that(is.string(result_type))
        assertthat::assert_that(class(conn) == "platform_connection")

        SUPPORTED_RESULT_TYPES <- c("SET", "QT", "CATEGORY")

        if (!is.null(description)) {
            assertthat::assert_that(is.string(description))
        }
        if (!is.null(url)) {
            assertthat::assert_that(is.string(url))
        }
        if (!is.null(query)) {
            assertthat::assert_that(is.string(query))
        }
        if (!is.null(tags)) {
            assertthat::assert_that(is.character(tags))
            tags <- purrr::map(tags, ~base::strsplit(.x, ",", fixed = TRUE)) %>%
                unlist() %>%
                as.list()
        }

        if (!(name == base::tolower(name))) {
            warning("name of phenotype should be lowercase - converting to lowercase")
            name <- base::tolower(name)
        }

        result_type <- toupper(result_type)

        if (!(result_type %in% SUPPORTED_RESULT_TYPES)) {
            stop('result_tupe should be one of : "SET", "QT" and "CATEGORY"')
        }

        uri <- gorr__get_endpoint(conn, "phenotype-catalog", "phenotypes")

        content <- list(name = name,
                        result_type = result_type,
                        description = description,
                        url = url,
                        category = category,
                        query = query,
                        tag_list = tags)

        resp <-
            gorr__api_request("POST",
                              url = uri,
                              body = content,
                              conn = conn)

        phenotype(resp$phenotype, conn = conn)
    }


#' Delete a phenotype, including all data from a project
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn Deprecated : gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' name <- "height"
#' phenotype <- get_phenotype(name, conn)
#' phenotype_delete(phenotype, conn)
#' }
phenotype_delete <- function(phenotype, conn=NULL) {
    if (!missing(conn)) {
        deprecated_argument_msg(conn) %>%
        warning()
    }
    assertthat::assert_that(class(phenotype) == "phenotype")
    assertthat::assert_that(class(attr(phenotype, which = "conn")) == "platform_connection")

    url <- get__link(phenotype, "self")

    gorr__api_request("DELETE",
                      url = url,
                      conn = attr(phenotype, which = "conn"),
                      parse.body = F)
}


#' Refresh phenotype
#'
#' Fetches current (up-to-date) phenotype version from project.
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn Deprecated : gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return phenotype structure
#' @export
phenotype_refresh <- function(phenotype, conn=NULL) {
    if (!missing(conn)) {
        deprecated_argument_msg(conn) %>%
        warning()
    }
    assertthat::assert_that(class(phenotype) == "phenotype")
    assertthat::assert_that(class(attr(phenotype, which = "conn")) == "platform_connection")

    url <- get__link(phenotype, "self")
    resp <- gorr__api_request("GET", url = url, conn = attr(phenotype, which = "conn"))

    phenotype(resp$phenotype, conn = attr(phenotype, which = "conn"))
}

#' Retrieve all tags for phenotype
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#'
#' @return a list of phenotype tags
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' phenotype <- get_phenotype(name="height", conn)
#' tags <- phenotype_get_tags(phenotype)
#' }
phenotype_get_tags <- function(phenotype) {
    assertthat::assert_that(class(phenotype) == "phenotype")

    phenotype$tag_list %>% unlist()
}

# Temporary deprecated argument handler
# msg of form "[arg_name] argument deprecated [- optional custom message]"
deprecated_argument_msg <- function(arg, custom=NULL) {
    deparse(substitute(arg)) %>%
    paste("argument deprecated", if (!is.null(custom)) paste("-", custom))
}
