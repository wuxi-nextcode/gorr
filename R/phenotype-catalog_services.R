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
    assertthat::assert_that(class(playlist) == "playlist" || is.null(playlist))

    if (!is.null(playlist)) {
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
        purrr::map(Phenotype, conn = conn)

    PhenotypeList(phenotypes, conn=conn)
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
get_phenotypes_dataframe <- function(conn,
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
    assertthat::assert_that(class(playlist) == "playlist" || is.null(playlist))

    if (!is.null(playlist)) {
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
                                do.call(rbind, .) %>%
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

    url <- gorr__get_endpoint(conn, "phenotype-catalog", "phenotypes") %>%
            paste(name, sep = "/")

    resp <- gorr__api_request("GET", url = url, conn = conn)

    Phenotype(resp$phenotype, conn = conn)
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
#' @param data data.frame or list of phenotype PN values to be applied to `phenotype_upload_data` (optional)
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
    function(name, result_type, conn, description = NULL, url=NULL, category = NULL, query = NULL, tags = NULL, data=NULL) {
        assertthat::assert_that(is.string(name))
        assertthat::assert_that(is.string(result_type))
        assertthat::assert_that(class(conn) == "platform_connection")
        assertthat::assert_that(is.string(description) || is.null(description))
        assertthat::assert_that(is.string(url) || is.null(url))
        assertthat::assert_that(is.string(query) || is.null(query))
        assertthat::assert_that(is.character(tags)  || is.null(tags))

        SUPPORTED_RESULT_TYPES <- c("SET", "QT", "CATEGORY")

        if (!is.null(tags)) {
            tags <- purrr::map(tags, ~base::strsplit(.x, ",", fixed = TRUE)) %>%
                unlist() %>%
                as.list()
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

        resp <- gorr__api_request("POST", url = uri, body = content, conn = conn)
        pheno <- Phenotype(resp$phenotype, conn = conn)

        if (!is.null(data)) {
            try(
                phenotype_upload_data(data = data, phenotype = pheno)
            )
        }

        pheno
    }

#--- Phenotype Playlists


#' A list of all the playlists in the current project.
#'
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#' @param limit Maximum number of results (default: 100)
#'
#' @return List of playlists
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' playlists <- get_playlists(conn)
#' }
get_playlists <- function(conn, limit = 100) {
    assertthat::assert_that(class(conn) == "platform_connection")
    assertthat::assert_that(is.numeric(limit))

    url <- gorr__get_endpoint(conn, "phenotype-catalog", "self") %>%
            paste("playlists", sep="/")

    content <- list(limit = limit)

    resp <- gorr__api_request("GET", url = url, query = content, conn = conn)

    fetch__from_lst(resp$playlists, "name")
}


#' Get playlist in the current project based on the playlist's name OR id.
#'
#' @param name Playlist name
#' @param id Playlist id
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return A playlist object
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' playlist <- get_playlist(id=1, conn)
#' }
get_playlist <- function(name = NULL, id = NULL, conn) {
    assertthat::assert_that(class(conn) == "platform_connection")

    if (is.null(name) && is.null(id)) {
        stop("Name OR id must be supplied")
    } else if (!is.null(name) && !is.null(id)) {
        stop("Name and id cannot both be supplied")
    }


    url <- gorr__get_endpoint(conn, "phenotype-catalog", "self") %>%
            paste("playlists", sep="/")

    if (!is.null(id)) {
        url <- paste(url, id, sep = "/")
    }

    resp <- gorr__api_request("GET", url = url, query = list(name = name), conn = conn)
    # This is needed as response is different because of different enpoints when querying by id or name
    if (!is.null(id)) {
        return(PhenotypePlaylist(resp$playlist, conn = conn))
    }

    PhenotypePlaylist(resp$playlist[[1]], conn = conn)
}


#' Create a new playlist in the current project.
#'
#' @param name Unique (lowercase) playlist name in the project
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#' @param description Free text description of the playlist (optional)
#' @param phenotypes comma seperated string of phenotypes to  add (optional) eg. "pheno1,pheno2"  or character vector c("pheno1", "pheno2")
#'
#' @return A playlist object
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' name <- "testpl"
#' playlist <- create_playlist(name = name, conn)
#' }
create_playlist <- function(name, conn, description=NULL, phenotypes=NULL) {
    assertthat::assert_that(is.string(name))
    assertthat::assert_that(class(conn) == "platform_connection")

    if (!is.null(description)) {
        assertthat::assert_that(is.string(description))
    }
    if (!is.null(phenotypes)) {
        assertthat::assert_that(is.character(phenotypes))
        phenotypes <- purrr::map(phenotypes, ~base::strsplit(.x, ",", fixed = TRUE)) %>%
            unlist() %>%
            as.list()
    }

    url <- gorr__get_endpoint(conn, "phenotype-catalog", "self") %>%
            paste("playlists", sep="/")
    payload <- list(playlist = list(name = name,
                                    description = description,
                                    phenotypes = phenotypes))

    resp <- gorr__api_request("POST", url = url, body = payload, conn = conn)
    PhenotypePlaylist(resp$playlist, conn = conn)
}


#--- Phenotype Matrix


#' Get a phenotype matrix object.
#'
#' @param base Optional name of base set
#' @param ... named arguments passed to `get_phenotypes` for populating matrix.
#'
#' @return a phenotype matrix object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' phenotype_mat <- get_phenotype_matrix()
#' }
get_phenotype_matrix <- function(base = NULL, ...) {
    dots <- rlang::dots_list(...)
    phemat <- PhenotypeMatrix(base = base)
    if (length(dots)>0) {
        if (!("conn" %in% names(dots))) gorr__failure("Please provide connector object for populating matrix using `get_phenotypes`")
        phenotypes = get_phenotypes(...)
        phemat <- phemat_add_phenotypes(names = purrr::map_chr(phenotypes, ~.x$name), phemat)
    }
    phemat
}


#' Get a phenotype matrix object.
#'
#' @param conn platform connection structure, create it using \code{\link{platform_connect}}
#' @param base Optional name of base set
#' @param ... named arguments passed to `get_phenotypes` for populating matrix.
#'
#' @return a phenotype matrix object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' phenotype_mat <- get_phenotype_matrix()
#' }
create_phenotype_matrix <- function(conn, base = NULL, ...) {
    dots <- rlang::dots_list(...)
    phemat <- PhenotypeMatrix(base = base, conn = conn)
    if (length(dots)>0) {
        phenotypes = get_phenotypes(..., conn = conn)
        phemat <- phemat_add_phenotypes(names = purrr::map_chr(phenotypes, ~.x$name), phemat)
    }
    phemat
}


#' Get all tags
#'
#' Get all tags available in the catalog
#'
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return A list of phenotype tags
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' tags <- get_tags(conn)
#' }
get_tags <- function(conn) {
    assertthat::assert_that(class(conn) == "platform_connection")

    url <-  gorr__get_endpoint(conn, "phenotype-catalog", "tags")
    resp <- gorr__api_request("GET", url, conn = conn)

    fetch__from_lst(resp$tags, "name")
}


#-------- Phenotype Categories

#' A list of all categories available in the system.
#'
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @return List of categories
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' phenotypes <- get_categories(conn)
#' }
get_categories <- function(conn) {
    assertthat::assert_that(class(conn) == "platform_connection")

    url <- paste(gorr__get_endpoint(conn, "phenotype-catalog", "projects"), conn$project, "categories", sep="/")

    resp <- gorr__api_request("GET",
                              url = url,
                              conn = conn)

    fetch__from_lst(resp$categories, "name")
}


#' Add a new category to this project.
#'
#' @param name Unique (lowercase) category name in the project
#' @param conn gor connection structure, create it using  \code{\link{platform_connect}}
#'
#' @return a list of category attrbues
#'
#' @importFrom assertthat is.string
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' name <- "testCat1"
#' category <- create_category(name, conn)
#' }
create_category <- function(name, conn) {
    assertthat::assert_that(is.string(name))
    assertthat::assert_that(class(conn) == "platform_connection")

    url <- paste(gorr__get_endpoint(conn, "phenotype-catalog", "projects"), conn$project, "categories", sep="/")

    content <- list(name = name)

    resp <- gorr__api_request("POST",
                              url = url,
                              body = content,
                              conn = conn)
    resp$category
}

#' Delete a phenotype category
#'
#' @param name phenotype category name in the project
#' @param conn gor connection structure, create it using \code{\link{platform_connect}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' name <- "testCat"
#' category_delete(name, conn)
#' }
category_delete <- function(name, conn) {
    url <- paste(gorr__get_endpoint(conn, "phenotype-catalog", "projects"), conn$project, "categories", name, sep="/")

    gorr__api_request("DELETE",
                      url = url,
                      conn = conn,
                      parse.body = F)
}

