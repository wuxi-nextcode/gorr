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

new_phenotype <- function(phenotype) {
    structure(phenotype, class = "phenotype")
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
phenotype <- function(phenotype) {
    new_phenotype(phenotype) %>%
        validate_phenotype()
}

#' A list of all the phenotypes in the current project.
#'
#' @param conn gor connection structure, create it using \code{\link{gor_connect}}
#' @param tags Optional character, character vector or list of tags to filter for.
#' @param limit Maximum number of results (default: 100)
#'
#' @return List of phenotypes
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- phenotype_connect(api_key, project)
#' phenotypes <- get_phenotypes(conn)
#' }
get_phenotypes <- function(conn,
                           tags = list(),
                           limit = 100) {
    assertthat::assert_that(class(conn) == "gor_connection")
    assertthat::assert_that(is.numeric(limit))
    if (!is.null(tags)) {
        assertthat::assert_that(is.list(tags) | is.character(tags))
    }


    url <- get__url_from_conn(conn, "phenotypes")

    content <- list(with_all_tags = as.list(tags),
                    limit = limit)

    resp <- gorr__api_request("GET",
                              url = url,
                              query = content,
                              conn = conn)

    fetch__from_lst(resp$phenotypes, "name")
}


#' Get a specific phenotype in the current project.
#'
#' @param name Unique (lowercase) phenotype name in the project
#' @param conn gor connection structure, create it using \code{\link{phenotype_connect}} or  \code{\link{gor_connect}}
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
#' conn <- phenotype_connect(api_key, project)
#' name <- "height"
#' phenotype <- get_phenotype(name, conn)
#' }
get_phenotype <- function(name, conn) {
    assertthat::assert_that(is.character(name))
    assertthat::assert_that(class(conn) == "gor_connection")

    url <-
        paste(get__url_from_conn(conn, "phenotypes"), name, sep = "/")

    resp <- gorr__api_request("GET", url = url, conn = conn)

    phenotype(resp$phenotype)
}

#' Create a new phenotype in the current project.
#'
#' @param name Unique (lowercase) phenotype name in the project
#' @param result_type Type of phenotype (supported types: "SET", "QT" and "CATEGORY")
#' @param conn gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{gor_connect}}
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
#' conn <- phenotype_connect(api_key, project)
#' name <- "height"
#' result_type <- "QT"
#' description <- "Height of individuals"
#' phenotype <- create_phenotype(name, result_type, conn, description)
#' }
create_phenotype <-
    function(name, result_type, conn, description = NULL, url=NULL, category = NULL, query = NULL, tags = NULL) {
        assertthat::assert_that(is.string(name))
        assertthat::assert_that(is.string(result_type))
        assertthat::assert_that(class(conn) == "gor_connection")

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

        uri <- get__url_from_conn(conn, "phenotypes")

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

        phenotype(resp$phenotype)
    }

#' Update the phenotype with a new description
#'
#' @param description phenotype description
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{gor_connect}}
#'
#' @return an updated list with the phenotype object
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- phenotype_connect(api_key, project)
#' name <- "height"
#' phenotype <- get_phenotype(name, conn)
#' description <- "individual height"
#' phenotype <- update_phenotype_desc(description, phenotype, conn)
#' }
phenotype_update_description <-
    function(description, phenotype, conn) {
        assertthat::assert_that(is.character(description))
        assertthat::assert_that(class(phenotype) == "phenotype")
        assertthat::assert_that(class(conn) == "gor_connection")

        #  Update the phenotype with a new description
        url <- get__link(phenotype, "self")
        content <- list(description = description)

        resp <-
            gorr__api_request("PATCH",
                              url = url,
                              body = content,
                              conn = conn)

        phenotype(resp$phenotype)
    }

#' Upload phenotype data
#'
#' The data is expected to be a list of lists.
#' e.g. `data <- list(list('a'), list('b'))`.
#' The `result_type` of the phenotype dictates
#' if each sublist should contain one or two items.
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param data a list of lists to be uploaded
#' @param conn gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{gor_connect}}
#'
#' @export
phenotype_upload_data <- function(phenotype, data, conn) {
    assertthat::assert_that(class(phenotype) == "phenotype")
    assertthat::assert_that(is.list(data))
    assertthat::assert_that(class(conn) == "gor_connection")

    url <- get__link(phenotype, "upload")
    content <- list(data = data)

    gorr__api_request("POST",
                      url,
                      body = content,
                      conn = conn,
                      parse.body = F) %>%
        httr::stop_for_status()
}

#' Delete a phenotype, including all data from a project
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{gor_connect}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- phenotype_connect(api_key, project)
#' name <- "height"
#' phenotype <- get_phenotype(name, conn)
#' phenotype_delete(phenotype, conn)
#' }
phenotype_delete <- function(phenotype, conn) {
    url <- get__link(phenotype, "self")

    gorr__api_request("DELETE",
                      url = url,
                      conn = conn,
                      parse.body = F)
}

#' Refresh phenotype
#'
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{gor_connect}}
#'
#' @return a list with the phenotype object
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- phenotype_connect(api_key, project)
#' name <- "height"
#' result_type <- "QT"
#' description <- "Height of individuals"
#' phenotype <- create_phenotype(name, result_type, conn, description)
#' phenotype <- phenotype_refresh(phenotype, conn)
#' }
phenotype_refresh <- function(phenotype, conn) {
    assertthat::assert_that(class(phenotype) == "phenotype")
    assertthat::assert_that(class(conn) == "gor_connection")

    url <- get__link(phenotype, "self")

    resp <- gorr__api_request("GET", url = url, conn = conn)

    phenotype(resp$phenotype)
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
#' conn <- phenotype_connect(api_key, project)
#' name <- "height"
#' tags <- phenotype_get_tags(phenotype, conn)
#' }
phenotype_get_tags <- function(phenotype) {
    assertthat::assert_that(class(phenotype) == "phenotype")

    phenotype$tag_list %>% unlist()
}

#' Add a new tag/s to this phenotype.
#'
#' @param tag string of tag/s to be added. Multiple tags should be seperated by comma e.g. "height,weight" or character vector "c("height", "weight")
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{gor_connect}}
#'
#' @return an updated phenotype object
#' @export
phenotype_add_tag <- function(tag, phenotype, conn) {
    assertthat::assert_that(is.character(tag))
    assertthat::assert_that(class(phenotype) == "phenotype")
    assertthat::assert_that(class(conn) == "gor_connection")

    tags <- phenotype_get_tags(phenotype)

    tag <- purrr::map(tag, ~base::strsplit(.x, ",", fixed = TRUE)) %>% unlist()

    if (all(tag %in% tags)) {
        warning(paste("Tags:", tag, "already in phenotype tag list:", tags))
        return(phenotype)
    }

    tag_lst <- base::union(tags, tag)

    url <- get__link(phenotype, "self")
    content <- list(tag_list = as.list(tag_lst))

    resp <-
        gorr__api_request(
            "PATCH",
            url,
            body = content,
            conn = conn
        )

    phenotype(resp$phenotype)
}

#' Set the tag list for this phenotype, overriding all previous tags
#'
#' @param tags string of tags to be added seperated by comma eg. "height,weight"  or character vector "c("height", "weight")
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{gor_connect}}
#'
#' @return an updated list with the phenotype object
#' @export
phenotype_set_tags <- function(tags, phenotype, conn) {
    assertthat::assert_that(is.character(tags))
    assertthat::assert_that(class(phenotype) == "phenotype")
    assertthat::assert_that(class(conn) == "gor_connection")

    tags <- purrr::map(tags, ~base::strsplit(.x, ",", fixed = TRUE)) %>% unlist()

    url <- get__link(phenotype, "self")
    content <- list(tag_list = as.list(tags))

    resp <-
        gorr__api_request(
            "PATCH",
            url,
            body = content,
            conn = conn
        )

    phenotype(resp$phenotype)
}

#' Delete a tag/s from phenotype.
#'
#' @param tag string of tag/s to be added seperated by comma eg. "height,weight"  or character vector "c("height", "weight")
#' @param phenotype phenotype structure, create or get it using \code{\link{create_phenotype}} or \code{\link{get_phenotype}}
#' @param conn gor connection structure, create it using \code{\link{phenotype_connect}} or \code{\link{gor_connect}}
#'
#' @return an updated list with the phenotype object
#' @export
phenotype_delete_tag <- function(tag, phenotype, conn) {
    assertthat::assert_that(is.string(tag))
    assertthat::assert_that(class(phenotype) == "phenotype")
    assertthat::assert_that(class(conn) == "gor_connection")

    tags <- phenotype_get_tags(phenotype)

    if (base::grepl(",", tag, fixed = TRUE)) {
        tag <- base::strsplit(tag, ",", fixed = TRUE) %>% unlist()
    }

    url <- get__link(phenotype, "self")
    tags_lst <- tags[!(tags %in% tag)]

    content <-  list(tag_list = as.list(tags_lst))
    resp <-
        gorr__api_request(
            "PATCH",
            url,
            body = content,
            conn = conn
        )

    phenotype(resp$phenotype)
}
