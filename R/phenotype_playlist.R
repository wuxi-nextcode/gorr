validate_playlist <- function(playlist) {
    values <- unclass(playlist)

    req_names <- c(
        "project_key",
        "name",
        "description",
        "links",
        "phenotypes"
    )

    req_links <- c("self")

    link_names <- names(get("links", values))
    links <- get("links", values) %>% unlist()

    if (!all(req_names %in% names(values))) {
        stop("Playlist must include the following properties: ",
                paste0(req_names, collapse = ", "),
                call. = FALSE)
    }

    if (!all(all(req_links %in% link_names) &
             (!is.null(values$links[["self"]]) &
              !is.null(values$links[["self"]])))) {
        stop("Playlist links must include a valid 'self' link",
                call. = FALSE)
    }

    playlist
}

new_playlist <- function(playlist, conn) {
    structure(playlist, class = "playlist", conn = conn)
}


# Playlist class constructor
# A local object representing a playlist response from the phenotype
# catalog service.
#
# Please refer to the API Documentation for the phenotype catalog service.
#
# In addition to the ones documented here, this object has at least these attributes:
#
# * name - Playlist name
# * description - Textual description of this playlist
# * created_at - Timestamp when the playlist was first created
# * updated_at - Timestamp when the playlist was last updated
# * created_by - Username who created the playlist
# * versions - List of data versions available in this playlist
playlist <- function(playlist, conn) {
    # Set playlist$phenotypes to be phenotype objects
    playlist$phenotypes <- playlist$phenotypes %>%
        purrr::map(gorr:::phenotype, conn = conn)
    attr(playlist$phenotypes, "names") <- playlist$phenotypes %>% purrr::map_chr(~.x$name)

    new_playlist(playlist, conn) %>%
        validate_playlist()
}

#' @export
print.playlist <- function(x, ...) {

    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    cli::cat_rule(left = ("Phenotype playlist"))

    bullet("$name: ", x$name)
    bullet("$id: ", x$id)
    bullet("$description: ", x$description)
    bullet("$phenotypes: ", paste(names(x$phenotypes), collapse= ", "))
    bullet("$created_by: ", x$created_by)
}

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

    url <- paste(gorr__get_endpoint(conn, "phenotype-catalog", "projects"), conn$project, "playlists", sep="/")

    content <- list(limit = limit)

    resp <- gorr__api_request("GET",
                              url = url,
                              query = content,
                              conn = conn)

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


    url <- paste(gorr__get_endpoint(conn, "phenotype-catalog", "projects"), conn$project, "playlists", sep="/")

    if (!is.null(id)) {
        url <- paste(url, id, sep = "/")
    }

    resp <- gorr__api_request("GET",
                              url = url,
                              query = list(name = name),
                              conn = conn)
    # This is needed as response is different because of different enpoints when querying by id or name
    if (!is.null(id)) {
        return(playlist(resp$playlist, conn = conn))
    }

    playlist(resp$playlist[[1]], conn = conn)
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

    url <- paste(gorr__get_endpoint(conn, "phenotype-catalog", "projects"), conn$project, "playlists", sep="/")
    payload <- list(playlist = list(name = name,
                                    description = description,
                                    phenotypes = phenotypes))

    resp <-
        gorr__api_request("POST",
                          url = url,
                          body = payload,
                          conn = conn)

    playlist(resp$playlist, conn = conn)
}



#' Add phenotype/s to a playlist.
#'
#' @param name comma seperated string of phenotypes existing in project to add eg. "pheno1,pheno2"  or character vector c("pheno1", "pheno2")
#' @param playlist playlist structure, create or get it using \code{\link{create_playlist}} or \code{\link{get_playlist}}
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
#' name <- "testpl"
#' playlist <- create_playlist(name = name, conn)
#' playlist <- playlist_add_phenotype("pheno1", playlist, conn)
#' }
playlist_add_phenotype <- function(name, playlist, conn = NULL) {
    assertthat::assert_that(is.character(name))
    assertthat::assert_that(class(playlist) == "playlist")
    assertthat::assert_that(class(attr(playlist, which = "conn")) == "platform_connection")

    if (!missing(conn)) {
        deprecated_argument_msg(conn) %>%
            warning()
    }

    name <- purrr::map(name, ~base::strsplit(.x, ",", fixed = TRUE)) %>%
            unlist() %>%
            as.list()

    url <- paste(get__link(playlist, "self"), "phenotypes", sep = "/")

    content <- list(name = name)

    resp <- gorr__api_request("POST",
                  url,
                  body = content,
                  conn = attr(playlist, which = "conn"))


    playlist_refresh(playlist)

}

#' Refresh playlist
#'
#' @param playlist structure, create or get it using \code{\link{create_playlist}} or \code{\link{get_playlist}}
#' @param conn gor connection structure, create it using  \code{\link{platform_connect}}
#'
#' @return a list with the playlist object
#' @export
#'
#' @examples
#' \dontrun{
#' api_key <- Sys.getenv("GOR_API_KEY")
#' project <- Sys.getenv("GOR_PROJECT")
#' conn <- platform_connect(api_key, project)
#' name <- "testPL"
#' pl <- create_playlist(name, conn)
#' pl <- playlist_refresh(pl, conn)
#' }
playlist_refresh <- function(playlist, conn = NULL) {
    assertthat::assert_that(class(playlist) == "playlist")
    assertthat::assert_that(class(attr(playlist, which = "conn")) == "platform_connection")
    if (!missing(conn)) {
        deprecated_argument_msg(conn) %>%
            warning()
    }

    url <- get__link(playlist, "self")

    resp <- gorr__api_request("GET", url = url, attr(playlist, which = "conn"))

    playlist(resp$playlist, conn = attr(playlist, which = "conn"))
}

#' Delete a playlist from a project.
#'
#' @param playlist playlist structure, create or get it using \code{\link{create_playlist}} or \code{\link{get_playlist}}
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
#' playlist <- get_playlist(conn, id=1)
#' playlist_delete(playlist, conn)
#' }
playlist_delete <- function(playlist, conn = NULL) {
    assertthat::assert_that(class(playlist) == "playlist")
    if (!missing(conn)) {
        deprecated_argument_msg(conn) %>%
            warning()
    }

    url <- get__link(playlist, "self")

    gorr__api_request("DELETE",
                      url = url,
                      conn = attr(playlist, which = "conn"),
                      parse.body = F)
}
