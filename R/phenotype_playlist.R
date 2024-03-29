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
PhenotypePlaylist <- function(data, conn) {
    # Set playlist$phenotypes to be phenotype objects
    data$phenotypes <- data$phenotypes %>%
        purrr::set_names(purrr::map_chr(.,~.x$name)) %>%
        purrr::map(Phenotype, conn = conn)

    structure(data, class = "playlist", conn = conn)
}

# print.playlist <- function(x, ...) {
#     bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
#     cli::cat_rule(left = ("Playlist"))
#
#     bullet("$name: ", x$name)
#     bullet("$id: ", x$id)
#     bullet("$description: ", x$description)
#     bullet("$phenotypes: ", paste(names(x$phenotypes), collapse= ", "))
#     bullet("$created_by: ", x$created_by)
# }

#' @export
print.playlist <- function(x, ...) {
    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    item_name <- function(x) paste0("$", x, ": ")
    cli::cat_rule(left = ("Playlist"))
    bullet("$name: ", x$name)
    bullet("$phenotypes: ", paste(names(x$phenotypes), collapse= ", "))

    excl <- c("name", "links", "phenotypes")
    conv <- c("tag_list")
    x <- x[(!(names(x)  %in% excl))]
    purrr::modify_at(x, conv, list) %>%
        purrr::walk2(., names(x), ~bullet(item_name(.y), .x))
}


#' Add phenotype to a playlist.
#'
#' For adding multiple phenotypes to playlist see: or \code{\link{playlist_add_phenotypes}}
#'
#' @param name name of phenotype existing in project
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
    assertthat::assert_that(is.string(name))
    assertthat::assert_that(class(playlist) == "playlist")
    assertthat::assert_that(class(attr(playlist, which = "conn")) == "platform_connection")

    if (!missing(conn)) {
        deprecated_argument_msg(conn) %>%
            warning()
    }

    url <- paste(get__link(playlist, "self"), "phenotypes", sep = "/")

    content <- list(name = name)

    resp <- gorr__api_request("POST",
                  url,
                  body = content,
                  conn = attr(playlist, which = "conn"))


    playlist_refresh(playlist)
}

#' Update playlist phenotypes
#'
#' @param names comma seperated string of phenotypes existing in project to add eg. "pheno1,pheno2" or character vector c("pheno1", "pheno2")
#' @param playlist playlist structure, create or get it using \code{\link{create_playlist}} or \code{\link{get_playlist}}
#' @param remove whether to delete the the phenotypes
#'
#' @return A playlist object
playlist_update_phenotypes <- function(names, playlist, remove) {
    url <- get__link(playlist, "self")

    phenotypes <- purrr::map(names, ~base::strsplit(.x, ",", fixed = TRUE)) %>%
            unlist() %>%
            purrr::map(~list(name = .x, `_destroy` = remove))

    content <- list(playlist = list(phenotypes = phenotypes))

    resp <- gorr__api_request("PATCH",
                  url,
                  body = content,
                  conn = attr(playlist, which = "conn"))

    playlist_refresh(playlist)
}


#' Add phenotype/s to playlist
#'
#' @param names comma seperated string of phenotypes existing in project to add eg. "pheno1,pheno2" or character vector c("pheno1", "pheno2")
#' @param playlist playlist structure, create or get it using \code{\link{create_playlist}} or \code{\link{get_playlist}}
#'
#' @return A playlist object
#' @export
playlist_add_phenotypes <- function(names, playlist) {
    assertthat::assert_that(is.character(names))
    assertthat::assert_that(class(playlist) == "playlist")
    assertthat::assert_that(class(attr(playlist, which = "conn")) == "platform_connection")

    playlist_update_phenotypes(names = names, playlist = playlist, remove = FALSE)
}

#' Delete phenotype/s from playlist
#'
#' @param names comma seperated string of phenotypes existing in project to delete eg. "pheno1,pheno2" or character vector c("pheno1", "pheno2")
#' @param playlist playlist structure, create or get it using \code{\link{create_playlist}} or \code{\link{get_playlist}}
#'
#' @return A playlist object
#' @export
playlist_delete_phenotypes <- function(names, playlist) {
    assertthat::assert_that(is.character(names))
    assertthat::assert_that(class(playlist) == "playlist")
    assertthat::assert_that(class(attr(playlist, which = "conn")) == "platform_connection")

    playlist_update_phenotypes(names = names, playlist = playlist, remove = TRUE)
}

#' Update playlist's description
#'
#' @param playlist playlist structure, create or get it using \code{\link{create_playlist}} or \code{\link{get_playlist}}
#' @param description playlist description
#'
#' @return A playlist object
#' @export
playlist_update_description <- function(description, playlist) {
    assertthat::assert_that(is.string(description))
    assertthat::assert_that(class(playlist) == "playlist")
    assertthat::assert_that(class(attr(playlist, which = "conn")) == "platform_connection")

    url <- get__link(playlist, "self")

    content <- list(playlist = list(description = description))

    resp <- gorr__api_request("PATCH",
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

    PhenotypePlaylist(resp$playlist, conn = attr(playlist, which = "conn"))
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
