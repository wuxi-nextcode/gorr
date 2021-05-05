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
