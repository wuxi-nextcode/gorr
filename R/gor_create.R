
#' gor_create
#'
#' Creates a partial-application of \code{\link{gor_query}} in the context of `conn` and the listed relations in the `...` arguments
#'
#' Relations can be either local dataframes (uploaded) or string queries(turned into create statements and prepended to the query when executed).
#'
#' @param ... ellipse for the relations to include
#' @param conn connection object from \code{\link{gor_connect}}
#'
#' @return partial-application of function \code{\link{gor_query}} with the `conn` and `relations` parameters set.
#' @export
#'
#' @examples
#' \dontrun{
#'   query <- gor_create(air = airquality, conn = conn)
#'   query("nor [air] | where Treatment = 'nonchilled'")
#'
#' }
gor_create <- function(..., conn = NULL) {
  dots <- pryr::named_dots(...)
  relations <- lapply(dots, eval, parent.frame())

  # Pick out virtual relations
  virtual_relations <- purrr::keep(relations, is.data.frame)
  # Pick out create statements
  create_statements <- relations %>%
    purrr::keep(is.character) %>%
    purrr::imap(~ stringr::str_glue("create {.y} = {.x};")) %>%
    paste(collapse = "\n")

  if (interactive()) {

  }

  function(query) {
      query <- paste(create_statements, query, sep = "\n")
      gor_query(query, conn = conn, relations = virtual_relations)
  }
}
