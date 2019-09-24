
#' gor_create
#'
#' Creates a partial-application of \code{\link{gor_query}} in the context of `conn` and the listed relations in the `...` arguments
#'
#' Relations can be either local dataframes (uploaded) or string queries(turned into create statements and prepended to the query when executed).
#'
#' @param ... ellipse for the relations to include
#' @param defs definitions string
#' @param conn connection object from \code{\link{gor_connect}}
#'
#' @return partial-application of function \code{\link{gor_query}} with the `conn` and `relations` parameters set.
#'
#' @examples
#' \dontrun{
#'   query <- gor_create(air = airquality, conn = conn)
#'   query("nor [air] | where Treatment = 'nonchilled'")
#'
#' }
#' @export
gor_create <- function(..., defs = "", conn = NULL, replace = NULL) {
  dots <- rlang::dots_list(...)
  if (!is.null(replace)) {
    if (class(replace) != "gor_creation") {
        stop("replace parameter must be of class gor_creation, see gor_create", call. = F)
    }

    prev <- attributes(replace)
    if (is.null(conn)) {
      conn <- prev$conn
    }

    if (defs == "") {
      defs <- prev$defs
    }

    prev_names <- purrr::discard(names(prev$dots), ~ . %in% names(dots))

    dots <- c(dots, prev$dots[prev_names])

  }

  dots <- purrr::discard(dots, is.null)

  fn <- function(query) {
      # evaluating the dots inside this function is necessary, otherwise virtual relations are fixed.
      relations <- lapply(dots, eval, parent.frame())
      # Pick out virtual relations
      virtual_relations <- purrr::keep(relations, is.data.frame)
      # Pick out create statements
      create_statements <- relations %>%
        purrr::keep(is.character) %>%
        purrr::imap(~ stringr::str_glue("create {.y} = {.x};")) %>%
        paste(collapse = "\n")


      query <- paste(defs, ";", create_statements, query, sep = "\n")
      gor_query(query, conn = conn, relations = virtual_relations)
  }

  structure(fn, class = "gor_creation", defs = defs, dots = dots, conn = conn)
}

#' @export
print.gor_creation <- function(x, ...) {
    x <- attributes(x)
    if (x$defs == "") x$defs <- "None"
    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    cli::cat_rule(left = ("GOR Creation Query"))

    cli::cat_line(crayon::green("Connection"))
    bullet(crayon::blue("Service Root: "), x$conn$service_root)
    bullet(crayon::blue("Project: "), x$conn$project)

    cli::cat_line(crayon::green("Definitions"))
    x$defs %>%
        stringr::str_trim() %>%
        stringr::str_split("\n", simplify = T) %>%
        purrr::map_chr(stringr::str_trim) %>%
        paste0(" ", .) %>%
        purrr::map_chr(crayon::italic) %>%
        purrr::walk(cli::cat_line)

    cli::cat_line(crayon::green("Create statements & virtual relations"))
    purrr::iwalk(x$dots, function(code, name) {
        cli::cat_line(" ", crayon::bold(name))
        cli::cat_line("   ", crayon::italic(as.character(code)))
    })
}

