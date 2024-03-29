
#' gor_create
#'
#' Creates a partial-application of \code{\link{gor_query}} in the context of `conn` and the listed relations in the `...` arguments
#'
#' Relations can be either local dataframes (uploaded) or string queries(turned into create statements and prepended to the query when executed).
#'
#' @param ... ellipse for the relations to include
#' @param defs definitions string
#' @param conn connection object from \code{\link{platform_connect}}
#' @param replace replace a previously created \code{gor_create} closure. When supplied, the two will be merged, overwriting existing values with the current values if they have been previously defined
#' @param query.service query service to use - either 'queryservice' (old) or 'queryserver' (new). Default: queryservice
#'
#' @return partial-application of function \code{\link{gor_query}} with the `conn` and `relations` parameters set.
#'
#' @importFrom methods is
#' @examples
#' \dontrun{
#' query <- gor_create(conn = conn)
#' query <- gor_create(air = airquality, replace = query)
#' query("nor [air] | where Month=8")
#' months_df <- data.frame(id = 1:12, MonthName = month.abb)
#' query <- gor_create(months = months_df, replace = query)
#' query("nor [air] | map -c Month [months]")
#' }
#' @export
gor_create <- function(..., defs = NULL, conn = NULL, replace = NULL, query.service = "queryserver") {
    defs <- gor_define(defs)

    dots <- rlang::dots_list(...)
    if (!is.null(replace)) {
        if (!is(replace, "gor_creation")) {
            gorr__failure("replace parameter must be of class gor_creation, see gor_create")
        }

        prev <- attributes(replace)
        if (is.null(conn)) {
            if (is.null(prev$conn)) gorr__failure("conn parameter must be supplied")
            conn <- prev$conn
        }
        query.service <- prev$query.service

        defs <- merge(prev$defs, defs)
        dots <- merge(prev$dots, dots)
    }

    # make sure that conn is set at this point
    if (is.null(conn))
        gorr__failure("conn parameter must be provided or provided as a part of replace parameter")

    dots <- purrr::discard(dots, is.null)

    fn <- function(query, render_only = F, ...) {
        # evaluating the dots inside this function is necessary, otherwise virtual relations are fixed.
        relations <- lapply(dots, eval, parent.frame())
        # Pick out virtual relations
        virtual_relations <- purrr::keep(relations, is.data.frame)
        # Pick out create statements
        create_statements <- relations %>%
            purrr::keep(is.character) %>%
            purrr::imap(~ stringr::str_glue("create {.y} = {.x};")) %>%
            paste(collapse = "\n")

        def_statements <- defs %>%
            purrr::imap(~ stringr::str_glue("def {.y} = {.x};")) %>%
            paste(collapse = "\n")

        query <- c(def_statements, create_statements, query) %>%
            purrr::discard(function(x) is.null(x) || x == "") %>%
            paste(collapse = "\n")

        if (render_only)
            return(query)

        gor_query(query, conn = conn, relations = virtual_relations, query.service = query.service, ...)
    }

    structure(fn, class = "gor_creation", defs = defs, dots = dots, conn = conn, query.service=query.service)
}

#' @export
print.gor_creation <- function(x, ...) {
    x <- attributes(x)

    bullet <- purrr::partial(cli::cat_bullet, bullet = " ")
    cli::cat_rule(left = ("GOR Creation Query"))

    cli::cat_line(crayon::green("Connection"))
    bullet(crayon::blue("Service Root: "), x$conn[["gor-query-api"]]$service_root)
    bullet(crayon::blue("Project: "), x$conn$project)

    cli::cat_line(crayon::green("Definitions"))
    if (length(x$defs) == 0) {
        cli::cat_line("  None")
    } else {
        purrr::iwalk(x$defs, function(code, name) {
            stringr::str_c("  def ", crayon::bold(name), " = ", code, ";") %>%
            cli::cat_line()
        })
    }

    cli::cat_line(crayon::green("Create statements & virtual relations"))
    if (length(x$dots) == 0) {
        cli::cat_line("  None")
    } else {
        purrr::iwalk(x$dots, function(code, name) {
            cli::cat_line(" ", crayon::bold(name))

            if (is.data.frame(code)) {
                preview <- utils::capture.output(print(tibble::as_tibble(code)))
                cli::cat_line("   ", preview)
            } else {
                cli::cat_line("   ", crayon::italic(code))
            }
        })
    }
}

