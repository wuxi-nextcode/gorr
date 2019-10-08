#' GOR Define
#'
#' Takes a parameter and parses it into a `gor_definitions` structure, which is basically a named list
#' of define statements, i.e. `def name = code;`
#' @param x can be a string, `gor_definitions` structure, list, or NULL
#' @examples
#' # below are just examples of input, not necessarily valid gor `def` code
#' gor_define("
#'    def #var1# = 10;
#'    def fun($1, $2) = gor $1
#'        | some
#'        | pipe
#'        | step $2;
#' ")
#'
#' gor_define(list(`#var1#` = 10, limit = 20, block = "| some | pipe | step"))
#' @export
gor_define <- function(x) {
    UseMethod("gor_define", x)
}

#' @export
gor_define.character <- function(x) {
    # First turn the string x into a list of strings by splitting it down into
    # blocks and cleaning each block
    defines <- x %>%
        stringr::str_replace_all("(def .* = )", ";\\1") %>% # make sure to include a ; before all defs
        stringr::str_split(pattern = ";", simplify = T) %>% # split string by ;
        purrr::map_chr(stringr::str_squish) %>% # remove whitespace left, right, and within
        purrr::discard(`==`, "") # drop empty ones, e.g. produced by str_split for repeated ;;;

    # Take the list and extract name and code from it, then return a named list
    named_list <-
        data.frame(string = defines) %>% # create a table out of the above with 1 column
        tidyr::extract(string, into = c("name", "code"), regex = "^def ([^=]+) = (.*)", remove = F) %>% # extract name and code from each blcok
        dplyr::select(name, code) %>%
        tibble::deframe() %>%
        as.list()

    gor_define(named_list)
}

#' @export
gor_define.list <- function(x) {
    # make sure the list is named
    assertthat::are_equal(length(names(x)), length(x))

    # make sure all of the definitions are a string
    if (length(x) > 0 && !all(purrr::map_lgl(x, is.character)) ) {
        warning("all definitions must be a string, coercing to string")

        x <- purrr::map_chr(x, as.character)
    }

    structure(x, class = c("gor_definitions", "list"))
}

gor_define.NULL <- function(x) {
    gor_define(list())
}

#' @export
gor_define.gor_definitions <- identity

#' @export
merge.list <- function(x, y, ...) {
    if (is.null(y)) return(x)
    if (is.null(x)) return(y)

    keep_x_names <- setdiff(names(x), names(y))

    names_order <- union(names(x), names(y))

    merged <- c(x[keep_x_names], y)
    purrr::discard(merged[names_order], is.null)
}
