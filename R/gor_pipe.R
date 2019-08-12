
#' get_gorpipe
#'
#' Generator function for gorpipe. Given a full path to the gorpipe executable, return a function that wraps gorpipe calls,
#' returning a data.frame of the output data
#'
#' @param path full path to gorpipe, e.g. ~/gor-scripts/bin/gorpipe
#'
#' @return function(code) -> data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' gorpipe <- get_gorpipe("~/gor-scripts/bin/gorpipe")
#' gorpipe("norrows 21 | calc even mod(rownum, 2) | calc even_txt if(even==1,'yes','no')")
#' }
get_gorpipe <- function(path) {
    requireNamespace("processx", quietly = TRUE)
    requireNamespace("fs", quietly = TRUE)
    requireNamespace("tibble", quietly = TRUE)

    path <- fs::path_expand(path)
    extract_error <- purrr::partial(
        stringr::str_replace,
        pattern = stringr::regex("(.*)Header:.*", multiline = T, dotall = T),
        replacement = "\\1" )

    check_error <- function(proc_res, errorfun, ...) {
        if (proc_res$status != 0) {
            errorfun(proc_res$stderr)
        }
    }

    fix_gor_colnames <- function(df) {
        colnames(df) <- stringr::str_replace(colnames(df), "#", "")
        df
    }

    gor_version <-
        processx::run(path, "norrows 1 | calc ver GORVERSION() | hide 1", error_on_status = F) %T>%
        check_error(function(stderr) stop("Could not get gorpipe from ", path, "\nerror: ", stderr, call. = F) ) %>%
        purrr::pluck("stdout") %>%
        stringr::str_replace(stringr::regex("#ver\n(.*)\n", multiline = T, dotall = T), "\\1")

    message(crayon::blue("Using gorpipe version ", crayon::bold(gor_version)))

    function(code, parse = T) {
        results <-
            processx::run(path, code, spinner = T, error_on_status = F ) %T>%
            check_error(function(stderr) {
                stop("\n",cli::boxx(paste0("Error in ", crayon::bold("gorpipe")), col = "red"), "\n",
                     crayon::italic(crayon::red(extract_error(stderr))), call. = F)
            }) %>%
            purrr::pluck("stdout")

        if (parse) {
            results %>%
                data.table::fread(sep = "\t", header = T) %>%
                tibble::as_tibble() %>%
                fix_gor_colnames()
        } else {
            results
        }
    }
}
