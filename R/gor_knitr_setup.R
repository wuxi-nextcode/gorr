#' GOR Knitr Setup
#'
#' Sets up 'gor' as a supported engine for knitr
#'
#' @return none
#' @export
#'
#' @examples
#' \dontrun{
#' gor_knitr_setup()
#' }
gor_knitr_setup <- function() {
    requireNamespace("knitr")

    eng_gor <- function(options) {
        code <- paste(options$code, collapse = "\n")
        output <- gor_query(code, options$connection)
        if (!is.null(options$output.var))
            assign(options$output.var, output, envir = knitr::knit_global())

        knitr::engine_output(options, code, output)

    }

    knit_engines$set(gor = eng_gor)
    invisible(NULL)
}
