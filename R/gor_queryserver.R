
#' Get query from queryserver api. This is not a public function, but is called from \code{\link{gor_query}}
#'
#' @param query gor query string
#' @param conn connection object, see \code{\link{platform_connect}}
#' @param parse should output be parsed into a data.frame
#' @param relations list of tables to upload and make available in the query
#' @param spinner spinner function (e.g. some ascii animation for long running queries)
#' @param ... additional arguments to be passed to \code{\link{gorr__post_query}}
#'
#' @return query results on text format
gorr__queryserver <- function(query, conn, parse, relations, spinner = invisible, ...) {
    start_time <- lubridate::now()

    tryCatch({
        spinner("Submitting and Running Query\n")
        response <- gorr__post_query(query = query,
                                           conn = conn,
                                           relations = relations,
                                           parse.body = F,
                                           ...)
        spinner(gorr__elapsed_time(lubridate::now() - start_time,
                                    status = "Preparing Results",
                                    info = ""))
    }, interrupt = function(x) gorr__failure("Query interrupted by user"),
    error = function(err) stop(err)
    )

    result_txt <- readBin(response$content, character())

    # Get last msg from string
    msg_line <- stringi::stri_extract_last_regex(result_txt,"#>.*?\\n")

    # If last message is an exception message - terminate execution
    if (startsWith(msg_line, "#> EXCEPTION")) {
        gorr__throw_error_from_line(msg_line)
    } else if (!startsWith(msg_line, "#> DONE")) {
        gorr__failure("Incomplete response from server - missing 'DONE' message")
    }

    ### Process result data
    # Remove last message from stream
    result_txt <- stringi::stri_replace_last(result_txt, replacement="",fixed=msg_line)

    # Process result stats
    query_stats <- get__query_stats(msg_line)
    spinner(gorr__elapsed_time(lubridate::now() - start_time,
                                   status = "DONE",
                                   info = query_stats))

    result_txt
}

gorr__get_line_info <- purrr::compose(~ purrr::pluck(.x, 3),
                                      purrr::partial(stringr::str_match, pattern = "#>\\s?([\\w]+)\\s(.*)"))

get__query_stats <- function(line) {
    line_info <- gorr__get_line_info(line)
    query_stats <- jsonlite::fromJSON(gorr__get_line_info(line))
    info <- paste0(" ", " \n Result details: ",
                   query_stats$lineCount, " rows, total size: ",
                  fs::fs_bytes(query_stats$bytesCount), "bytes \n")
    info
}

# Process error messages from stream
gorr__throw_error_from_line <- function(line) {
    line_info <- gorr__get_line_info(line)
    gorr__failure("FAILED", detail = gorr__get_exception_details(line_info))
}

gorr__get_exception_details <- purrr::compose(~ purrr::pluck(.x, 1, 1),
                                      purrr::partial(stringr::str_split, pattern = "Stack Trace"),
                                      ~ purrr::pluck(.x, "gorMessage"),
                                      jsonlite::fromJSON)
